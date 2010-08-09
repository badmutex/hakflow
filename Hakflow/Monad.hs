{-# LANGUAGE
  GeneralizedNewtypeDeriving,
  MultiParamTypeClasses,
  NoImplicitPrelude,
  TemplateHaskell,
  TypeOperators,
  TypeSynonymInstances
  #-}

module Hakflow.Monad where

import Hakflow.Makeflow
import Hakflow.Util

import Data.Maybe
import Control.Monad.RWS.Strict (RWST,MonadWriter,MonadReader,MonadState,MonadIO, put, ask)
import qualified Control.Monad.RWS.Strict as RWS
import qualified Data.Vector as V
import Data.Text (Text,pack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Record.Label as L
import Data.Default
import Text.Printf
import qualified Data.Set as S
import Prelude.Plus



newtype Log = Log {unLog :: V.Vector String} deriving Show

instance Monoid Log where
    mempty = Log V.empty
    mappend l1 l2 = Log $ unLog l1 V.++ unLog l2


newtype Hak a = Hak {
      runHak :: RWST MakeflowOpts Log HakState IO a
    } deriving (Applicative, Functor, Monad, MonadWriter Log, MonadReader MakeflowOpts, MonadState HakState, MonadIO)

run = RWS.runRWST . runHak


data HakState = HS {
      _counter :: !Integer
    , _workflow :: !Flow
    , _resultPrefix :: String
    , _resultSuffix :: String
    } deriving Show

$(mkLabels [''HakState])
counter :: HakState :-> Integer
workflow :: HakState :-> Flow
resultPrefix :: HakState :-> String
resultSuffix :: HakState :-> String


instance Default HakState where
    def = HS {
            _counter = 0
          , _workflow = V.empty
          , _resultPrefix = "result"
          , _resultSuffix = "txt"
          }

inc :: Num a => HakState :-> a -> Hak ()
inc = flip change (+1)

dec :: Num a => HakState :-> a -> Hak ()
dec = flip change ((-) 1)

change :: HakState :-> a -> (a -> a) -> Hak ()
change label diff = put =<< L.mod label diff <$> RWS.get

result :: Hak File
result = do
  c    <- L.get counter <$> RWS.get
  pref <- L.get resultPrefix <$> RWS.get
  suff <- L.get resultSuffix <$> RWS.get
  inc counter
  digits <- L.get counterDigits <$> ask
  let fmt = "%0" ++ show digits ++ "X"
  return . File $ pref ++ "." ++ printf fmt c ++ "." ++ suff


saveFlow :: Flow -> Hak ()
saveFlow f = RWS.modify (L.set workflow f)

addFlow :: Flow -> Hak ()
addFlow f = do
  wf <- L.get workflow <$> RWS.get
  let f' = f V.++ wf
  saveFlow f'

withPrefix :: String -> (Hak a -> Hak b) -> Hak a -> Hak b
withPrefix = withState resultPrefix
withPrefix' :: String -> Hak a -> Hak a
withPrefix' = flip withPrefix id

withSuffix :: String -> (Hak a -> Hak b) -> Hak a -> Hak b
withSuffix = withState resultSuffix
withSuffix' :: String -> Hak a -> Hak a
withSuffix' = flip withSuffix id

withState :: HakState :-> a -> a -> (Hak b -> Hak c) -> Hak b -> Hak c
withState name val next action = do
  current <- L.get name <$> RWS.get
  RWS.modify (L.set name val)
  res <- next action
  RWS.modify (L.set name current)
  return res



instance Eval Hak Command where
    eval cmd@(Shell s) = do
      r <- result
      return Rule { outputs = S.empty
                  , inputs = S.empty
                  , mainOut = Just r
                  , commands = V.singleton cmd
                  , local = True }

    eval cmd = do let ins = (filesin $ exec cmd) `S.union` (filesin $ params cmd) `S.union` depends cmd
                      outs = filesout $ params cmd
                  res <- if isJust (redirection cmd)
                         then return . fromJust . redirection $ cmd
                         else (Combine Write) <$> result
                  return Rule { outputs = outs
                              , inputs = ins
                              , mainOut = Just $ redirectionFile res
                              , commands = V.singleton cmd {redirection=Just res}
                              , local = False }


writeFlow :: Flow -> FilePath -> IO ()
writeFlow f p = do
  T.writeFile p (pack "")
  V.mapM_ (T.appendFile p . flip T.snoc '\n' . emerge) f



go :: IO (a, HakState, Log) -> FilePath -> IO ()
go m p = do (_, s, _) <- m
            writeFlow (L.get workflow s) p
