{-# LANGUAGE
  FlexibleInstances,
  GeneralizedNewtypeDeriving,
  MultiParamTypeClasses,
  NoImplicitPrelude,
  NoMonomorphismRestriction,
  PackageImports,
  TemplateHaskell,
  TypeOperators,
  TypeSynonymInstances
  #-}

module Hakflow.Monad where

import Hakflow.Makeflow
import Hakflow.Util

import Data.Maybe
import "monads-fd" Control.Monad.RWS.Strict as RWS
import qualified Data.Vector as V
import Data.Text (Text,pack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Random.Mersenne (MTGen)
import qualified System.Random.Mersenne as Rand
import Data.Record.Label as L
import Control.Applicative ((<$>))
import Data.Default
import Data.Word
import System.IO.Unsafe
import Text.Printf
import "monads-fd" Control.Monad.Identity
import Data.Set (Set)
import qualified Data.Set as S
import Prelude.Plus

-- for testing
import Debug.Trace


newtype Log = Log {unLog :: V.Vector String} deriving Show

instance Monoid Log where
    mempty = Log V.empty
    mappend l1 l2 = Log $ unLog l1 V.++ unLog l2


newtype Hak a = Hak {
      runHak :: RWST MakeflowOpts Log HakState IO a
    } deriving (Functor, Monad, MonadWriter Log, MonadReader MakeflowOpts, MonadState HakState, MonadIO)

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
          , _resultPrefix = "result_"
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
  return . File $ pref ++ printf fmt c ++ "." ++ suff



cmd = Cmd
        (executable "/bin/echo")
        [Param (TextArg (pack "hello"))
        , Param (FileInArg (File "world"))
        , Param (FileOutArg (File "universe"))]
        S.empty


instance Eval Hak Command where
    eval cmd = do let ins = (filesin $ exec cmd) `S.union` (filesin $ params cmd) `S.union` depends cmd
                      outs = filesout $ params cmd
                  res <- if isJust (redirection cmd)
                         then return . fromJust . redirection $ cmd
                         else (Redir StdOut Write) <$> result
                  return Rule { outputs = outs
                              , inputs = ins
                              , mainOut = Just $ redirectionFile res
                              , commands = V.singleton cmd {redirection=Just res} }
