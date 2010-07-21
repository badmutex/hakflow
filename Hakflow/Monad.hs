{-# LANGUAGE
  FlexibleInstances,
  GeneralizedNewtypeDeriving,
  MultiParamTypeClasses,
  NoMonomorphismRestriction,
  PackageImports,
  TemplateHaskell,
  TypeOperators,
  TypeSynonymInstances
  #-}

module Hakflow.Monad where

import Hakflow.Makeflow

import Data.Maybe
import System.Directory
import "monads-fd" Control.Monad.RWS.Strict as RWS
import qualified Data.Vector as V
import System.Random.Mersenne (MTGen)
import qualified System.Random.Mersenne as Rand
import Data.Record.Label as L
import Control.Applicative ((<$>))
import Data.Default
import Data.Word
import System.IO.Unsafe



newtype Log = Log {unLog :: V.Vector String} deriving Show

instance Monoid Log where
    mempty = Log V.empty
    mappend l1 l2 = Log $ unLog l1 V.++ unLog l2


newtype Hak a = Hak {
      runHak :: RWST Opts Log HakState IO a
    } deriving (Functor, Monad, MonadWriter Log, MonadReader Opts, MonadState HakState, MonadIO)

run = RWS.runRWST


data HakState = HS {
      _counter :: !Integer
    -- , _randGen :: !MTGen
    , _workflow :: !Makeflow
    , _resultPrefix :: String
    , _resultSuffix :: String
    }

$(mkLabels [''HakState])
counter :: HakState :-> Integer
-- randGen :: HakState :-> MTGen
workflow :: HakState :-> Makeflow
resultPrefix :: HakState :-> String
resultSuffix :: HakState :-> String


instance Default HakState where
    def = HS {
            _counter = 0
          -- , _randGen = unsafePerformIO $ Rand.newMTGen (Just 42)
          , _workflow = empty
          , _resultPrefix = "result"
          , _resultSuffix = "txt"
          }


-- random :: Rand.MTRandom a => Hak a
-- random = do
--   s  <- RWS.get
--   let g = L.get randGen s
--   r  <- liftIO $ Rand.random g
--   g' <- liftIO $ Rand.getStdGen
--   let s' = L.set randGen g' s
--   put s'
--   return r
-- {-# INLINE random #-}

-- randomIO :: Rand.MTRandom a => Hak a
-- randomIO = do
--   r <- liftIO $ Rand.randomIO
--   g <- liftIO $ Rand.getStdGen
--   put =<< L.set randGen g <$> RWS.get
--   return r
-- {-# INLINE randomIO #-}


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
  return . File $ pref ++ show c ++ "." ++ suff