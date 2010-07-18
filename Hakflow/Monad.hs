{-# LANGUAGE
  FlexibleInstances,
  GeneralizedNewtypeDeriving,
  MultiParamTypeClasses,
  PackageImports,
  TypeSynonymInstances
  #-}

module Hakflow.Monad where

import Hakflow.Makeflow

import Data.Maybe
import System.Directory
import "mtl" Control.Monad.RWS.Strict
import qualified Data.Vector as V
import System.Random.Mersenne


newtype Log = Log {unLog :: V.Vector String} deriving Show

instance Monoid Log where
    mempty = Log V.empty
    mappend l1 l2 = Log $ unLog l1 V.++ unLog l2


newtype Hak a = Hak {
      runHak :: RWST Opts Log HakState IO a
    } deriving (Functor, Monad, MonadWriter Log, MonadReader Opts, MonadState HakState, MonadIO)


data HakState = HS {
      counter :: !Integer
    , randGen :: !MTGen
    , workflow :: !Makeflow
    }
