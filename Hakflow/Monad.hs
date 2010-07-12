{-# LANGUAGE
  GeneralizedNewtypeDeriving,
  PackageImports
  #-}

module Hakflow.Monad where

import Hakflow.Makeflow

import "mtl" Control.Monad.RWS
import qualified Data.Sequence as Seq


type Log = Seq.Seq String

newtype Hak a = Hak {
      runHak :: RWST Opts Log Makeflow IO a
    } deriving (Functor, Monad, MonadWriter Log, MonadReader Opts, MonadState Makeflow, MonadIO)


