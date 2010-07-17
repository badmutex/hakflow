{-# LANGUAGE
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
import qualified Data.Sequence as Seq


type Log = Seq.Seq String

newtype Hak a = Hak {
      runHak :: RWST Opts Log Command IO a
    } deriving (Functor, Monad, MonadWriter Log, MonadReader Opts, MonadState Command, MonadIO)


