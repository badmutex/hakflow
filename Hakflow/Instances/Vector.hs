{-# LANGUAGE
  NoImplicitPrelude
  #-}

module Hakflow.Instances.Vector where

import Prelude.Plus

import Data.Vector (Vector)
import qualified Data.Vector as V

instance Foldable Vector where
    foldr = V.foldr
    foldr1 = V.foldr1
    foldl = V.foldl
    foldl1 = V.foldl1