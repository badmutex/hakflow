{-# LANGUAGE
  NoImplicitPrelude
  #-}

module Hakflow.Instances.Vector where

import Prelude.Plus

import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.DeepSeq

instance Foldable Vector where
    foldr = V.foldr
    foldr1 = V.foldr1
    foldl = V.foldl
    foldl1 = V.foldl1

instance Functor Vector where
    fmap = V.map

instance Monad Vector where
    return = V.singleton
    (>>=) = flip V.concatMap

instance Traversable Vector where
    traverse f = V.foldr cons_f (pure V.empty)
        where cons_f x ys = V.cons <$> f x <*> ys

instance Applicative Vector where
    pure = V.singleton
    (<*>) = ap


instance Alternative Vector where
    empty = V.empty
    (<|>) = (V.++)

instance NFData a => NFData (Vector a) where
    rnf v = V.map rnf v `deepseq` ()
