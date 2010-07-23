{-# LANGUAGE
  MultiParamTypeClasses
  #-}

module Hakflow.Util where

import qualified Data.Text as T


class Functor f => Magma f a where magma :: a -> a -> f a


newtype Tagged v t = Tag {unTag :: v}

newtype File = File {path :: FilePath} deriving (Eq, Ord, Show)

