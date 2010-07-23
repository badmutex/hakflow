{-# LANGUAGE
  MultiParamTypeClasses
  #-}

module Hakflow.Util where

import qualified Data.Text as T


newtype Tagged v t = Tag {unTag :: v}

newtype File = File {path :: FilePath} deriving (Eq, Ord, Show)

