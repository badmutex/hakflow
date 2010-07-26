{-# LANGUAGE
  NoImplicitPrelude
  #-}

module Hakflow.Abstraction.Map
    ( MapCfg (..)
    , map 
    ) where

import Hakflow.Makeflow
import Hakflow.Monad
import Hakflow.Util
import Hakflow.Magma
import Hakflow.Instances.Vector

import Data.Default
import qualified Data.Vector as V
import Prelude.Plus hiding (map)


chunk :: Int -> [Rule] -> [[Rule]]
chunk limit xs = foldl' pick empty xs
    where
      pick rs r
          | length rs == 0           = [[r]]
          | length (last rs) < limit = init rs ++ [last rs ++ [r]]
          | otherwise                = rs ++ [[r]]



data MapCfg = Map {chunksize :: Int}

instance Default MapCfg where def = Map {chunksize = 1}

map :: MapCfg -> Command -> [Parameter] -> Hak Flow
map cfg c ps = do
  rules <- mapM (addRule c) ps
  let chunks = chunk (chunksize cfg) rules
  V.fromList <$> mapM mcat chunks


addRule :: Command -> Parameter -> Hak Rule
addRule c p = eval c {params = params c ++ [p]}
