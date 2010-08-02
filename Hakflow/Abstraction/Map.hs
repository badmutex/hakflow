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
import Hakflow.Commands.Cat
import Hakflow.Commands.Rm


import Data.Default
import Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude.Plus hiding (group, map)
import qualified Prelude.Plus as P (map)
import Data.Maybe


chunk :: Foldable f => Int -> f Rule -> Vector (Vector Rule)
chunk limit xs = foldl' pick empty xs
    where
      pick rs r
          | V.length rs == 0             = pure . pure $ r
          | V.length (V.last rs) < limit = V.init rs <|> pure (V.last rs `V.snoc` r)
          | otherwise                    = rs `V.snoc` pure r


data MapCfg = Map { chunksize :: Int
                  , groupsize :: Int}

instance Default MapCfg where def = Map {chunksize = 1, groupsize = 16}

map :: Traversable t => MapCfg -> Command -> t Parameter -> Hak Flow
map cfg c ps = do
  rules <- mapM (addRule c) ps
  let chunks = chunk (chunksize cfg) rules
  mapM mcat chunks


clean :: Vector Rule -> Hak Rule
clean rs = do
  let mains = V.map (param . FileInArg . fromJust) . V.filter isJust $ V.map mainOut rs
  r <- eval =<< cat def mains
  rm def mains
  return r



mapA cfg c ps = do
  rules <- mapM (addRule c) ps
  let chunks = chunk (chunksize cfg) rules
  rules' <- mapM mcat chunks
  let chunks' = chunk (groupsize cfg) rules'
  rules'' <- mapM clean chunks'
  return $ rules' V.++ rules''



addRule :: Command -> Parameter -> Hak Rule
addRule c p = eval c {params = params c `V.snoc` p}
