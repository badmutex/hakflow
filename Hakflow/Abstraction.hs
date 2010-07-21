{-# LANGUAGE
  FlexibleContexts
  #-}

module Hakflow.Abstraction where

import Hakflow.Makeflow
import Hakflow.Monad

import Data.List (foldl')
import qualified Data.Vector as V
import Data.Default



amap :: Command a -> V.Vector (Parameter a) -> V.Vector (Command a)
amap c ps = V.map (c `addParam`) ps

addParam :: Command a -> Parameter a -> Command a
addParam (Cmd e ps r) p = Cmd e (ps ++ [p]) r


chunkBy :: V.Vector Rule -> Int -> V.Vector Rule
chunkBy rules size = V.map (V.foldl1' magma) $ V.foldl' pick V.empty rules
    where
      pick :: V.Vector (V.Vector Rule) -> Rule -> V.Vector (V.Vector Rule)
      pick rs r
          | V.length rs == 0 = V.singleton $ V.singleton r
          | V.length (V.last rs) < size = V.init rs V.++ (V.singleton (V.last rs V.++ V.singleton r))
          | otherwise               = rs V.++ V.singleton (V.singleton r)

v = V.fromList [r1,r2]



data MapOpts = Map {
      chunksize :: Int
    }

instance Default MapOpts where
    def = Map {
            chunksize = 1
          }



mapA :: FilesOut (Command a) => Command a -> V.Vector (Parameter a) -> MapOpts -> Hak (V.Vector File)
mapA c ps opts = do
    out <- output c
    let c' = redirect c (Redir Write StdOut out)
        m = amap c' ps
    return undefined


output :: FilesOut (Command a) => Command a -> Hak File
output c = if (null $ filesout c) || Out == redirection c
           then result
           else error "Manual redirection of commands not supported yet"