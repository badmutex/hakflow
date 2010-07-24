{-# LANGUAGE
  NoImplicitPrelude,
  NoMonomorphismRestriction
  #-}

module Hakflow.Abstraction where

import Hakflow.Makeflow
import Hakflow.Monad
import Hakflow.Util
import Hakflow.Magma
import Hakflow.Instances.Vector

import Data.Default
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Set (Set)
import qualified Data.Set as S
import Data.Default

import Prelude.Plus

chunkBy :: Int -> [a] -> [[a]]
chunkBy s xs = undefined

chunk :: Int -> [Rule] -> [[Rule]]
chunk limit xs = foldl' pick empty xs
    where
      pick rs r
          | length rs == 0           = [[r]]
          | length (last rs) < limit = init rs ++ [last rs ++ [r]]
          | otherwise                = rs ++ [[r]]



data MapCfg = Map {chunksize :: Int}

instance Default MapCfg where def = Map {chunksize = 1}

mapA :: MapCfg -> Command -> [Parameter] -> Hak Flow
mapA cfg c ps = do
  rules <- mapM (addRule c) ps
  let chunks = chunk (chunksize cfg) rules
  V.fromList <$> mapM mcat chunks


addRule :: Command -> Parameter -> Hak Rule
addRule c p = eval c {params = params c ++ [p]}



testMap s = let prog = do let c1 = Cmd { exec = executable "/bin/echo"
                                       , params = []
                                       , depends = S.empty
                                       , redirection = Nothing
                                       }
                          mapA def {chunksize = s} c1 (map (Param . TextArg . T.pack . (++) "test" . show)  [1..20])
          in do (r,_,_) <- run prog def {_counterDigits=2} def
                T.writeFile "/tmp/testMF/Makeflow" $ V.foldl' (\t1 t2 -> t1 `T.append` T.pack "\n" `T.append` t2) T.empty $ V.map emerge r
