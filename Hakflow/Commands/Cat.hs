{-# LANGUAGE
  NoImplicitPrelude
  #-}

module Hakflow.Commands.Cat where

import Hakflow.Makeflow
import Hakflow.Monad
import Hakflow.Util

import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Default
import Prelude.Plus


data CatCfg = Cat {
      catLocation :: File
    } deriving (Eq, Show)

instance Default CatCfg where def = Cat . File $ "/bin/cat"

cat :: CatCfg -> Vector Parameter -> Hak Command
cat cfg opts = do
  out <- result
  let exe = Exe $ catLocation cfg
      ps  = opts
  return $ Cmd { exec = exe
               , params = ps
               , depends = S.empty
               , redirection = Nothing }
{-# INLINE cat #-}


