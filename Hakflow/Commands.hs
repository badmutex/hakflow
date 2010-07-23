{-# LANGUAGE
  EmptyDataDecls,
  FlexibleInstances,
  PackageImports
  #-}

module Hakflow.Commands where

import Hakflow.Makeflow
import Hakflow.Monad
import Hakflow.Util

import Data.Set (Set)
import qualified Data.Set as S
import Data.Default

-- for testing
import "monads-fd" Control.Monad.RWS.Strict
import Data.Text.IO as T


data Cat


data CatCfg = Cat {
      catLocation :: File
    } deriving (Eq, Show)

instance Default CatCfg where def = Cat . File $ "/bin/cat"

cat :: CatCfg -> [Parameter] -> Hak Command
cat cfg opts = do
  out <- result
  let exe = Exe $ catLocation cfg
      ps  = opts
      r   = Redir StdOut Write out
  return $ Cmd { exec = exe
               , params = ps
               , depends = S.empty
               , redirect = r }


cmd' = let files = map (Param . FileOutArg . File) ["foo","bar"]
           prog  = eval =<< cat def files
       in do r@(c,s,l) <- run prog def {_counterDigits=2} def
             T.putStrLn . emerge $ c

