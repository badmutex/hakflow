{-# LANGUAGE
  EmptyDataDecls,
  FlexibleInstances,
  PackageImports
  #-}

module Hakflow.Commands where

import Hakflow.Makeflow
import Hakflow.Monad

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Default

-- for testing
import "monads-fd" Control.Monad.RWS.Strict
import Data.Text.IO as T


data Cat

fileparams :: Vector File -> Vector (Parameter a)
fileparams = V.map (param . filepath)


data CatCfg = Cat {
      catLocation :: File
    } deriving (Eq, Show)

instance Default CatCfg where def = Cat . File $ "/bin/cat"

cat :: CatCfg -> Vector (Parameter Cat) -> Hak (Command Cat)
cat cfg opts = do
  out <- result
  let exe = catLocation cfg
      ps  = V.toList opts
      r   = Redir Write StdOut out
  return $ Cmd (Exec exe) ps r

instance FilesIn (Command Cat) where
    filesin (Cmd (Exec exe) ps _) = exe : map (File . para . unTag) ps

instance FilesOut (Command Cat) where
    filesout (Cmd _ _ r) = filesout r

cmd = let files = V.fromList $ map param ["foo","bar"]
          prog  = cat def files
      in do r@(c,s,l) <- run prog def {_counterDigits=2} def
            return . emerge . makeflow $ eval c

