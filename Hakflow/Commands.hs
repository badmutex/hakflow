{-# LANGUAGE
  EmptyDataDecls,
  FlexibleInstances
  #-}

module Hakflow.Commands where

import Hakflow.Makeflow
import Hakflow.Monad

import qualified Data.Vector as V
import Data.Default



data Cat

cat :: V.Vector File -> Hak (Command Cat)
cat files = do
  out <- result
  let exe = "cat"            
      ps  = V.toList $ V.map (param . filepath) files
      r   = Redir Write StdOut out
  return $ Cmd (Exec $ File "cat") ps r

instance FilesIn (Command Cat) where
    filesin (Cmd (Exec exe) ps _) = exe : map (File . para . unTag) ps

instance FilesOut (Command Cat) where
    filesout (Cmd _ _ r) = filesout r

cmd = runHak . cat . V.map File $ V.fromList ["foo"]