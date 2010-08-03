{-# LANGUAGE
  NoImplicitPrelude
  #-}

module Hakflow.Commands.Rm where

import Hakflow.Makeflow
import Hakflow.Monad
import Hakflow.Util

import qualified Data.Set as S
import Data.Vector (Vector)
import Data.Default
import Prelude.Plus


data RmCfg = Rm {
      rmLocation :: File
    } deriving (Eq, Show)

instance Default RmCfg where def = Rm . File $ "/bin/rm"


rm :: RmCfg -> Vector Parameter -> Hak Command
rm cfg opts =
    return Cmd { exec = Exe $ rmLocation cfg
               , params = opts
               , depends = S.empty
               , redirection = Just Out }
{-# INLINE rm #-}