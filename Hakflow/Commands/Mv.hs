{-# LANGUAGE
  NoImplicitPrelude
  #-}

module Hakflow.Commands.Mv where

import Hakflow.Makeflow
import Hakflow.Monad
import Hakflow.Util

import qualified Data.Set as S
import Data.Vector (Vector)
import Data.Default
import Prelude.Plus


data MvCfg = Mv { mvLocation :: File } deriving (Read,Show)


instance Default MvCfg where def = Mv . File $ "/bin/mv"



mv :: MvCfg -> Vector Parameter -> Hak Command
mv cfg opts =
    return Cmd { exec = Exe $ mvLocation cfg
               , params = opts
               , depends = S.empty
               , redirection = Just Out }
{-# INLINE mv #-}