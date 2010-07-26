
module Hakflow.Monad where

import Hakflow.Makeflow
import Hakflow.Util

import Data.Text (pack)
import Data.Set (Set)
import qualified Data.Set as S
import Prelude.Plus


cmd = Cmd
        (executable "/bin/echo")
        [Param (TextArg (pack "hello"))
        , Param (FileInArg (File "world"))
        , Param (FileOutArg (File "universe"))]
        S.empty
        Nothing