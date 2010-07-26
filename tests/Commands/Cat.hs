{-# LANGUAGE
  PackageImports,
  NoImplicitPrelude
  #-}


import Hakflow.Commands.Cat
import Hakflow.Makeflow
import Hakflow.Monad
import Hakflow.Util

import Data.Set (Set)
import qualified Data.Set as S
import Data.Default
import Prelude.Plus

-- for testing
import "monads-fd" Control.Monad.RWS.Strict
import Data.Text.IO as T



cmd' = let files = map (Param . FileOutArg . File) ["foo","bar"]
           prog  = eval =<< cat def files
       in do r@(c,s,l) <- run prog def {_counterDigits=2} def
             T.putStrLn . emerge $ c

