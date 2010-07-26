{-# LANGUAGE
  NoImplicitPrelude
  #-}

import qualified Hakflow.Abstraction as A
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


testMap s = let prog = do let c1 = Cmd { exec = executable "/bin/echo"
                                       , params = []
                                       , depends = S.empty
                                       , redirection = Nothing
                                       }
                          A.map def {A.chunksize = s} c1 (map (Param . TextArg . T.pack . (++) "test" . show)  [1..10])
          in do (r,_,_) <- run prog def def
                T.putStrLn . emerge $ r
