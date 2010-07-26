{-# LANGUAGE
  MultiParamTypeClasses,
  NoImplicitPrelude,
  TypeFamilies
  #-}

import Hakflow.Magma
import Hakflow.Monad
import Hakflow.Makeflow
import Hakflow.Commands.Cat
import Hakflow.Util


import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Maybe
import Data.Default
import Prelude.Plus

-- debugging
import qualified Data.Text as T
import qualified Data.Text.IO as T


test = let progs = do
             let c1 = Cmd { exec = executable "foo"
                          , params = map (Param . TextArg . T.pack) ["hello","world"] ++
                                     [Param . FileInArg . File $ "input"] ++
                                     [Param . FileOutArg . File $ "output"]
                          , depends = S.empty
                          , redirection = Nothing
                          }
                 c2 = c1 { exec = executable "bar" }
                 c3 = c1 { exec = executable "bang" }
             rs <- mapM eval [c1,c2,c3]
             r3 <- foldlM magma (head rs) (tail rs)
             return r3
       in do (r,_,_) <- run progs def {_counterDigits=2} def
             T.putStrLn . emerge $ r
