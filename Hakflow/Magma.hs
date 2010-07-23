{-# LANGUAGE
  MultiParamTypeClasses,
  NoMonomorphismRestriction
  #-}

module Hakflow.Magma where

import Hakflow.Monad
import Hakflow.Makeflow
import Hakflow.Commands.Cat
import Hakflow.Util


import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Maybe
import Data.Default
import Data.Foldable
import Prelude hiding (foldl,foldl1,foldr,foldr1,mapM_)

-- debugging
import qualified Data.Text as T
import qualified Data.Text.IO as T


class Functor f => Magma f a where magma :: a -> a -> f a



instance Magma Hak Rule where magma = magmaR

magmaR r1 r2 = let outs = outputs r1 `S.union` outputs r2
                   ins  = inputs r1 `S.union` inputs r2
                   mains = map fromJust . filter isJust . map mainOut $ [r1,r2]
               in do r3 <- eval =<< cat def (map (Param . FileInArg) mains)
                     return Rule { outputs = outs `S.union` outputs r3
                                 , inputs = ins
                                 , mainOut = mainOut r3
                                 , commands = commands r1 V.++ commands r2 V.++ commands r3
                                 }

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
       in progs
