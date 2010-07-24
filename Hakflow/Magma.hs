{-# LANGUAGE
  MultiParamTypeClasses,
  NoImplicitPrelude,
  NoMonomorphismRestriction,
  TypeFamilies
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
import Prelude.Plus

-- debugging
import qualified Data.Text as T
import qualified Data.Text.IO as T


class Functor f => Magma f a where
    type Seq :: * -> *
    magma :: a -> a -> f a
    mcat :: Seq a -> f a


instance Magma Hak Rule where
    type Seq = []
    magma = magmaR
    mcat = mcatR


magmaR r1 r2 = mcatR [r1, r2]
{-# INLINE magmaR #-}

mcatR rules = let files f = foldl' (\os r -> os `S.union` f r) S.empty rules
                  outs = files outputs
                  ins  = files inputs
                  mains = map fromJust . filter isJust . map mainOut $ rules
              in do r <- eval =<< cat def (map (Param . FileInArg) mains)
                    return Rule { outputs = outs `S.union` outputs r
                                , inputs = ins
                                , mainOut = mainOut r
                                , commands = (foldl' (V.++) V.empty $ map commands rules) V.++ commands r }
{-# INLINE mcatR #-}



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
