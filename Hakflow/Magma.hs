{-# LANGUAGE
  MultiParamTypeClasses,
  NoImplicitPrelude,
  TypeFamilies
  #-}

module Hakflow.Magma (Magma(..)) where

import Hakflow.Monad
import Hakflow.Makeflow
import Hakflow.Commands.Cat


import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Maybe
import Data.Default
import Prelude.Plus


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


