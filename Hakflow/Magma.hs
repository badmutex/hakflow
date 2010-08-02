{-# LANGUAGE
  MultiParamTypeClasses,
  NoImplicitPrelude,
  TypeFamilies
  #-}

module Hakflow.Magma (Magma(..)) where

import Hakflow.Monad
import Hakflow.Makeflow
import Hakflow.Commands.Cat
import Hakflow.Commands.Rm


import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe
import Data.Default
import Prelude.Plus


class Functor f => Magma s f a where
    magma :: a -> a -> f a
    mcat :: s a -> f a

instance Magma Vector Hak Rule where
    magma = magmaR
    mcat = mcatR




magmaR r1 r2 = mcatR $ V.fromList [r1, r2]
{-# INLINE magmaR #-}

mcatR :: Vector Rule -> Hak Rule
mcatR rules =
    let files f = foldl' (\os r -> os `S.union` f r) S.empty rules
        outs = files outputs
        ins  = files inputs
        mains = V.map fromJust . V.filter isJust . V.map mainOut $ rules
        onMains f cfg = eval =<< f cfg (V.map (Param . FileInArg) mains)
    in do rCat <- onMains cat def
          rRm  <- onMains rm def
          return Rule { outputs = outs `S.union` outputs rCat
                      , inputs = ins
                      , mainOut = mainOut rCat
                      , commands = (foldl' (V.++) V.empty $ V.map commands rules) V.++ commands rCat V.++ commands rRm }
{-# INLINE mcatR #-}
