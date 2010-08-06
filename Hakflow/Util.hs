module Hakflow.Util where

import qualified Data.Text as T
import Prelude.Plus
import Control.DeepSeq


newtype Tagged v t = Tag {unTag :: v}

newtype File = File {path :: FilePath} deriving (Eq, Ord, Read, Show)


instance NFData v => NFData (Tagged v t) where
    rnf t = unTag t `deepseq` ()

instance NFData File where
    rnf f = path f `deepseq` ()