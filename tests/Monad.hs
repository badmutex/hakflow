
import Hakflow.Monad

import Hakflow.Makeflow
import Hakflow.Util

import Data.Text (pack)
import Data.Set (Set)
import qualified Data.Set as S
import Prelude.Plus
import qualified Data.Vector as V
import Data.Default


cmd = Cmd
        (executable "/bin/echo")
        (V.fromList [ Param (TextArg (pack "hello"))
                    , Param (FileInArg (File "world"))
                    , Param (FileOutArg (File "universe"))])
        S.empty
        Nothing


h = let mkr = eval $ shell "echo hello world"
    in do (r,_,_) <- run mkr def def
          return r
