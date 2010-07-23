{-# LANGUAGE
  EmptyDataDecls,
  FlexibleInstances,
  FlexibleContexts,
  GADTs,
  GeneralizedNewtypeDeriving,
  MultiParamTypeClasses,
  TemplateHaskell,
  TypeFamilies,
  TypeOperators,
  TypeSynonymInstances,
  UndecidableInstances,
  NoMonomorphismRestriction
  #-}

module Hakflow.Makeflow where

import Text.Printf
import Data.List (intercalate,intersect,nub)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Record.Label
import Data.Default


newtype Tagged v t = Tag {unTag :: v} deriving (Eq, Show)

retag :: Tagged v t1 -> Tagged v t2
retag = Tag . unTag


class Magma a where
    magma :: a -> a -> a


newtype Input = Input [File] deriving (Eq, Show)
newtype Output = Output [File] deriving (Eq, Show)

instance Magma Input where
    magma (Input i1) (Input i2) = Input (nub $ i1 ++ i2)

instance Magma Output where
    magma (Output o1) (Output o2) = let outs = o1 ++ o2
                                        l1 = length outs
                                        l2 = length $ nub outs
                                    in if l1 == l2
                                       then Output outs
                                       else error $ "Magma Output: cannot join different outputs to the same file(s): " ++
                                                (show $ intersect o1 o2)


data Rule = Rule Output Input [Cmd] deriving (Show)



-- instance Eq Rule where
--     r1@(Rule o1 i1 c1) == r2@(Rule o2 i2 c2) =
--         if o1 == o2 && i1 == i2 && c1 == c2
--         then True
--         else False



instance Magma Rule where
    magma r1@(Rule o1 i1 c1) r2@(Rule o2 i2 c2) = Rule (o1 `magma` o2) (i1 `magma` i2) (c1 `magma` c2)


data Command a = Cmd Executable [Parameter a] Redirection deriving (Eq, Show)

redirection :: Command a -> Redirection
redirection (Cmd _ _ r) = r

redirect :: Command a -> Redirection -> Command a
redirect (Cmd e ps _) r = Cmd e ps r

data Cmd where
    C ::  (Eq (Command a), FilesIn (Command a), FilesOut (Command a)) => Command a -> Cmd


instance Magma [Cmd] where magma = (++)


instance Show Cmd where
    show (C c) = "C (" ++ show c ++ ")"

newtype Executable = Exec File deriving (Eq, Show)

data Param = Para String
           | Flagged String Param
             deriving (Eq, Show)
type Parameter = Tagged Param


para (Para s) = s
param = Tag . Para

flagged s = Tag . Flagged s


newtype File = File {filepath :: FilePath} deriving (Eq, Show)

instance Magma [File] where magma xs ys = nub $ xs ++ ys

data OutBuffer = StdErr
               | StdOut
                 deriving (Eq, Show)

data OutputMode = Write | Append
                deriving (Eq, Show)

data Redirection = Redir OutputMode OutBuffer File
                 | Combine OutputMode File
                 | Split Redirection Redirection
                 | Out
                   deriving (Eq, Show)




class Eval a where
    eval :: a -> Rule

class FilesIn a where
    filesin :: a -> [File]

class FilesOut a where
    filesout :: a -> [File]





instance FilesOut Output where
    filesout (Output fs) = fs

instance FilesIn Input where
    filesin (Input fs) = fs

instance FilesOut Redirection where
    filesout (Redir _ _ f) = [f]
    filesout (Combine _ f) = [f]
    filesout (Split r1 r2) = filesout r1 `magma` filesout r2
    filesout _ = []

instance FilesIn Executable where
    filesin (Exec f) = [f]


instance Eval Rule where
    eval = id

instance (FilesIn (Command a), FilesOut (Command a)) => Eval (Command a) where
        eval cmd@(Cmd exe _ redir) = Rule (Output (filesout cmd `magma` filesout redir)) (Input (filesin cmd)) [C cmd]

instance FilesIn Rule where
    filesin (Rule _ input _) = filesin input

instance FilesOut Rule where
    filesout (Rule out _ _) = filesout out




data A
data B

instance FilesIn (Command A) where filesin = const []
instance FilesOut (Command A) where filesout = const []

instance FilesIn (Command B) where filesin = const []
instance FilesOut (Command B) where filesout = const []


cmd1 :: Command A
cmd1 = Cmd (Exec $ File "/bin/env") [] Out

cmd2 :: Command B
cmd2 = Cmd (Exec $ File "/bin/echo") [param "hello world"] (Redir Write StdOut $ File "/tmp/out")

r1 = eval cmd1
r2 = eval cmd2

r3 = r1 `magma` r2

cmd3 :: Command A
cmd3 = Cmd (Exec $ File "/usr/bin/python") [param "/tmp/hello.py"] (Redir Write StdOut $ File "/tmp/out")








buffer StdOut = T.pack "2"
buffer StdErr = T.pack "1"
{-# INLINE buffer #-}
mode Write = T.pack ">"
mode Append = T.pack ">>"
{-# INLINE mode #-}


type Makeflow = V.Vector Rule

data Opts = Opts {
      _counterDigits :: !Int
    }
$(mkLabels [''Opts])
counterDigits :: Opts :-> Int

instance Default Opts where
    def = Opts { _counterDigits = 10 }

writeMakeflow :: Makeflow -> FilePath -> Opts -> IO ()
writeMakeflow mf p opts = undefined


class MF a where
    makeflow :: a -> Makeflow

instance MF Rule where
    makeflow = V.singleton

instance MF (Vector Rule) where
    makeflow = id

instance Emerge Makeflow where
    emerge = V.foldl' join T.empty . V.map emerge
        where join txt rule = txt `T.append` rule `T.append` T.pack "\n"



class Emerge a where
    emerge :: a -> T.Text

instance Emerge File where
    emerge = T.pack . filepath

emerge_rule :: Rule -> T.Text
emerge_rule (Rule (Output outs) (Input ins) cmds) =
    outfs `T.append` colon `T.append` infs `T.append` newline_tab `T.append` cmd
        where files = T.intercalate (T.pack " ") . map (T.pack . filepath)
              outfs = files outs
              infs  = files ins
              cmd = emerge cmds

              colon = T.pack " : "
              newline_tab = T.pack "\n\t"

instance Emerge Rule where
    emerge = emerge_rule



emerge_command (Cmd exe args redir) =
    emerge exe `T.append` s `T.append` emerge args `T.append` s `T.append` emerge redir
        where s = T.pack " "

instance Emerge (Command a) where
    emerge = emerge_command


instance Emerge Cmd where
    emerge (C c) = emerge c


instance Emerge [Cmd] where
    emerge = T.intercalate (T.pack " ; ") . map emerge



emerge_executable (Exec path) = T.pack $ filepath path

instance Emerge Executable where
    emerge = emerge_executable

emerge_param (Para s) = T.pack s
emerge_param (Flagged s p) = T.pack s `T.append` T.pack " " `T.append` emerge_param p

instance Emerge (Parameter a) where
    emerge = emerge_param . unTag

emerge_parameter_list = T.intercalate (T.pack " ") . map emerge

instance Emerge [Parameter a] where
    emerge = emerge_parameter_list


emerge_redirection (Redir m b p) = buffer b `T.append` mode m `T.append` T.pack " " `T.append` emerge p
emerge_redirection (Combine m p) = mode m `T.append` T.pack " " `T.append` emerge p `T.append` T.pack " 2>&1"
emerge_redirection (Split r1 r2) = emerge_redirection r1 `T.append` T.pack " " `T.append` emerge_redirection r2
emerge_redirection _ = T.empty

instance Emerge Redirection where
    emerge = emerge_redirection



-- data Compilation a b = End a | Step b

-- class Compile a b where
--     type Step :: *
--     compile :: a -> Compilation b Step


-- -- instance Compile String Arg where
-- --     type Step = [Arg]
-- --     compile str = 



empty :: Makeflow
empty = V.empty