{-# LANGUAGE
  EmptyDataDecls,
  FlexibleInstances,
  GADTs,
  GeneralizedNewtypeDeriving,
  MultiParamTypeClasses,
  TypeFamilies,
  TypeSynonymInstances
  #-}

module Hakflow.Makeflow where

import Text.Printf
import Data.List (intercalate)
import qualified Data.Sequence as Seq
import qualified Data.Text as T


class Magma a where
    magma :: a -> a -> a

instance Magma [a] where
    magma = (++)

data Rule = Rule Output Input [Command] deriving Show

instance Magma Rule where
    magma (Rule o1 i1 c1) (Rule o2 i2 c2) = Rule (magma o1 o2) (magma i1 i2) (magma c1 c2)

newtype Output = Output [File] deriving (Show)
instance Magma Output where
    magma (Output o1) (Output o2) = Output $ magma o1 o2

newtype Input = Input [File] deriving (Show)
instance Magma Input where
    magma (Input i1) (Input i2) = Input $ magma i1 i2

newtype File = File {filepath :: FilePath} deriving Show

data Command = Cmd Executable [Arg] Redirection deriving Show

newtype Executable = Exec File deriving Show

data Arg = Flag String | Arg File deriving Show

-- Phantom type: used to 'tag' parameters for type class instances (ie: FileIn/Out, Compile)
data Parameter a = Param {param :: String}
                 | FileParam {fileParam :: File}
                 | FlaggedParam {flag :: String, fparam :: Parameter a}
                   deriving Show


data OutBuffer = StdErr
               | StdOut
                 deriving Show

data OutputMode = Write | Append
                deriving Show

data Redirection = Redir OutputMode OutBuffer File
                 | Combine OutputMode File
                 | Split Redirection Redirection
                   deriving Show


cmd1 = Cmd (Exec (File "exe1")) [] (Redir Write StdOut (File "out1"))
cmd2 = Cmd (Exec (File "exe2")) [Arg (File "foo")] (Redir Write StdOut (File "out2"))


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
    filesout (Split r1 r2) = filesout r1 ++ filesout r2

instance FilesIn Arg where
    filesin (Flag _) = []
    filesin (Arg f) = [f]

instance FilesIn [Arg] where
    filesin = concatMap filesin

instance FilesIn Executable where
    filesin (Exec f) = [f]

instance FilesIn Command where
    filesin (Cmd exec args _) = filesin exec ++ filesin args

instance FilesOut Command where
    filesout (Cmd _ _ redir) = filesout redir

instance Eval Rule where
    eval = id

instance Eval Command where
    eval cmd = Rule (Output (filesout cmd)) (Input (filesin cmd)) [cmd]

instance FilesIn Rule where
    filesin (Rule _ input _) = filesin input

instance FilesOut Rule where
    filesout (Rule out _ _) = filesout out






buffer StdOut = "2"
buffer StdErr = "1"
{-# INLINE buffer #-}
mode Write = ">"
mode Append = ">>"
{-# INLINE mode #-}


type Makeflow = Seq.Seq Rule

data Opts = Opts

writeMakeflow :: Makeflow -> FilePath -> Opts -> IO ()
writeMakeflow mf p opts = undefined


class MF a where
    makeflow :: a -> Makeflow

class Eval a where
    eval :: a -> Rule


class Emerge a where
    emerge :: a -> String

instance Emerge File where
    emerge = filepath

emerge_rule :: Rule -> String
emerge_rule (Rule (Output outs) (Input ins) cmds) =
    printf "%s : %s\n\t%s"
               (intercalate " " . map filepath $ outs)
               (intercalate " " . map filepath $ ins)
               (emerge cmds)
instance Emerge Rule where
    emerge = emerge_rule


emerge_command_list = intercalate " ; " . map emerge
instance Emerge [Command] where
    emerge = emerge_command_list

emerge_command (Cmd exe args redir) =
    printf "%s %s %s"
               (emerge exe)
               (emerge args)
               (emerge redir)
instance Emerge Command where
    emerge = emerge_command


emerge_executable (Exec path) = filepath path
instance Emerge Executable where
    emerge = emerge_executable

emerge_arg (Arg a) = filepath a
emerge_arg (Flag s) = s
instance Emerge Arg where
    emerge = emerge_arg

emerge_args = intercalate " " . map emerge
instance Emerge [Arg] where
    emerge = emerge_args


emerge_redirection (Redir m b p) = printf "%s%s %s" (buffer b) (mode m) (emerge p)
emerge_redirection (Combine m p) = printf "%s %s 2>&1" (mode m) (emerge p)
emerge_redirection (Split r1 r2) = emerge_redirection r1 ++ " " ++ emerge_redirection r2
instance Emerge Redirection where
    emerge = emerge_redirection



data Compilation a b = End a | Step b

class Compile a b where
    type Step :: *
    compile :: a -> Compilation b Step


-- instance Compile String Arg where
--     type Step = [Arg]
--     compile str = 