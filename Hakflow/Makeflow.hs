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
  NoMonomorphismRestriction,
  NoImplicitPrelude
  #-}

module Hakflow.Makeflow where

import Hakflow.Util

import Text.Printf
import Data.List (intercalate,intersect,nub)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Record.Label
import Data.Default
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Prelude.Plus


data Rule = Rule { outputs :: Set File
                 , inputs  :: Set File
                 , mainOut :: Maybe File
                 , commands :: Vector Command
                 } deriving (Eq, Show)

newtype Executable = Exe {exeFile :: File} deriving (Eq, Ord, Show)
executable :: FilePath -> Executable
executable = Exe . File

data Command = Cmd { exec :: Executable
                   , params :: [Parameter]
                   , depends :: Set File
                   , redirection :: Maybe Redirection
                   } deriving (Eq, Ord, Show)


data ParamType = TextArg Text | FileInArg File | FileOutArg File deriving (Eq, Ord, Show)
data Parameter = Param ParamType | Flagged Text ParamType deriving (Eq, Ord, Show)

textArg = TextArg . pack
fileInArg = FileInArg . File
fileOutArg = FileOutArg . File
param = Param
flagged f = Flagged (pack f)

data Buffer = StdOut | StdErr deriving (Eq, Ord, Show)
data Mode = Write | Append deriving (Eq, Ord, Show)
data Redirection = Redir Buffer Mode File
                 | Combine Mode File
                 | Out
                   deriving (Eq, Ord, Show)
redirectionFile :: Redirection -> File
redirectionFile (Redir _ _ f) = f
redirectionFile (Combine _ f) = f




data MakeflowOpts = MFOpts {
      _counterDigits :: !Int
    }

$(mkLabels [''MakeflowOpts])
counterDigits :: MakeflowOpts :-> Int

instance Default MakeflowOpts where
    def = MFOpts 9

type Flow = Vector Rule
class Monad m => Makeflow m a where makeflow :: a -> m Flow
class Monad m => Eval m a where eval :: a -> m Rule
class Emerge a where emerge :: a -> Text

class FilesIn a where filesin :: a -> Set File
class FilesOut a where filesout :: a -> Set File


instance FilesIn Executable where
    filesin = S.singleton . exeFile

instance FilesIn ParamType where
    filesin (FileInArg f) = S.singleton f
    filesin _ = S.empty

instance FilesIn Parameter where
    filesin (Param pt) = filesin pt
    filesin (Flagged _ pt) = filesin pt

instance FilesIn [Parameter] where
    filesin = foldl' (\fs p -> fs `S.union` filesin p) S.empty


instance FilesOut ParamType where
    filesout (FileOutArg f) = S.singleton f
    filesout _ = S.empty

instance FilesOut Parameter where
    filesout (Param pt) = filesout pt
    filesout (Flagged _ pt) = filesout pt

instance FilesOut [Parameter] where
    filesout = foldl (\fs p -> fs `S.union` filesout p) S.empty


instance FilesOut Redirection where
    filesout (Redir _ _ f) = S.singleton f
    filesout (Combine _ f) = S.singleton f


instance FilesIn Command where
    filesin c = S.singleton (exeFile . exec $ c) `S.union`
                filesin (params c)               `S.union`
                depends c

instance FilesOut Command where
    filesout = filesout . params



emergeExecutable (Exe f) = pack . path $ f

emergeParamType (TextArg t) = t
emergeParamType (FileInArg f) = pack . path $ f
emergeParamType (FileOutArg f) = pack . path $ f

emergeParameter (Param pt) = emergeParamType pt
emergeParameter (Flagged t pt) = t `T.append` pack " " `T.append` emergeParamType pt

emergeCommand c = let exe = emergeExecutable . exec $ c
                      ps  = foldl' (\t p -> t `T.append` pack " " `T.append` emergeParameter p) T.empty (params c)
                      r   = emergeRedirection $ fromMaybe Out (redirection c)
                  in exe `T.append` ps `T.append` pack " " `T.append` r


emergeBuffer StdOut = pack "1"
emergeBuffer StdErr = pack "2"
emergeMode Write = pack ">"
emergeMode Append = pack ">>"
emergeRedirection (Redir b m f) = emergeBuffer b `T.append`
                                  emergeMode m   `T.append`
                                  pack (path f)
emergeRedirection (Combine m f) = pack "2>&1"    `T.append`
                                  pack " "       `T.append`
                                  emergeMode m   `T.append`
                                  pack (path f)
emergeRedirection Out = T.empty


instance Emerge Rule where
    emerge r = let paths f = S.map (pack . path) (f r)
                   spaces = foldr' (\txts txt -> txts `T.append` pack " " `T.append` txt) T.empty
                   outs = main `T.append` pack " " `T.append` spaces (paths outputs)
                   ins  = spaces (paths inputs)
                   main = if isJust (mainOut  r)
                          then T.pack . path . fromJust $ mainOut r
                          else T.empty
                   cmds = V.foldl' (\cs c -> cs `T.append` c `T.append` pack ";" ) T.empty $ V.map emergeCommand (commands r)
               in outs`T.append` pack ": " `T.append` ins `T.append` pack "\n\t" `T.append` cmds



instance Emerge Flow where
    emerge = V.foldl' (\mf r -> mf `T.append` emerge r `T.append` pack "\n") T.empty

