{-# LANGUAGE
  FlexibleContexts,
  PackageImports
  #-}

module Hakflow.Abstraction where

import Hakflow.Makeflow
import Hakflow.Monad
import Hakflow.Commands

import Data.List (foldl')
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Default


-- for testing
import qualified Data.Text.IO as T
import qualified "monads-fd" Control.Monad.RWS.Strict as RWS
import Debug.Trace



amap :: Command a -> Vector (Parameter a) -> Vector (Command a)
amap c ps = V.map (c `addParam`) ps

addParam :: Command a -> Parameter a -> Command a
addParam (Cmd e ps r) p = Cmd e (ps ++ [p]) r


chunkBy :: Vector Rule -> Int -> Hak (Vector Rule)
chunkBy rules size = V.mapM join =<< V.foldM' pick V.empty rules
    where
      pick :: Vector (Vector Rule) -> Rule -> Hak (Vector (Vector Rule))
      pick rs r
          | V.length rs == 0 = return . V.singleton $ V.singleton r
          | V.length (V.last rs) < size = return $ V.init rs V.++ (V.singleton (V.last rs V.++ V.singleton r))
          | otherwise               = return $ rs V.++ V.singleton (V.singleton r)


-- join :: Vector (Vector Rule) -> Hak (Vector Rule)
join chunk = let files f = concat . V.toList $ V.map f chunk
                 ins = fileparams . V.fromList $ files filesin
             in do prog <- cat def ins
                   let (Rule outp inp cmds) = eval prog
                       out = head $ filesout outp
                   return $ Rule outp inp (cmds ++ [C prog])
                   
                   



data MapCfg = Map {
      chunksize :: Int
    }

instance Default MapCfg where
    def = Map {
            chunksize = 1
          }


mapA :: (FilesIn (Command a), FilesOut (Command a)) => MapCfg -> Command a -> Vector (Parameter a) -> Hak (Vector Rule)
mapA cfg c ps = flip chunkBy (chunksize cfg) =<< V.mapM (newRule c) ps

newRule :: (FilesIn (Command a), FilesOut (Command a)) => Command a -> Parameter a -> Hak Rule
newRule cmd par = do
  out <- output cmd
  let cmd' = redirect cmd $ Redir Write StdOut out
      cmd'' = trace ("newRule: " ++ show (eval cmd')) cmd'
  return $ eval cmd''


output :: FilesOut (Command a) => Command a -> Hak File
output c = if (null $ filesout c) || Out == redirection c
           then result
           else result --  error "Manual redirection of commands not supported yet"



testMap size = let prog = do prog <- cat def . fileparams . V.map (File . show) . V.fromList $ [1..5]
                             let prog' = trace ("Prog: " ++ show prog) prog
                             mapA def {chunksize=size} prog' (fileparams . V.map (File . show) . V.fromList $ [1..5])
               in do (mf,_,_) <- run prog def def
                     mapM_ (T.putStr . emerge . makeflow) [mf]

test = let prog = do progs <- mapM (cat def . fileparams . V.map (File . show) . V.fromList) [[1..5],[6..10]]
                     let [r1,r2] = map eval progs
                         c@(Cmd exe ps r) = head progs
                     RWS.liftIO $ print $ exe == exe && ps == ps && r == r && c == c
                     return $ (r1,r2,r1 `magma` r2)
       in do ((mf1,mf2,mf3),_,_) <- run prog def def
             mapM_ (T.putStr . emerge . makeflow) [mf1,mf2,mf3]
