{-# LANGUAGE DeriveDataTypeable #-}

import Data.Aeson.Generic(decode)
import Data.Data(Data, Typeable)
import Data.Maybe(fromJust)
import qualified Data.ByteString.Lazy as L8
import Path
import System.Process(readProcessWithExitCode)
import System.Exit
import Data.List(inits)

data PathPair = Path { local :: Path, remote :: Path } deriving (Show, Data, Typeable)
data Conf = Conf { root :: PathPair, paths :: [PathPair] } deriving (Show, Data, Typeable)

main = do
  string <- L8.readFile ".oegysyncrc"
  let conf = fromJust $ decode string :: Conf
  mapM_ (sync conf) (paths conf)

sync :: Conf -> PathPair -> IO ()
sync conf path = do
  let localPath = subPath (local $ root $ conf) (local path)
  let remotePath = subPath (remote $ root $ conf) (remote path)
  putStrLn $ "Syncing " ++ localPath ++ " <-> " ++ remotePath
  rsync path (root conf)

rsync :: PathPair -> PathPair -> IO ()
rsync path root = do
    let includes = []
    let excludes = ["--exclude='*'", "--exclude='.DS_Store'"]
    let paths = [remote root, local root]
    let rsyncOptions = ["-ruh", "--progress", "--include='*.*'"] ++ includes ++ excludes ++ paths
    putStrLn $ show rsyncOptions
    result <- readProcessWithExitCode "echo" rsyncOptions []
    case result of
        (ExitSuccess, output, _) -> do
          putStrLn "done"
          putStrLn output

rsyncIncludes :: [String] -> [String]
rsyncIncludes pathElems = map wrap $ paths ++ [last paths ++ "/**/"]
  where paths = map ((++ "/") . joinPaths) $ filter (/= []) $ inits pathElems
        wrap path = "--include='" ++ path ++ "'"
