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
    let excludes = ["--exclude='.DS_Store'"]
    let remoteDir = joinPaths [remote root, remote path]
    let localDir = joinPaths [local root, local path]
    let paths = [(remoteDir ++ "/"), (localDir ++ "/")]
    let rsyncOptions = ["-ruht", "--progress"] ++ excludes ++ paths
    exec "mkdir" ["-p", localDir]
    output <- exec "rsync" rsyncOptions
    putStrLn "done"
    putStrLn output

exec :: String -> [String] -> IO String
exec program args = do
    result <- readProcessWithExitCode program args []
    case result of
        (ExitSuccess, output, _) -> return output
        (ExitFailure code, _, output) -> do
          putStrLn output
          fail $ "exit code " ++ (show code)
