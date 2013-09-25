{-# LANGUAGE DeriveDataTypeable #-}

import Data.Aeson.Generic(decode)
import Data.Data(Data, Typeable)
import Data.Maybe(fromJust)
import qualified Data.ByteString.Lazy as L8
import Path
import System.Process
import System.Exit
import System.Directory(doesDirectoryExist, getHomeDirectory, doesFileExist)
import Data.List(inits)
import Control.Monad(when)

data PathPair = Path { local :: Path, remote :: Path } deriving (Show, Data, Typeable)
data Conf = Conf { root :: PathPair, paths :: [PathPair] } deriving (Show, Data, Typeable)

main = do
  string <- getConfigFile >>= L8.readFile
  let conf = fromJust $ decode string :: Conf
  mapM_ (sync conf) (paths conf)

getConfigFile = do
  let fileName = ".oegysyncrc"
  exists <- doesFileExist fileName
  if exists 
    then putStrLn "using local config" >> return fileName
    else getHomeDirectory >>= return . (++ ("/" ++ fileName))

sync :: Conf -> PathPair -> IO ()
sync conf path = do
  rsync path (root conf)

rsync :: PathPair -> PathPair -> IO ()
rsync path root = do
    let remoteDir = joinPaths [remote root, remote path]
    let localDir = joinPaths [local root, local path]
    rsync1way remoteDir localDir
    rsync1way localDir remoteDir 

rsync1way :: Path -> Path -> IO ()
rsync1way src dst = do
    srcExists <- doesDirectoryExist src
    when srcExists $ do
      putStrLn $ "Syncing " ++ src ++ " --> " ++ dst
      let excludes = ["--exclude='.DS_Store'"]
      let paths = [(src ++ "/"), (dst ++ "/")]
      let rsyncOptions = ["-ruht", "--progress"] ++ excludes ++ paths
      exec "mkdir" ["-p", dst]
      exec "rsync" rsyncOptions
      putStrLn "done"

exec :: String -> [String] -> IO ()
exec program args = do
    handle <- runProcess program args Nothing Nothing Nothing Nothing Nothing
    exitCode <- waitForProcess handle
    case exitCode of
        ExitSuccess -> return ()
        (ExitFailure code) -> do
          fail $ "exit code " ++ (show code)
