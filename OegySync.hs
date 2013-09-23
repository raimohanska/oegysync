{-# LANGUAGE DeriveDataTypeable #-}

import Data.Aeson.Generic(decode)
import Data.Data(Data, Typeable)
import Data.Maybe(fromJust)
import qualified Data.ByteString.Lazy as L8

data Path = Path { local :: String, remote :: String } deriving (Show, Data, Typeable)
data Conf = Conf { localRoot :: String, remoteRoot :: String, paths :: [Path] } deriving (Show, Data, Typeable)

main = do
  string <- L8.readFile ".oegysyncrc"
  let conf = fromJust $ decode string :: Conf
  putStrLn $ localRoot conf
