{-# LANGUAGE OverloadedStrings #-}
module Config.DB where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as E
import System.Environment

import Config.Load

type DbConfig = ByteString

loadDBConfig :: IO DbConfig
loadDBConfig = do
  bucketConfig <- lookupEnv "CONFIG_BUCKET"

  case bucketConfig of
    Nothing -> loadConfig (ConfigFile "config/development.json")
    Just bucket -> do
      key <- getEnv "CONFIG_KEY"
      loadConfig $ ConfigS3 bucket key

instance FromJSON ByteString where
  parseJSON (Object o) = E.encodeUtf8 <$> (o .: "database")
  parseJSON _ = mzero
