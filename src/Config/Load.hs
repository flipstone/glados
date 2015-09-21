module Config.Load
  ( loadConfig
  , ConfigLocation(..)
  ) where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Network.AWS.AWSConnection
import Network.AWS.S3Object
import System.Environment

data ConfigLocation =
    ConfigFile FilePath
  | ConfigS3 Bucket Key

type Bucket = String
type Key = String

loadConfig :: FromJSON a => ConfigLocation -> IO a
loadConfig (ConfigFile path) = do
  decoded <- eitherDecodeStrict <$> BS.readFile path
  case decoded of
    Right config -> pure config
    Left e -> error $ "Unable to load config from file"
                   ++ path
                   ++ ": "
                   ++ e

loadConfig (ConfigS3 bucket key) = do
  conn <- connection
  result <- getObject conn $ S3Object {
              obj_bucket = bucket
            , obj_name = key
            , content_type = "application/json"
            , obj_headers = []
            , obj_data = ""
            }

  case result of
    Left reqError -> error $ show reqError
    Right obj ->
      case eitherDecode $ obj_data obj of
        Right config -> pure config
        Left e -> error $ "Unable to load config from file"
                       ++ "s3://" ++ bucket ++ "/" ++ key
                       ++ ": "
                       ++ e


connection :: IO AWSConnection
connection = do
  accessKeyId <- getEnv "CONFIG_ACCESS_KEY_ID"
  secretAccessKey <- getEnv "CONFIG_SECRET_ACCESS_KEY"

  pure $ AWSConnection {
      awsHost = "s3.amazonaws.com"
    , awsPort = 80
    , awsAccessKey = accessKeyId
    , awsSecretKey = secretAccessKey
    }

