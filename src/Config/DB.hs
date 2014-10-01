{-# LANGUAGE OverloadedStrings #-}
module Config.DB where

import Control.Monad (mzero)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as E
import App.Environment

type DbConfig = ByteString

loadDBConfig :: IO (ForEnv DbConfig)
loadDBConfig = loadForEnv "database config" "config/db.json"

instance FromJSON ByteString where
  parseJSON (String t) = return $ E.encodeUtf8 t
  parseJSON _ = mzero
