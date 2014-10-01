{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module App.Environment where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.List (intercalate)
import Control.Applicative
import Control.Monad.Reader
import System.Environment

data Env = Development | Production
  deriving Show

data ForEnv a = ForEnv {
    forEnvDescription :: String
  , development :: Maybe a
  , production :: Maybe a
  }

data ForEnvBuilder a = FEB { buildForEnv :: String -> ForEnv a }

instance FromJSON a => FromJSON (ForEnvBuilder a) where
  parseJSON (Object v) = do
    dev <- v .:? "development"
    prod <- v .:? "production"
    return $ FEB (\desc -> ForEnv desc dev prod)

  parseJSON _ = mzero

loadForEnv :: FromJSON a => String -> String -> IO (ForEnv a)
loadForEnv description path = do
  decoded <- eitherDecodeStrict <$> BS.readFile path
  case decoded of
    Right builder -> return $ buildForEnv builder description
    Left e -> error $ "Unable to load "
                   ++ description
                   ++ " from "
                   ++ path
                   ++ ": "
                   ++ e

newtype InEnv a = InEnv (ReaderT Env (Either String) a)
  deriving (Functor, Applicative, Monad)

runEnv :: Env -> InEnv a -> Either String a
runEnv env (InEnv reader) = runReaderT reader env

runEnvOrCrash :: Env -> InEnv a -> a
runEnvOrCrash env inEnv =
  case runEnv env inEnv of
  Right a -> a
  Left e -> error e

getForEnv :: ForEnv a -> InEnv a
getForEnv config = InEnv $ do
  value <- asks (forEnv config)
  case value of
    Just a -> lift (Right a)
    _ -> do env <- ask
            let msg = intercalate " " [ "Missing"
                                      , forEnvDescription config
                                      , "for"
                                      , show env
                                      ]
            lift (Left msg)

forEnv :: ForEnv a -> Env -> Maybe a
config `forEnv` Development = development config
config `forEnv` Production = production config

envFromArgs :: [String] -> Env
envFromArgs args = if "Production" `elem` args
                   then Production
                   else Development

getEnvironment :: IO Env
getEnvironment = fmap envFromArgs getArgs
