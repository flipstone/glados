module Main where

import Control.Monad (msum)
import Control.Monad.Logger (runStdoutLoggingT)
import Happstack.Server
import Database.Persist.Postgresql

import App.Types
import App.Environment
import Config.DB

import Handler.Agreement
import Handler.Doors
import Handler.DoorKeys
import Handler.Equipment
import Handler.Fob
import Handler.FobAssignments
import Handler.People
import Handler.PossessionContracts
import Handler.Open
import Model.DB

connString = "host=db port=5432 dbname=glados_dev user=glados password=glados"

main :: IO ()
main = getEnvironment >>= runServer

runServer :: Env -> IO ()
runServer env = do
  dbConfig <- loadDBConfig
  runEnvOrCrash env $ do
    connString <- getForEnv dbConfig
    return $
      withPostgresqlPool connString 5 $ \pool -> do
        runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
        putStrLn $ "Starting server on port " ++ (show $ 8000)
        simpleHTTP nullConf $ runApp pool app

app :: App Response
app = do
  decodeBody $ defaultBodyPolicy "/tmp/" 4096 4096 4096
  msum [
      dir "open" open
    , dir "doors" doors
    , dir "doorKeys" doorKeys
    , dir "equipment" equipment
    , dir "people" people
    , dir "possessionContracts" possessionContracts
    , dir "agreements" agreements
    , dir "fobs" fobs
    , dir "fobAssignments" fobAssignments
    ]

instance BackendHost IO where
  runDB action = do
    withPostgresqlPool connString 1 $ \pool -> do
      runBackendPool pool action

