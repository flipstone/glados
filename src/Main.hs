module Main where

import Control.Monad (msum)
import Control.Monad.Logger (runStdoutLoggingT)
import Happstack.Server
import Database.Persist.Postgresql

import App.Types
import Model.DB
import Handler.Equipment
import Handler.People
import Handler.PossessionContracts
import Handler.Agreement
import Handler.Fob
import Handler.Doors

connString = "host=db port=5432 dbname=glados_dev user=glados password=glados"

main = do
  withPostgresqlPool connString 5 $ \pool -> do
    runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
    putStrLn $ "Starting server on port " ++ (show $ 8000)
    simpleHTTP nullConf $ runApp pool app

app :: App Response
app = do
  decodeBody $ defaultBodyPolicy "/tmp/" 4096 4096 4096
  msum [
      dir "doors" doors
    , dir "equipment" equipment
    , dir "people" people
    , dir "possessionContracts" possessionContracts
    , dir "agreement" agreement
    , dir "fob" fob
    ]

instance BackendHost IO where
  runDB action = do
    withPostgresqlPool connString 1 $ \pool -> do
      runBackendPool pool action

