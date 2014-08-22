module Main where

import Control.Monad.Logger (runStdoutLoggingT)
import Happstack.Server
import Database.Persist.Postgresql

import App.Types
import Model.DB
import Handler.People

connString = "host=db port=5432 dbname=glados_dev user=glados password=glados" 

main = do
  withPostgresqlPool connString 5 $ \pool -> do
    runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
    putStrLn $ "Starting server on port " ++ (show $ 8000)
    simpleHTTP nullConf $ runApp pool app

app :: App Response
app = do
  decodeBody $ defaultBodyPolicy "/tmp/" 4096 4096 4096
  dir "people" people

instance BackendHost IO where
  runDB action = do
    withPostgresqlPool connString 1 $ \pool -> do
      runBackendPool pool action

