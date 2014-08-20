{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import Control.Monad.Logger (runStdoutLoggingT)
import Happstack.Server
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import App.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
|]

connString = "host=db port=5432 dbname=glados_dev user=glados password=glados" 

main = do
  withPostgresqlPool connString 5 $ \pool -> do
    runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
    putStrLn $ "Starting server on port " ++ (show $ 8000)
    simpleHTTP nullConf $ runApp pool app

app :: App String
app = ok ("Hello World!"::String)

