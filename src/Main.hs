module Main where

import Control.Monad (msum)
import Control.Monad.Logger (runStdoutLoggingT)
import Happstack.Server
import Database.Persist.Postgresql

import App.Types
import Config.DB

import Handler.Agreement
import Handler.Applications
import Handler.Doors
import Handler.DoorKeys
import Handler.Equipment
import Handler.Fob
import Handler.FobAssignments
import Handler.Memberships
import Handler.Payments
import Handler.People
import Handler.PossessionContracts
import Handler.Open
import Handler.Style
import Handler.Welcome

import Handler.Helpers.Routing

import Model.DB

main :: IO ()
main = do
  connString <- loadDBConfig
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
    , dir "people" (entityId $ \ent ->
        msum [ dir "applications" $ applications ent
             , dir "membership" $ memberships ent
             ])

    , dir "memberships" (entityId $ \ent ->
        msum [ dir "payments" $ payments ent
             ])

    , dir "possessionContracts" possessionContracts
    , dir "agreements" agreements
    , dir "fobs" fobs
    , dir "fobAssignments" fobAssignments
    , dir "style" style
    , welcome
    ]

instance BackendHost IO where
  runDB action = do
    connString <- loadDBConfig
    withPostgresqlPool connString 1 $ \pool -> do
      runBackendPool pool action

