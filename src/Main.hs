{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Logger (runStdoutLoggingT)
import Happstack.Server
import Data.Text (Text)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Text.Digestive
import Text.Digestive.Blaze.Html5
import Text.Digestive.Happstack
import Text.Hamlet (shamlet, Html)

import App.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
  firstName String
  lastName String
  deriving Show
|]

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

people :: App Response
people = msum [
    methodM GET >> peopleList
  , methodM POST >> peopleCreate
  , dir "new" $ methodM GET >> peopleNew
  ]

peopleList :: App Response
peopleList = do
  people <- runDB $ selectList [] [] :: App [Entity Person]
  ok $ toResponse $ [shamlet|
    <html>
      <body>
        <ul>
          $forall Entity _ p <- people
            <li>
              #{personFirstName p}
              #{personLastName p}
    |]

peopleNew :: App Response
peopleNew = do
  view <- getForm "person" personForm
  ok $ toResponse $ personView view

personView :: View Text -> Html
personView view = [shamlet|
  <html>
    <body>
      <form action="/people" method="POST">
        <div>
          ^{label "firstName" view "First Name"}
          ^{inputText "firstName" view}
        <div>
          ^{label "lastName" view "Last Name"}
          ^{inputText "lastName" view}

        <input type="submit" value="save">
  |]

peopleCreate :: App Response
peopleCreate = do
  (view, result) <- runForm "person" personForm

  case result of
    Just person -> do
      runDB $ insert person
      found ("/people"::String) $ toResponse ("Look over there"::String)

    Nothing ->
      badRequest $ toResponse $ personView view

personForm :: Monad m => Form Text m Person
personForm = Person
         <$> "firstName" .: string Nothing
         <*> "lastName" .: string Nothing

instance BackendHost IO where
  runDB action = do
    withPostgresqlPool connString 1 $ \pool -> do
      runBackendPool pool action


