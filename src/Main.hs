{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Logger (runStdoutLoggingT)
import Happstack.Server
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Database.Persist.Types
import Text.Digestive (Formlet, View, getForm,
                       (.:), string, validate, Result(..))
import Text.Digestive.Blaze.Html5
import Text.Digestive.Happstack
import Text.Hamlet (shamlet, Html)
import Text.Blaze.Html (ToMarkup, toMarkup)

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

(>>.) :: Monad m => m c -> (a -> m b) -> (a -> m b)
m1 >>. f = \a -> m1 >> f a

people :: App Response
people = msum [
    methodM GET >> peopleList
  , methodM POST >> peopleCreate
  , entityId $ methodM POST >>. peopleUpdate
  , dir "new" $ methodM GET >> peopleNew
  , dir "edit" $ entityId $ methodM GET >>. peopleEdit
  ]

peopleList :: App Response
peopleList = do
  people <- runDB $ selectList [] [] :: App [Entity Person]
  ok $ toResponse $ [shamlet|
    <html>
      <body>
        <a href="/people/new">Add a Person</a>
        <ul>
          $forall Entity key p <- people
            <li>
              #{personFirstName p}
              #{personLastName p}

              <a href="/people/edit/#{key}">Edit</a>
    |]

peopleNew :: App Response
peopleNew = do
  view <- getForm "person" (personForm Nothing)
  ok $ toResponse $ peopleNewView view

peopleEdit :: Entity Person -> App Response
peopleEdit ent@(Entity key person) = do
  view <- getForm "person" (personForm (Just person))
  ok $ toResponse $ peopleEditView ent view

peopleNewView :: View Html -> Html
peopleNewView view = [shamlet|
  <html>
    <body>
      <form action="/people" method="POST">
        ^{personFields view}

        <input type="submit" value="save">
  |]

peopleEditView :: Entity Person -> View Html -> Html
peopleEditView (Entity id _) view = [shamlet|
  <html>
    <body>
      <form action="/people/#{id}" method="POST">
        ^{personFields view}

        <input type="submit" value="save">
  |]

personFields :: View Html -> Html
personFields view = [shamlet|
  <div>
    ^{label "firstName" view "First Name"}
    ^{inputText "firstName" view}
    ^{errorList "firstName" view}
  <div>
    ^{label "lastName" view "Last Name"}
    ^{inputText "lastName" view}
    ^{errorList "lastName" view}
  |]

peopleCreate :: App Response
peopleCreate = do
  (view, result) <- runForm "person" (personForm Nothing)

  case result of
    Just person -> do
      runDB $ insert person
      found ("/people"::String) $ toResponse ("Look over there"::String)

    Nothing ->
      badRequest $ toResponse $ peopleNewView view

peopleUpdate :: Entity Person -> App Response
peopleUpdate ent@(Entity key person) = do
  (view, result) <- runForm "person" (personForm (Just person))

  case result of
    Just person -> do
      runDB $ replace key person
      found ("/people"::String) $ toResponse ("Look over there"::String)

    Nothing ->
      badRequest $ toResponse $ peopleNewView view

personForm :: Monad m => Formlet Html m Person
personForm p = Person
  <$> "firstName" .: validate notEmpty (string (personFirstName <$> p))
  <*> "lastName" .: validate notEmpty (string (personLastName <$> p))

notEmpty :: String -> Result Html String
notEmpty s = if any (not . isSpace) s
             then Success s
             else Error "must not be empty"

entityId :: ( PersistEntity entity,
              PersistEntityBackend entity ~ SqlBackend)
            => (Entity entity -> App Response) -> App Response
entityId action = path $ \id -> do
  result <- runDB $ get id

  case result of
    Just ent -> action (Entity id ent)
    Nothing -> notFound $ toResponse ("Not found"::String)

instance FromReqURI (KeyBackend backend entity) where
  fromReqURI = fmap (Key . PersistInt64) . fromReqURI

instance ToMarkup (KeyBackend backend entity) where
  toMarkup (Key id) = [shamlet|#{id}|]

instance ToMarkup PersistValue where
  toMarkup val = [shamlet|#{render val}|]
    where render :: PersistValue -> Text
          render (PersistText t) = t
          render (PersistByteString bs) = E.decodeUtf8 bs
          render (PersistInt64 i) = T.pack $ show i
          render (PersistDouble d) = T.pack $ show d
          render (PersistBool b) = T.pack $ show b
          render (PersistDay d) = T.pack $ show d
          render (PersistTimeOfDay tod) = T.pack $ show tod
          render (PersistUTCTime utc) = T.pack $ show utc
          render PersistNull = T.empty

instance BackendHost IO where
  runDB action = do
    withPostgresqlPool connString 1 $ \pool -> do
      runBackendPool pool action

