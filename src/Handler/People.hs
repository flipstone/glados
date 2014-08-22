{-# LANGUAGE GADTs #-}
module Handler.People
  ( people
  ) where

import Control.Applicative
import Control.Monad
import Database.Persist.Postgresql
import Happstack.Server
import Text.Digestive (Formlet, getForm, (.:), string, validate)
import Text.Digestive.Happstack
import Text.Hamlet (Html)

import Model
import App.Types
import Text.Digestive.Validations
import View.People

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
  ok $ toResponse $ peopleListView people

peopleNew :: App Response
peopleNew = do
  view <- getForm "person" (personForm Nothing)
  ok $ toResponse $ peopleNewView view

peopleEdit :: Entity Person -> App Response
peopleEdit ent@(Entity key person) = do
  view <- getForm "person" (personForm (Just person))
  ok $ toResponse $ peopleEditView ent view

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
      badRequest $ toResponse $ peopleEditView ent view

personForm :: Monad m => Formlet Html m Person
personForm p = Person
  <$> "firstName" .: validate notEmpty (string (personFirstName <$> p))
  <*> "lastName" .: validate notEmpty (string (personLastName <$> p))

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

