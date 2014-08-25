{-# LANGUAGE GADTs #-}
module Handler.People
  ( people
  ) where

import Model
import Handler.Helpers
import View.People

people :: App Response
people = msum [
    methodM GET >> peopleList
  , methodM POST >> peopleCreate
  , entityId $ methodM POST >>. peopleUpdate
  , dir "new" $ methodM GET >> peopleNew
  , dir "edit" $ entityId $ methodM GET >>. peopleEdit
  ]

peopleRes :: Resource Person
peopleRes = defaultResource {
    resNewView = peopleNewView
  , resEditView = peopleEditView
  , resIndexUri = "/people"
  }

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
  post <- runForm "person" (personForm Nothing)
  handleCreate peopleRes post

peopleUpdate :: Entity Person -> App Response
peopleUpdate ent@(Entity key person) = do
  post <- runForm "person" (personForm (Just person))
  handleUpdate peopleRes ent post

personForm :: Monad m => Formlet Text m Person
personForm p = Person
  <$> "firstName" .: validate notEmpty (string (personFirstName <$> p))
  <*> "lastName" .: validate notEmpty (string (personLastName <$> p))

