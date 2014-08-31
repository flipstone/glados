{-# LANGUAGE GADTs #-}
module Handler.Fob
  ( fob
  ) where

import Handler.Helpers
import Model
import View.Fob

fob :: App Response
fob = routeResource $ ResourceActions {
    resActionList = fobList
  , resActionNew = fobNew
  , resActionEdit = fobEdit
  , resActionCreate = fobCreate
  , resActionUpdate = fobUpdate
}

fobRes :: Resource Fob
fobRes = defaultResource {
    resNewView = fobNewView
  , resEditView = fobEditView
  , resIndexUri = "/fob"
  }

fobList :: App Response
fobList = do
  fob <- runDB $ selectList [] [] :: App [Entity Fob]
  ok $ toResponse $ fobListView fob

fobNew :: App Response
fobNew = do
  view <- getForm "fob" (fobForm Nothing)
  ok $ toResponse $ fobNewView view

fobEdit :: Entity Fob -> App Response
fobEdit ent@(Entity key fob) = do
  view <- getForm "fob" (fobForm (Just fob))
  ok $ toResponse $ fobEditView ent view

fobCreate :: App Response
fobCreate =  do
  post <- runForm "fob" (fobForm Nothing)
  handleCreate fobRes post

fobUpdate :: Entity Fob -> App Response
fobUpdate ent@(Entity key fob) = do
  post <- runForm "fob" (fobForm (Just fob))
  handleUpdate fobRes ent post

fobForm :: Monad m => Formlet Text m Fob
fobForm f = Fob
  <$> "key" .: validate notEmpty (string (fobKey <$> f))







