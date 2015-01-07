{-# LANGUAGE GADTs #-}
module Handler.Fob
  ( fobs
  ) where

import Handler.Helpers
import Model
import View.Fob

fobs :: App Response
fobs = routeResource $ defaultActions {
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
  , resIndexUri = "/fobs"
  }

fobList :: App Response
fobList = do
  fobs <- runDB $ selectList [] [] :: App [Entity Fob]
  ok $ toResponse $ fobListView fobs

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







