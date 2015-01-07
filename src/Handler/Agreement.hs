{-# LANGUAGE GADTs #-}
module Handler.Agreement
  ( agreements
  ) where

import Handler.Helpers
import Model
import View.Agreement

agreements :: App Response
agreements = routeResource $ defaultActions {
    resActionList = agreementList
  , resActionNew = agreementNew
  , resActionEdit = agreementEdit
  , resActionCreate = agreementCreate
  , resActionUpdate = agreementUpdate
  }

agreementRes :: Resource Agreement
agreementRes = defaultResource {
    resNewView = agreementNewView
  , resEditView = agreementEditView
  , resIndexUri = "/agreements"
  }

agreementList :: App Response
agreementList = do
  agreements <- runDB $ selectList [] [] :: App [Entity Agreement]
  ok $ toResponse $ agreementListView agreements

agreementNew :: App Response
agreementNew = do
  view <- getForm "agreement" (agreementForm Nothing)
  ok $ toResponse $ agreementNewView view

agreementEdit :: Entity Agreement -> App Response
agreementEdit ent@(Entity key agreement) = do
  view <- getForm "agreement" (agreementForm (Just agreement))
  ok $ toResponse $ agreementEditView ent view

agreementCreate :: App Response
agreementCreate = do
  post <- runForm "agreement" (agreementForm Nothing)
  handleCreate agreementRes post

agreementUpdate :: Entity Agreement -> App Response
agreementUpdate ent@(Entity key agreement) = do
  post <- runForm "agreement" (agreementForm (Just agreement))
  handleUpdate agreementRes ent post

agreementForm :: Monad m => Formlet Text m Agreement
agreementForm e = Agreement
  <$> "name" .: validate notEmpty (string (agreementName <$> e))
  <*> "author" .: validate notEmpty (string (agreementAuthor <$> e))
  <*> "version" .: stringRead "must be an integer" (agreementVersion <$> e)
