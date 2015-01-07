{-# LANGUAGE GADTs #-}
module Handler.Equipment
  ( equipment
  ) where

import Handler.Helpers
import Model
import View.Equipment

equipment :: App Response
equipment = routeResource $ defaultActions {
    resActionList = equipmentList
  , resActionNew = equipmentNew
  , resActionEdit = equipmentEdit
  , resActionCreate = equipmentCreate
  , resActionUpdate = equipmentUpdate
  }

equipmentRes :: Resource Equipment
equipmentRes = defaultResource {
    resNewView = equipmentNewView
  , resEditView = equipmentEditView
  , resIndexUri = "/equipment"
  }

equipmentList :: App Response
equipmentList = do
  equipment <- runDB $ selectList [] [] :: App [Entity Equipment]
  ok $ toResponse $ equipmentListView equipment

equipmentNew :: App Response
equipmentNew = do
  view <- getForm "equipment" (equipmentForm Nothing)
  ok $ toResponse $ equipmentNewView view

equipmentEdit :: Entity Equipment -> App Response
equipmentEdit ent@(Entity key equipment) = do
  view <- getForm "equipment" (equipmentForm (Just equipment))
  ok $ toResponse $ equipmentEditView ent view

equipmentCreate :: App Response
equipmentCreate = do
  post <- runForm "equipment" (equipmentForm Nothing)
  handleCreate equipmentRes post

equipmentUpdate :: Entity Equipment -> App Response
equipmentUpdate ent@(Entity key equipment) = do
  post <- runForm "equipment" (equipmentForm (Just equipment))
  handleUpdate equipmentRes ent post

equipmentForm :: Monad m => Formlet Text m Equipment
equipmentForm e = Equipment
  <$> "make" .: validate notEmpty (string (equipmentMake <$> e))
  <*> "model" .: validate notEmpty (string (equipmentModel <$> e))
  <*> "serialNumber" .: optionalString (equipmentSerialNumber =<< e)
  <*> "replacementCost" .: optionalStringRead "must be an integer" (equipmentReplacementCost =<< e)

