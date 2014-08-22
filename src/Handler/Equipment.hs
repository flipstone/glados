{-# LANGUAGE GADTs #-}
module Handler.Equipment
  ( equipment
  ) where

import Handler.Helpers
import Model
import View.Equipment

equipment :: App Response
equipment = msum [
    methodM GET >> equipmentList
  , methodM POST >> equipmentCreate
  , entityId $ methodM POST >>. equipmentUpdate
  , dir "new" $ methodM GET >> equipmentNew
  , dir "edit" $ entityId $ methodM GET >>. equipmentEdit
  ]

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
  (view, result) <- runForm "equipment" (equipmentForm Nothing)

  case result of
    Just equipment -> do
      runDB $ insert equipment
      found ("/equipment"::String) $ toResponse ("Look over there"::String)

    Nothing ->
      badRequest $ toResponse $ equipmentNewView view

equipmentUpdate :: Entity Equipment -> App Response
equipmentUpdate ent@(Entity key equipment) = do
  (view, result) <- runForm "equipment" (equipmentForm (Just equipment))

  case result of
    Just equipment -> do
      runDB $ replace key equipment
      found ("/equipment"::String) $ toResponse ("Look over there"::String)

    Nothing ->
      badRequest $ toResponse $ equipmentEditView ent view

equipmentForm :: Monad m => Formlet Text m Equipment
equipmentForm e = Equipment
  <$> "make" .: validate notEmpty (string (equipmentMake <$> e))
  <*> "model" .: validate notEmpty (string (equipmentModel <$> e))
  <*> "serialNumber" .: optionalString (equipmentSerialNumber =<< e)
  <*> "replacementCost" .: optionalStringRead "must be an integer" (equipmentReplacementCost =<< e)

