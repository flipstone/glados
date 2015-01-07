{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Handler.DoorKeys
  ( doorKeys
  ) where

import qualified Data.Text as T

import Model
import Handler.Helpers
import View.DoorKeys

doorKeys  :: App Response
doorKeys = routeResource $ defaultActions {
    resActionList = doorKeysList
  , resActionNew = doorKeysNew
  , resActionEdit = doorKeysEdit
  , resActionCreate = doorKeysCreate
  , resActionUpdate = doorKeysUpdate
  }

doorKeyRes :: Resource DoorKey
doorKeyRes = defaultResource {
    resNewView = doorKeysNewView
  , resEditView = doorKeysEditView
  , resIndexUri = "/doorKeys"
}

doorKeysList :: App Response
doorKeysList = do
  keys <- runDB $ selectList [] [] :: App [Entity DoorKey]
  keyViews <- runDB $
    loadAssociations keys $
      DoorKeyView
      <$> own id
      <*> ( belongsTos DoorKeyDoorId )
      <*> ( belongsTos DoorKeyPersonId )

  ok $ toResponse $ doorKeysListView keyViews

doorKeysNew :: App Response
doorKeysNew = do
  view <- getForm "doorKey" (doorKeyForm Nothing)
  ok $ toResponse $ doorKeysNewView view

doorKeysEdit :: Entity DoorKey -> App Response
doorKeysEdit ent@(Entity key doorKey) = do
  view <- getForm "doorKey" (doorKeyForm (Just doorKey))
  ok $ toResponse $ doorKeysEditView ent view

doorKeysCreate :: App Response
doorKeysCreate = do
  post <- runForm "doorKey" (doorKeyForm Nothing)
  handleCreate doorKeyRes post

doorKeysUpdate :: Entity DoorKey -> App Response
doorKeysUpdate ent@(Entity key doorKey) = do
  post <- runForm "doorKey" (doorKeyForm (Just doorKey))
  handleUpdate doorKeyRes ent post

doorKeyForm :: Maybe DoorKey -> Form Text App DoorKey
doorKeyForm doorKey = monadic $ do
  people <- runDB $ selectList [] []
  doors <- runDB $ selectList [] []
  return $ DoorKey
    <$> "doorId" .: foreignKey doors (doorKeyDoorId <$> doorKey)
    <*> "personId" .: foreignKey people (doorKeyPersonId <$> doorKey)
    <*> "startDate" .: dateField (doorKeyStartDate <$> doorKey)
    <*> "expirationDate" .: optionalDateField (doorKeyExpirationDate =<< doorKey)

instance SelectOption Person where
  toOptionText p = T.pack $ personFirstName p ++ " " ++
                            personLastName p
instance SelectOption Door where
  toOptionText d = T.pack $ doorName d
