{-# LANGUAGE GADTs #-}
module Handler.Doors
  ( doors
  ) where

import Model
import Handler.Helpers
import View.Doors

doors :: App Response
doors = routeResource $ (ResourceActions {
    resActionList = doorsList
  , resActionNew = doorsNew
  , resActionCreate = doorsCreate
  , resActionEdit = doorsEdit
  , resActionUpdate = doorsUpdate
})

doorsRes :: Resource Door
doorsRes = defaultResource {
    resIndexUri = "/doors"
  , resNewView = doorsNewView
  , resEditView = doorsEditView
}

doorsEdit :: Entity Door -> App Response
doorsEdit ent@(Entity _ door) = do
  view <- getForm "door" (doorForm (Just door))
  ok $ toResponse $ doorsEditView ent view

doorsList :: App Response
doorsList = do
  doors <- runDB $ selectList [] [] :: App [Entity Door]
  doorViews <- runDB $
    loadAssociations doors $
      DoorView
      <$> own id
  ok $ toResponse $ doorsListView doorViews

doorsNew :: App Response
doorsNew = do
  view <- getForm "door" (doorForm Nothing)
  ok $ toResponse $ doorsNewView view

doorsCreate :: App Response
doorsCreate = do
  post <- runForm "door" (doorForm Nothing)
  handleCreate doorsRes post

doorsUpdate :: Entity Door -> App Response
doorsUpdate ent@(Entity _ door) = do
  put <- runForm "door" (doorForm (Just door))
  handleUpdate doorsRes ent put

doorForm :: Formlet Text App Door
doorForm d = Door
  <$> "name" .: validate notEmpty (string (doorName <$> d))
  <*> "hardwareAddress" .: validate notEmpty (string (doorHardwareAddress <$> d))

