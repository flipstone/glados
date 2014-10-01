{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.FobAssignments
( fobAssignments
) where

import qualified Data.Text as T

import Model
import Handler.Helpers
import View.FobAssignments

fobAssignments :: App Response
fobAssignments = routeResource $ ResourceActions {
    resActionList = fobAssignmentsList
  , resActionNew = fobAssignmentsNew
  , resActionEdit = fobAssignmentsEdit
  , resActionCreate = fobAssignmentsCreate
  , resActionUpdate = fobAssignmentsUpdate
  }

fobAssignmentsRes :: Resource FobAssignment
fobAssignmentsRes = defaultResource {
    resNewView = fobAssignmentsNewView
  , resEditView = fobAssignmentsEditView
  , resIndexUri = "/fobAssignments"
  }

fobAssignmentsList :: App Response
fobAssignmentsList = do
  fobAssignments <- runDB $ selectList [] [] :: App [Entity FobAssignment]
  fobAssignmentViews <- runDB $
    loadAssociations fobAssignments $
      FobAssignmentView
      <$> own entityKey
      <*> (entityVal <$> belongsTos FobAssignmentPersonId)
      <*> (entityVal <$> belongsTos FobAssignmentFobId)

  ok $ toResponse $ fobAssignmentsListView fobAssignmentViews

fobAssignmentsNew :: App Response
fobAssignmentsNew = do
  view <- getForm "fobAssignment" (fobAssignmentForm Nothing)
  ok $ toResponse $ fobAssignmentsNewView view

fobAssignmentsEdit :: Entity FobAssignment -> App Response
fobAssignmentsEdit ent@(Entity key fobAssignment) = do
  view <- getForm "fobAssignment" (fobAssignmentForm (Just fobAssignment))
  ok $ toResponse $ fobAssignmentsEditView ent view

fobAssignmentsCreate :: App Response
fobAssignmentsCreate = do
  post <- runForm "fobAssignment" (fobAssignmentForm Nothing)
  handleCreate fobAssignmentsRes post

fobAssignmentsUpdate :: Entity FobAssignment -> App Response
fobAssignmentsUpdate ent@(Entity key fobAssignment) = do
  post <- runForm "fobAssignment" (fobAssignmentForm (Just fobAssignment))
  handleUpdate  fobAssignmentsRes ent post

fobAssignmentForm :: Formlet Text App FobAssignment
fobAssignmentForm fa = monadic $ do
  people <- runDB $ selectList [] []
  fobs <- runDB $ selectList [] []

  return $ fobAssignmentFormPure people fobs fa

fobAssignmentFormPure  :: Monad m
                          => [Entity Person]
                          -> [Entity Fob]
                          -> Formlet Text m FobAssignment
fobAssignmentFormPure people fobs fa = FobAssignment
  <$> "personId" .: foreignKey people (fobAssignmentPersonId <$> fa)
  <*> "fobId" .: foreignKey fobs (fobAssignmentFobId <$> fa)
  <*> "startDate" .: dateField (fobAssignmentStartDate <$> fa)
  <*> "expirationDate" .: optionalDateField (fobAssignmentExpirationDate =<< fa)

instance SelectOption Person where
  toOptionText p = T.pack $ personFirstName p ++ " " ++
                            personLastName p

instance SelectOption Fob where
  toOptionText e = T.pack $ fobKey e


