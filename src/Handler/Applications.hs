{-# LANGUAGE GADTs #-}
module Handler.Applications
  ( applications
  ) where

import qualified Data.Text as T

import Model
import Handler.Helpers
import View.Applications

applications :: Entity Person -> App Response
applications person = routeResource $ defaultActions {
    resActionNew = applicationsNew person
  , resActionEdit = applicationsEdit person
  , resActionShow = applicationsShow person
  , resActionCreate = applicationsCreate person
  , resActionUpdate = applicationsUpdate person
  }

applicationsRes :: Entity Person -> Resource Application
applicationsRes person = defaultResource {
    resNewView = applicationsNewView person
  , resEditView = applicationsEditView person
  , resIndexUri = T.concat [ "/people/"
                           , entityKeyURI (entityKey person)
                           ]
  }

applicationsNew :: Entity Person -> App Response
applicationsNew person = do
  view <- getForm "application" (applicationForm person Nothing)
  ok $ toResponse $ applicationsNewView person view

applicationsEdit :: Entity Person -> Entity Application -> App Response
applicationsEdit person ent@(Entity key application) = do
  view <- getForm "application" (applicationForm person (Just application))
  ok $ toResponse $ applicationsEditView person ent view

applicationsShow :: Entity Person -> Entity Application -> App Response
applicationsShow person ent@(Entity key application) = do
  ok $ toResponse $ applicationsShowView person ent

applicationsCreate :: Entity Person -> App Response
applicationsCreate person = do
  post <- runForm "application" (applicationForm person Nothing)
  handleCreate (applicationsRes person) post

applicationsUpdate :: Entity Person -> Entity Application -> App Response
applicationsUpdate person ent@(Entity key application) = do
  post <- runForm "application" (applicationForm person (Just application))
  handleUpdate (applicationsRes person) ent post

applicationForm :: Monad m => Entity Person -> Formlet Text m Application
applicationForm person a = Application
  <$> pure (entityKey person)
  <*> "name" .: validate notEmpty (string (applicationName <$> a))
  <*> "streetAddress" .: validate notEmpty (string (applicationStreetAddress <$> a))
  <*> "city" .: validate notEmpty (string (applicationCity <$> a))
  <*> "state" .: validate notEmpty (string (applicationState <$> a))
  <*> "zip" .: validate notEmpty (string (applicationZip <$> a))
  <*> "cellPhone" .: optionalString (applicationCellPhone =<< a)
  <*> "workPhone" .: optionalString (applicationWorkPhone =<< a)
  <*> "emailAddress" .: optionalString (applicationEmailAddress =<< a)
  <*> "planType" .: choice planTypeOptions (applicationPlanType <$> a)
  <*> "nameOfSpouse" .: optionalString (applicationNameOfSpouse =<< a)

planTypeOptions :: [ (MembershipPlanType, T.Text) ]
planTypeOptions = map mkOption membershipPlanTypes
  where mkOption planType = (planType, T.pack (show planType))

