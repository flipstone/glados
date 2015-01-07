{-# LANGUAGE GADTs #-}
module Handler.Memberships
  ( memberships
  ) where

import qualified Data.Text as T

import Model
import Handler.Helpers
import View.Memberships

memberships :: Entity Person -> App Response
memberships person = routeResource $ defaultActions {
    resActionNew = membershipsNew person
  , resActionEdit = membershipsEdit person
  , resActionShow = membershipsShow person
  , resActionCreate = membershipsCreate person
  , resActionUpdate = membershipsUpdate person
  }

membershipsRes :: Entity Person -> Resource Membership
membershipsRes person = defaultResource {
    resNewView = membershipsNewView person
  , resEditView = membershipsEditView person
  , resIndexUri = T.concat [ "/people/"
                           , entityKeyURI (entityKey person)
                           ]
  }

membershipsNew :: Entity Person -> App Response
membershipsNew person = do
  view <- getForm "membership" (membershipForm person Nothing)
  ok $ toResponse $ membershipsNewView person view

membershipsEdit :: Entity Person -> Entity Membership -> App Response
membershipsEdit person ent@(Entity key membership) = do
  view <- getForm "membership" (membershipForm person (Just membership))
  ok $ toResponse $ membershipsEditView person ent view

membershipsShow :: Entity Person -> Entity Membership -> App Response
membershipsShow person ent@(Entity key membership) = do
  ok $ toResponse $ membershipsShowView person ent

membershipsCreate :: Entity Person -> App Response
membershipsCreate person = do
  post <- runForm "membership" (membershipForm person Nothing)
  handleCreate (membershipsRes person) post

membershipsUpdate :: Entity Person -> Entity Membership -> App Response
membershipsUpdate person ent@(Entity key membership) = do
  post <- runForm "membership" (membershipForm person (Just membership))
  handleUpdate (membershipsRes person) ent post

membershipForm :: Monad m => Entity Person -> Formlet Text m Membership
membershipForm person a = Membership
  <$> pure (entityKey person)
  <*> "active" .: bool (membershipActive <$> a)


