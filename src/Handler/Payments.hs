{-# LANGUAGE GADTs #-}
module Handler.Payments
  ( payments
  ) where

import qualified Data.Text as T

import Handler.Helpers
import Model
import View.Payments

payments :: Entity Membership -> App Response
payments membership = routeResource $ defaultActions {
    resActionList = paymentList membership
  , resActionNew = paymentNew membership
  , resActionEdit = paymentEdit membership
  , resActionCreate = paymentCreate membership
  , resActionUpdate = paymentUpdate membership
}

paymentRes :: Entity Membership -> Resource Payment
paymentRes membership = defaultResource {
    resNewView = paymentNewView membership
  , resEditView = paymentEditView membership
  , resIndexUri = T.concat ["/memberships/"
                           , entityKeyURI (entityKey membership)
                           ,"/payments"
                           ]
  }

paymentList :: Entity Membership -> App Response
paymentList membership = do
  payments <- runDB $ selectList [ PaymentMembershipId ==. entityKey membership ]
                                 [ Desc PaymentMembershipYear
                                 , Desc PaymentMembershipMonth
                                 ]
  ok $ toResponse $ paymentListView membership payments

paymentNew :: Entity Membership -> App Response
paymentNew membership = do
  view <- getForm "payment" (paymentForm membership Nothing)
  ok $ toResponse $ paymentNewView membership view

paymentEdit :: Entity Membership -> Entity Payment -> App Response
paymentEdit membership ent@(Entity key payment) = do
  view <- getForm "payment" (paymentForm membership (Just payment))
  ok $ toResponse $ paymentEditView membership ent view

paymentCreate :: Entity Membership -> App Response
paymentCreate membership =  do
  post <- runForm "payment" (paymentForm membership Nothing)
  handleCreate (paymentRes membership) post

paymentUpdate :: Entity Membership -> Entity Payment -> App Response
paymentUpdate membership ent@(Entity key payment) = do
  post <- runForm "payment" (paymentForm membership (Just payment))
  handleUpdate (paymentRes membership) ent post

paymentForm :: Monad m => Entity Membership -> Formlet Text m Payment
paymentForm membership p = Payment
  <$> pure (entityKey membership)
  <*> "receivedOn" .: dateField (paymentReceivedOn <$> p)
  <*> "amount" .: moneyField (paymentAmount <$> p)
  <*> "membershipMonth" .: stringRead "Invalid Month" (paymentMembershipMonth <$> p)
  <*> "membershipYear" .: stringRead "Invalid Year" (paymentMembershipYear <$> p)


