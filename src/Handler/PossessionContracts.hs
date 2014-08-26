{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Handler.PossessionContracts
  ( possessionContracts
  ) where

import qualified Data.Text as T

import Model
import Handler.Helpers
import View.PossessionContracts

possessionContracts :: App Response
possessionContracts = routeResource $ ResourceActions {
    resActionList = possessionContractsList
  , resActionNew = possessionContractsNew
  , resActionEdit = possessionContractsEdit
  , resActionCreate = possessionContractsCreate
  , resActionUpdate = possessionContractsUpdate
  }

possessionContractsRes :: Resource PossessionContract
possessionContractsRes = defaultResource {
    resNewView = possessionContractsNewView
  , resEditView = possessionContractsEditView
  , resIndexUri = "/possessionContracts"
  }

possessionContractsList :: App Response
possessionContractsList = do
  possessionContracts <- runDB $ selectList [] [] :: App [Entity PossessionContract]
  possessionContractViews <- runDB $
    loadAssociations possessionContracts $
      PossessionContractView
      <$> own entityKey
      <*> (entityVal <$> belongsTos possessionContractPersonId)
      <*> (entityVal <$> belongsTos possessionContractEquipmentId)

  ok $ toResponse $ possessionContractsListView possessionContractViews

possessionContractsNew :: App Response
possessionContractsNew = do
  view <- getForm "possessionContract" (possessionContractForm Nothing)
  ok $ toResponse $ possessionContractsNewView view

possessionContractsEdit :: Entity PossessionContract -> App Response
possessionContractsEdit ent@(Entity key possessionContract) = do
  view <- getForm "possessionContract" (possessionContractForm (Just possessionContract))
  ok $ toResponse $ possessionContractsEditView ent view

possessionContractsCreate :: App Response
possessionContractsCreate = do
  post <- runForm "possessionContract" (possessionContractForm Nothing)
  handleCreate possessionContractsRes post

possessionContractsUpdate :: Entity PossessionContract -> App Response
possessionContractsUpdate ent@(Entity key possessionContract) = do
  post <- runForm "possessionContract" (possessionContractForm (Just possessionContract))
  handleUpdate possessionContractsRes ent post

possessionContractForm :: Formlet Text App PossessionContract
possessionContractForm p = monadic $ do
  people <- runDB $ selectList [] []
  equipment <- runDB $ selectList [] []

  return $ possessionContractFormPure people equipment p

possessionContractFormPure  :: Monad m
                            => [Entity Person]
                            -> [Entity Equipment]
                            -> Formlet Text m PossessionContract
possessionContractFormPure people equipment p = PossessionContract
  <$> "personId" .: foreignKey people (possessionContractPersonId <$> p)
  <*> "equipmentId" .: foreignKey equipment (possessionContractEquipmentId <$> p)
  <*> "paymentCents" .: optionalStringRead "must be integral cents" (possessionContractPaymentCents =<< p)
  <*> "startDate" .: dateFormlet "%m/%d/%Y" (possessionContractStartDate <$> p)
  <*> "expirationDate" .: optionalDateFormlet "%m/%d/%Y" (possessionContractExpirationDate =<< p)

instance SelectOption Person where
  toOptionText p = T.pack $ personFirstName p ++ " " ++
                            personLastName p

instance SelectOption Equipment where
  toOptionText e = T.pack $ equipmentMake e ++ " " ++
                            equipmentModel e

