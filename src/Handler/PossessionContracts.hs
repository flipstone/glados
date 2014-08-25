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
possessionContracts = msum [
    methodM GET >> possessionContractsList
  , methodM POST >> possessionContractsCreate
  , entityId $ methodM POST >>. possessionContractsUpdate
  , dir "new" $ methodM GET >> possessionContractsNew
  , dir "edit" $ entityId $ methodM GET >>. possessionContractsEdit
  ]

possessionContractsList :: App Response
possessionContractsList = do
  possessionContracts <- runDB $ selectList [] [] :: App [Entity PossessionContract]
  possessionContractViews <- runDB $
    loadAssociations possessionContracts $
      PossessionContractView
      <$> theEntity
      <*> belongsTos PersonId possessionContractPersonId
      <*> belongsTos EquipmentId possessionContractEquipmentId

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
  (view, result) <- runForm "possessionContract" (possessionContractForm Nothing)

  case result of
    Just possessionContract -> do
      runDB $ insert possessionContract
      found ("/possessionContracts"::String) $ toResponse ("Look over there"::String)

    Nothing ->
      badRequest $ toResponse $ possessionContractsNewView view

possessionContractsUpdate :: Entity PossessionContract -> App Response
possessionContractsUpdate ent@(Entity key possessionContract) = do
  (view, result) <- runForm "possessionContract" (possessionContractForm (Just possessionContract))

  case result of
    Just possessionContract -> do
      runDB $ replace key possessionContract
      found ("/possessionContracts"::String) $ toResponse ("Look over there"::String)

    Nothing ->
      badRequest $ toResponse $ possessionContractsEditView ent view

possessionContractForm :: Formlet Text App PossessionContract
possessionContractForm p = monadic $ do
  people <- runDB $ selectList [] []
  equipment <- runDB $ selectList [] []

  return $ possessionContractFormPure people equipment p

possessionContractFormPure  :: Monad m =>
                               [Entity Person] ->
                               [Entity Equipment] ->
                               Formlet Text m PossessionContract
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

