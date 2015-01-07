{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module Model.DB
  ( module Model.FieldTypes
  , module Model.DB
  ) where

import Data.Time.Calendar (Day)
import Database.Persist.TH

import Model.FieldTypes

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Application
  personId PersonId
  name String
  signatureDate Day
  streetAddress String
  city String
  state String
  zip String
  cellPhone String Maybe
  workPhone String Maybe
  emailAddress String Maybe

  planType MembershipPlanType
  nameOfSpouse String Maybe

  referralSource ReferralSource
  referralOther String Maybe

  interests Interests
  interestsOther String Maybe

  emergencyContactName String
  emergencyContactHomePhone String
  emergencyContactWorkPhone String

Equipment
  make String
  model String
  serialNumber String Maybe
  replacementCost Int Maybe
  deriving Show
Fob
  key String
  UniqueKey key
  deriving Show
FobAssignment
  personId PersonId
  fobId FobId
  startDate Day
  expirationDate Day Maybe
  AssignmentByFobId fobId
  deriving Show
Membership
  personId PersonId
  active Bool
  MemberId personId
  deriving Show
Person
  firstName String
  lastName String
  deriving Show
PossessionContract
  personId PersonId
  equipmentId EquipmentId
  paymentCents Int Maybe
  startDate Day
  expirationDate Day Maybe
  deriving Show
Agreement
  name String
  author String
  version Int
  deriving Show
Door
  name String
  hardwareAddress String
  UniqueAddress hardwareAddress
  deriving Show
DoorKey
  doorId DoorId
  personId PersonId
  startDate Day
  expirationDate Day Maybe
  deriving Show
|]

