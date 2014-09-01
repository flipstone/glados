{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module Model.DB where

import Data.Time.Calendar (Day)
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Equipment
  make String
  model String
  serialNumber String Maybe
  replacementCost Int Maybe
  deriving Show
Fob
  key String
  deriving Show
FobAssignment
  personId PersonId
  fobId FobId
  startDate Day
  expirationDate Day Maybe
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
Door
  name String
  deriving Show
DoorKey
  doorId DoorId
  personId PersonId
|]

