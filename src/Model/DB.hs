{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module Model.DB where

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Equipment
  make String
  model String
  serialNumber String Maybe
  replacementCost Int Maybe
  deriving Show
Person
  firstName String
  lastName String
  deriving Show
|]

