{-# LANGUAGE GADTs #-}
module Database.Associations
  ( AssociationLoader
  , loadAssociations
  , own
  , belongsTos
  ) where

import Control.Applicative
import qualified Data.Map.Strict as Map
import Database.Persist

------------------------------------------------------------
loadAssociations :: (Monad m)
                 => [ent]
                 -> AssociationLoader m ent associatedEnt
                 -> m [associatedEnt]
loadAssociations = flip runLoader

------------------------------------------------------------
data AssociationLoader m ent foreignEnt = AssociationLoader {
    runLoader :: [ent] -> m [foreignEnt]
  }

instance Monad m => Functor (AssociationLoader m ent) where
  fmap f loader = AssociationLoader $ \entities -> do
                    foreignEntities <- runLoader loader entities
                    return $ map f foreignEntities

instance Monad m => Applicative (AssociationLoader m ent) where
  pure a = AssociationLoader $ const (return $ repeat a)
  fLoader <*> argLoader = AssociationLoader $ \entities -> do
                            fs <- runLoader fLoader entities
                            args <- runLoader argLoader entities
                            return (zipWith ($) fs args)

------------------------------------------------------------
own :: Monad m => (ent -> a) -> AssociationLoader m ent a
own f = AssociationLoader (return . map f)


------------------------------------------------------------
belongsTos :: (PersistEntity foreignEnt,
               PersistMonadBackend m ~ PersistEntityBackend foreignEnt,
               PersistQuery m)
           => EntityField foreignEnt (Key foreignEnt)
           -> (a -> Key foreignEnt)
           -> AssociationLoader m (Entity a) foreignEnt
belongsTos keyField foreignKeyField = AssociationLoader $ \entities -> do
  let foreignKeys = map (foreignKeyField . entityVal) entities

  foreigns <- selectMap [keyField <-. foreignKeys] []

  let missingError = error "Missing foreign entity! Your DB should be enforcing foreign keys"
      findForeign pId = case Map.lookup pId foreigns of
                        Just ent -> ent
                        _ -> missingError

  return $ map findForeign foreignKeys


------------------------------------------------------------
selectMap :: (PersistEntity val, PersistQuery m, PersistEntityBackend val ~ PersistMonadBackend m)
          => [Filter val]
          -> [SelectOpt val]
          -> m (Map.Map (Key val) val)
selectMap f o = do list <- selectList f o
                   return (makeMap list)
  where makeMap = Map.fromList . map toPair
        toPair (Entity key val) = (key, val)


