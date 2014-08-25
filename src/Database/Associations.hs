{-# LANGUAGE GADTs #-}
module Database.Associations
  ( AssociationLoader
  , loadAssociations
  , theEntity
  , belongsTos
  ) where

import Control.Applicative
import qualified Data.Map.Strict as Map
import Database.Persist

------------------------------------------------------------
loadAssociations :: (Monad m)
                 => [Entity ent]
                 -> AssociationLoader m ent associatedEnt
                 -> m [(Key ent, associatedEnt)]
loadAssociations entities loader = do
    associated <- runLoader loader (map entityVal entities)
    return $ zipWith pairUp entities associated
  where pairUp (Entity key _) v = (key, v)


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
theEntity :: Monad m => AssociationLoader m ent ent
theEntity = AssociationLoader return


------------------------------------------------------------
belongsTos :: (PersistEntity foreignEnt,
               PersistMonadBackend m ~ PersistEntityBackend foreignEnt,
               PersistQuery m)
           => EntityField foreignEnt (Key foreignEnt)
           -> (a -> Key foreignEnt)
           -> AssociationLoader m a foreignEnt
belongsTos keyField foreignKeyField = AssociationLoader $ \entities -> do
  let foreignKeys = map foreignKeyField entities

  foreigns <- selectMap [keyField <-. foreignKeys] []

  let missingError = error "Missing foreign entity! Your DB should be enforcing foreign keys"
      findForeign pId = case Map.lookup pId foreigns of
                        Just ent -> ent
                        _ -> missingError

      resolve = findForeign . foreignKeyField

  return $ map resolve entities


------------------------------------------------------------
selectMap :: (PersistEntity val, PersistQuery m, PersistEntityBackend val ~ PersistMonadBackend m)
          => [Filter val]
          -> [SelectOpt val]
          -> m (Map.Map (Key val) val)
selectMap f o = do list <- selectList f o
                   return (makeMap list)
  where makeMap = Map.fromList . map toPair
        toPair (Entity key val) = (key, val)


