{-# LANGUAGE GADTs #-}
module Database.Associations
  ( AssociationLoader
  , loadAssociations
  , own
  , belongsTos
  , hasManys
  , through
  , throughMany
  ) where

import Control.Applicative
import Data.List (foldl', nub)
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

instance Functor m => Functor (AssociationLoader m ent) where
  fmap f loader = AssociationLoader $ \entities ->
                    map f <$> runLoader loader entities

instance Applicative m => Applicative (AssociationLoader m ent) where
  pure a = AssociationLoader $ const (pure $ repeat a)
  fLoader <*> argLoader = AssociationLoader $ \entities ->
                                zipWith ($)
                            <$> runLoader fLoader entities
                            <*> runLoader argLoader entities

------------------------------------------------------------
own :: Applicative m => (ent -> a) -> AssociationLoader m ent a
own f = AssociationLoader (pure . map f)


------------------------------------------------------------
belongsTos :: (PersistEntity foreignEnt,
               PersistMonadBackend m ~ PersistEntityBackend foreignEnt,
               PersistQuery m)
           => EntityField foreignEnt (Key foreignEnt)
           -> (a -> Key foreignEnt)
           -> AssociationLoader m (Entity a) (Entity foreignEnt)
belongsTos keyField foreignKeyField = AssociationLoader $ \entities -> do
  let foreignKeys = map (foreignKeyField . entityVal) entities

  foreigns <- selectMap [keyField <-. nub foreignKeys] []

  let missingError = error "Missing foreign entity! Your DB should be enforcing foreign keys"
      findForeign fId = case Map.lookup fId foreigns of
                        Just ent -> Entity fId ent
                        _ -> missingError

  return $ map findForeign foreignKeys


------------------------------------------------------------
hasManys :: (PersistEntity foreignEnt,
             PersistMonadBackend m ~ PersistEntityBackend foreignEnt,
             PersistQuery m)
         => EntityField foreignEnt (Key ent)
         -> (foreignEnt -> Key ent)
         -> AssociationLoader m (Entity ent) [Entity foreignEnt]
hasManys keyField keyFunc = AssociationLoader $ \entities -> do
  let keys = map entityKey entities
      prepend ent _ ents = ent : ents
      ownerId ent@(Entity _ val) = (keyFunc val, [ent])

  foreigns <- selectMapBy ownerId
                          prepend
                          [keyField <-. nub keys]
                          []

  let findForeign pId = Map.findWithDefault [] pId foreigns

  return $ map findForeign keys


------------------------------------------------------------
through :: Monad m =>
           AssociationLoader m join foreignEnt
        -> AssociationLoader m ent join
        -> AssociationLoader m ent foreignEnt
through foreignLoader joinLoader =
  AssociationLoader $ \entities -> do
    joinEnts <- runLoader joinLoader entities
    runLoader foreignLoader joinEnts

------------------------------------------------------------
throughMany :: Monad m =>
               AssociationLoader m join foreignEnt
            -> AssociationLoader m ent [join]
            -> AssociationLoader m ent [foreignEnt]
throughMany foreignLoader joinLoader =
    AssociationLoader $ \entities -> do
      joinEnts <- runLoader joinLoader entities
      foreignEnts <- runLoader foreignLoader (concat joinEnts)
      return (recollect joinEnts foreignEnts)
  where
    -- recollect the flattened entities back into
    -- the lists of many relationships.
    recollect :: [[a]] -> [b] -> [[b]]
    recollect [] _ = []
    recollect (as:moreAs) bs = let n = length as
                                   (these, rest) = splitAt n bs
                               in these : recollect moreAs rest

------------------------------------------------------------
selectMap :: (PersistEntity val, PersistQuery m, PersistEntityBackend val ~ PersistMonadBackend m)
          => [Filter val]
          -> [SelectOpt val]
          -> m (Map.Map (Key val) val)
selectMap = selectMapBy keyVal firstIn
  where keyVal (Entity key val) = (key, val)
        firstIn _ _ val = val


selectMapBy :: (PersistEntity val,
                PersistQuery m,
                PersistEntityBackend val ~ PersistMonadBackend m,
                Ord a)
            => (Entity val -> (a,b))
            -> (Entity val -> b -> b -> b)
            -> [Filter val]
            -> [SelectOpt val]
            -> m (Map.Map a b)
selectMapBy extract resolve filter opts =
     do list <- selectList filter opts
        return (makeMap list)

  where makeMap = foldl' insertEnt Map.empty
        insertEnt map ent = let (k,v) = extract ent
                            in Map.insertWith (resolve ent)
                                              k
                                              v
                                              map

