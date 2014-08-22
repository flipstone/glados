{-# LANGUAGE GADTs #-}
module Handler.Helpers.Routing where

import Database.Persist.Postgresql
import Happstack.Server

import App.Types

(>>.) :: Monad m => m c -> (a -> m b) -> (a -> m b)
m1 >>. f = \a -> m1 >> f a

entityId :: ( PersistEntity entity,
              PersistEntityBackend entity ~ SqlBackend)
            => (Entity entity -> App Response) -> App Response
entityId action = path $ \id -> do
  result <- runDB $ get id

  case result of
    Just ent -> action (Entity id ent)
    Nothing -> notFound $ toResponse ("Not found"::String)

instance FromReqURI (KeyBackend backend entity) where
  fromReqURI = fmap (Key . PersistInt64) . fromReqURI

