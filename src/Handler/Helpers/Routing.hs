{-# LANGUAGE GADTs #-}
module Handler.Helpers.Routing where

import qualified Data.Text as T

import Database.Persist.Postgresql
import Happstack.Server

import App.Types


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

entityKeyURI :: KeyBackend backend entity -> T.Text
entityKeyURI (Key (PersistInt64 id)) = T.pack (show id)
entityKeyURI _ = error "Tried to encode non-integer db value as uri key"

