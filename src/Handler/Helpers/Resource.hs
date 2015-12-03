{-# LANGUAGE GADTs #-}
module Handler.Helpers.Resource where

import Control.Monad (msum)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Postgresql
import Happstack.Server
import Text.Digestive (Form, Formlet, View)
import Text.Hamlet (Html)

import App.Types
import Handler.Helpers.Routing

data ResourceActions a = ResourceActions {
    resActionList :: App Response
  , resActionNew :: App Response
  , resActionEdit :: Entity a -> App Response
  , resActionShow :: Entity a -> App Response
  , resActionCreate :: App Response
  , resActionUpdate :: Entity a -> App Response
  , resActionDelete :: Entity a -> App Response
  }

defaultActions :: ResourceActions a
defaultActions = ResourceActions {
    resActionList = notAvailable
  , resActionNew = notAvailable
  , resActionEdit = const notAvailable
  , resActionShow = const notAvailable
  , resActionCreate = notAvailable
  , resActionUpdate = const notAvailable
  , resActionDelete = const notAvailable
  }
  where
    notAvailable = notFound $ toResponse ("Action not available for this resource"::String)

routeResource :: ( PersistEntity a,
                   PersistEntityBackend a ~ SqlBackend)
              => ResourceActions a -> App Response
routeResource actions = msum [
    methodM GET >> resActionList actions
  , methodM POST >> resActionCreate actions
  , dir "new" $ methodM GET >> resActionNew actions
  , entityId $ \ent ->
      msum [ methodM POST >> resActionUpdate actions ent
           , methodM GET >> resActionShow actions ent
           , methodM DELETE >> resActionDelete actions ent
           , dir "edit" $ methodM GET >> resActionEdit actions ent
           ]
  ]

data Resource a = Resource {
    resCreate :: a -> AppBackend (Key a)
  , resUpdate :: Key a -> a -> AppBackend ()
  , resDelete :: Key a -> AppBackend ()
  , resNewView :: View Text -> Html
  , resEditView :: Entity a -> View Text -> Html
  , resShowView :: Entity a -> View Text -> Html
  , resIndexUri :: Text
  }

defaultResource :: ( PersistEntity a
                   , PersistEntityBackend a ~ SqlBackend)
                => Resource a
defaultResource = Resource {
    resCreate = insert
  , resUpdate = replace
  , resDelete = delete
  , resNewView = const ""
  , resEditView = const (const "")
  , resShowView = const (const "")
  , resIndexUri = "/"
  }


handleCreate :: Resource a
             -> (View Text, Maybe a)
             -> App Response
handleCreate res (view, result) =
  case result of
    Just ent -> do
      runDB $ resCreate res ent
      seeOther (resIndexUri res) $ toResponse ("Look over there"::String)

    Nothing ->
      badRequest $ toResponse $ resNewView res view

handleDelete :: Resource a
            -> Entity a
            -> App Response
handleDelete res oldEnt@(Entity key _) = do
  runDB $ resDelete res key
  seeOther (resIndexUri res) $ toResponse ("Look over there"::String)

handleUpdate :: Resource a
             -> Entity a
             -> (View Text, Maybe a)
             -> App Response
handleUpdate res oldEnt@(Entity key _) (view, result) =
  case result of
    Just newEnt -> do
      runDB $ resUpdate res key newEnt
      seeOther (resIndexUri res) $ toResponse ("Look over there"::String)

    Nothing ->
      badRequest $ toResponse $ resEditView res oldEnt view

