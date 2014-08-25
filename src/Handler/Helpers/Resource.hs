{-# LANGUAGE GADTs #-}
module Handler.Helpers.Resource where

import Data.Text (Text)
import Database.Persist
import Database.Persist.Postgresql
import Happstack.Server
import Text.Digestive (Form, Formlet, View)
import Text.Hamlet (Html)

import App.Types

data Resource a = Resource {
    resCreate :: a -> AppBackend (Key a)
  , resUpdate :: Key a -> a -> AppBackend ()
  , resNewView :: View Text -> Html
  , resEditView :: Entity a -> View Text -> Html
  , resIndexUri :: Text
  }

defaultResource :: ( PersistEntity a
                   , PersistEntityBackend a ~ SqlBackend)
                => Resource a
defaultResource = Resource {
    resCreate = insert
  , resUpdate = replace
  , resNewView = const ""
  , resEditView = const (const "")
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

