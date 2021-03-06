{-# LANGUAGE QuasiQuotes #-}
module View.Helpers.Markup
  ( ToMarkup, toMarkup, showField )
  where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Database.Persist.Types
import Text.Blaze.Html (ToMarkup, toMarkup)
import Text.Hamlet (shamlet, Html)

import Model.FieldTypes

instance ToMarkup a => ToMarkup (Maybe a) where
  toMarkup (Just a) = toMarkup a
  toMarkup _ = ""

instance ToMarkup (KeyBackend backend entity) where
  toMarkup (Key id) = toMarkup id

instance ToMarkup PersistValue where
  toMarkup val = toMarkup (render val)
    where render :: PersistValue -> Text
          render (PersistText t) = t
          render (PersistByteString bs) = E.decodeUtf8 bs
          render (PersistInt64 i) = T.pack $ show i
          render (PersistDouble d) = T.pack $ show d
          render (PersistBool b) = T.pack $ show b
          render (PersistDay d) = T.pack $ show d
          render (PersistTimeOfDay tod) = T.pack $ show tod
          render (PersistUTCTime utc) = T.pack $ show utc
          render PersistNull = T.empty

instance ToMarkup MembershipPlanType where
  toMarkup val = toMarkup (show val)

instance ToMarkup KeyAccess where
  toMarkup val = toMarkup (show val)

showField :: ToMarkup value
          => Text -> (record -> value) -> record -> Html
showField label getter record = [shamlet|
  <div>
    #{label}: #{getter record}
  |]

