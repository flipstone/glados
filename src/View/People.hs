{-# LANGUAGE QuasiQuotes        #-}
module View.People where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Text.Digestive (View)
import Text.Digestive.Blaze.Html5
import Text.Hamlet (shamlet, Html)
import Text.Blaze.Html (ToMarkup, toMarkup)

import Model

peopleListView :: [Entity Person] -> Html
peopleListView people = [shamlet|
  <html>
    <body>
      <a href="/people/new">Add a Person</a>
      <ul>
        $forall Entity key p <- people
          <li>
            #{personFirstName p}
            #{personLastName p}

            <a href="/people/edit/#{key}">Edit</a>
  |]

peopleNewView :: View Html -> Html
peopleNewView view = [shamlet|
  <html>
    <body>
      <form action="/people" method="POST">
        ^{personFields view}

        <input type="submit" value="save">
  |]

peopleEditView :: Entity Person -> View Html -> Html
peopleEditView (Entity id _) view = [shamlet|
  <html>
    <body>
      <form action="/people/#{id}" method="POST">
        ^{personFields view}

        <input type="submit" value="save">
  |]

personFields :: View Html -> Html
personFields view = [shamlet|
  <div>
    ^{label "firstName" view "First Name"}
    ^{inputText "firstName" view}
    ^{errorList "firstName" view}
  <div>
    ^{label "lastName" view "Last Name"}
    ^{inputText "lastName" view}
    ^{errorList "lastName" view}
  |]


instance ToMarkup (KeyBackend backend entity) where
  toMarkup (Key id) = [shamlet|#{id}|]

instance ToMarkup PersistValue where
  toMarkup val = [shamlet|#{render val}|]
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

