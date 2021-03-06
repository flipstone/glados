{-# LANGUAGE QuasiQuotes #-}
module View.DoorKeys where

import Model
import View.Helpers
import View.Layout

data DoorKeyView = DoorKeyView {
    doorKey :: Entity DoorKey
  , door :: Entity Door
  , person :: Entity Person
}

doorKeysListView :: [DoorKeyView] -> Html
doorKeysListView doorKeys = layout [shamlet|
  <a href="/doorKeys/new">Add a Key
  <ul>
    $forall view <- doorKeys
      $with Entity id doorKey <- doorKey view
        <li>
          $with Entity _ person <- person view
            $with Entity _ door <- door view
              #{personFirstName $ person } #{personLastName $ person }
              can open #{doorName $ door} : #{doorKeyKeyAccess doorKey}

          &nbsp;&nbsp;<a href="/doorKeys/#{id}/edit">Edit</a>

  |]

doorKeysNewView :: View Text -> Html
doorKeysNewView view = layout [shamlet|
  <form action="/doorKeys" method="POST">
    ^{doorKeyFields view}
    <input type="submit" value="Save">
  |]

doorKeysEditView :: Entity DoorKey -> View Text -> Html
doorKeysEditView (Entity id _) view = layout [shamlet|
  <form action="/doorKeys/#{id}" method="POST">
    ^{doorKeyFields view}

    <input type="submit" value="save">
  |]

doorKeyFields :: View Text -> Html
doorKeyFields view = do [shamlet|
  ^{selectField "personId" "Person" view}
  ^{selectField "doorId" "Door" view}
  ^{selectField "keyAccess" "Access" view}
  ^{dateField "startDate" "Start Date" view}
  ^{dateField "expirationDate" "Expiration Date" view}
  |]

