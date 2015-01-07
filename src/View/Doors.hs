{-# LANGUAGE QuasiQuotes #-}
module View.Doors where

import Model
import View.Helpers
import View.Layout

data DoorView = DoorView {
  door :: Entity Door
}

doorsListView :: [DoorView] -> Html
doorsListView doors = layout [shamlet|
  <a href="/doors/new">Add a Door
  <ul>
    $forall view <- doors
      $with Entity key d <- door view
        <li>
          #{doorName d}

          <a href="/doors/#{key}/edit">Edit
|]

doorsNewView :: View Text -> Html
doorsNewView view = layout [shamlet|
  <form action="/doors" method="POST">
    ^{doorsFields view}

    <input type="submit" value="save">
|]

doorsEditView :: Entity Door -> View Text -> Html
doorsEditView (Entity id _) view = layout [shamlet|
  <form action="/doors/#{id}" method="POST">
    ^{doorsFields view}

    <input type="submit" value="save">
|]

doorsFields :: View Text -> Html
doorsFields view = [shamlet|
  ^{textField "name" "Door Name" view}
  ^{textField "hardwareAddress" "Hardware Address" view}
|]
