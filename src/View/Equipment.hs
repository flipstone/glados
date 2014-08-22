{-# LANGUAGE QuasiQuotes #-}
module View.Equipment where

import Model
import View.Helpers

equipmentListView :: [Entity Equipment] -> Html
equipmentListView equipment = [shamlet|
  <html>
    <body>
      <a href="/equipment/new">Add Equipment</a>
      <ul>
        $forall Entity key e <- equipment
          <li>
            #{equipmentMake e}
            #{equipmentModel e}
            #{equipmentSerialNumber e}
            #{equipmentReplacementCost e}

            <a href="/equipment/edit/#{key}">Edit</a>
  |]

equipmentNewView :: View Text -> Html
equipmentNewView view = [shamlet|
  <html>
    <body>
      <form action="/equipment" method="POST">
        ^{equipmentFields view}

        <input type="submit" value="save">
  |]

equipmentEditView :: Entity Equipment -> View Text -> Html
equipmentEditView (Entity id _) view = [shamlet|
  <html>
    <body>
      <form action="/equipment/#{id}" method="POST">
        ^{equipmentFields view}

        <input type="submit" value="save">
  |]

equipmentFields :: View Text -> Html
equipmentFields view = [shamlet|
  ^{textField "make" "Make" view}
  ^{textField "model" "Model" view}
  ^{textField "serialNumber" "Serial Number" view}
  ^{textField "replacementCost" "Replacement Cost" view}
  |]

