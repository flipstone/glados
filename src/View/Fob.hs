{-# LANGUAGE QuasiQuotes #-}
module View.Fob where

import Model
import View.Helpers
import View.Layout

fobListView :: [Entity Fob] -> Html
fobListView fob = layout [shamlet|
  <a href="/fob/new">Add Fob</a>
  <ul>
    $forall Entity key f <-fob
      <li>
        #{fobKey f}

        <a href="/fob/edit/#{key}">Edit</a>
|]

fobNewView :: View Text -> Html
fobNewView view = layout [shamlet|
  <form action="/fob" method="POST">
    ^{fobFields view}

    <input type="submit" value="save">
|]

fobEditView :: Entity Fob -> View Text ->Html
fobEditView (Entity id _) view = layout [shamlet|
  <form action="/fob/#{id}" method="POST">
    ^{fobFields view}

    <input type="submit" value="save">
|]

fobFields :: View Text -> Html
fobFields view = [shamlet|
  ^{textField "key" "Key" view}
|]