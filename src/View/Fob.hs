{-# LANGUAGE QuasiQuotes #-}
module View.Fob where

import Model
import View.Helpers
import View.Layout

fobListView :: [Entity Fob] -> Html
fobListView fobs = layout [shamlet|
  <a href="/fobs/new">Add Fob
  <ul>
    $forall Entity key fob <-fobs
      <li>
        #{fobKey fob}

        <a href="/fobs/#{key}/edit">Edit
|]

fobNewView :: View Text -> Html
fobNewView view = layout [shamlet|
  <form action="/fobs" method="POST">
    ^{fobFields view}

    <input type="submit" value="save">
|]

fobEditView :: Entity Fob -> View Text ->Html
fobEditView (Entity id _) view = layout [shamlet|
  <form action="/fobs/#{id}" method="POST">
    ^{fobFields view}

    <input type="submit" value="save">
|]

fobFields :: View Text -> Html
fobFields view = [shamlet|
  ^{textField "key" "Key" view}
|]
