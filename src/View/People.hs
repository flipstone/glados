{-# LANGUAGE QuasiQuotes #-}
module View.People where

import Model
import View.Helpers
import View.Layout

peopleListView :: [Entity Person] -> Html
peopleListView people = layout [shamlet|
  <a href="/people/new">Add a Person</a>
  <ul>
    $forall Entity key p <- people
      <li>
        #{personFirstName p}
        #{personLastName p}

        <a href="/people/edit/#{key}">Edit</a>
  |]

peopleNewView :: View Text -> Html
peopleNewView view = layout [shamlet|
  <form action="/people" method="POST">
    ^{personFields view}

    <input type="submit" value="save">
  |]

peopleEditView :: Entity Person -> View Text -> Html
peopleEditView (Entity id _) view = layout [shamlet|
  <form action="/people/#{id}" method="POST">
    ^{personFields view}

    <input type="submit" value="save">
  |]

personFields :: View Text -> Html
personFields view = [shamlet|
  ^{textField "firstName" "First Name" view}
  ^{textField "lastName" "Last Name" view}
  |]
