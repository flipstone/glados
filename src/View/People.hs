{-# LANGUAGE QuasiQuotes #-}
module View.People where

import Model
import View.Helpers
import View.Layout

data PersonView = PersonView {
    person :: Entity Person
  , equipments :: [Entity Equipment]
  }

peopleListView :: [PersonView] -> Html
peopleListView people = layout [shamlet|
  <a href="/people/new">Add a Person</a>
  <ul>
    $forall view <- people
      $with Entity key p <- person view
        <li>
          #{personFirstName p}
          #{personLastName p}

          <a href="/people/edit/#{key}">Edit</a>

          <ul>
            $forall Entity _ e <- equipments view
              <li>
                #{equipmentMake e}
                #{equipmentModel e}
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
