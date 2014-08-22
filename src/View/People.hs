{-# LANGUAGE QuasiQuotes        #-}
module View.People where

import Model
import View.Helpers

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

peopleNewView :: View Text -> Html
peopleNewView view = [shamlet|
  <html>
    <body>
      <form action="/people" method="POST">
        ^{personFields view}

        <input type="submit" value="save">
  |]

peopleEditView :: Entity Person -> View Text -> Html
peopleEditView (Entity id _) view = [shamlet|
  <html>
    <body>
      <form action="/people/#{id}" method="POST">
        ^{personFields view}

        <input type="submit" value="save">
  |]

personFields :: View Text -> Html
personFields view = [shamlet|
  ^{textField "firstName" "First Name" view}
  ^{textField "lastName" "Last Name" view}
  |]
