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
  <a href="/people/new">Add a Person
  <ul>
    $forall view <- people
      $with Entity key p <- person view
        <li>
          <a href="/people/#{key}">
            #{personFirstName p}
            #{personLastName p}

          <a href="/people/#{key}/edit">
            Edit

          <ul>
            $forall Entity _ e <- equipments view
              <li>
                #{equipmentMake e}
                #{equipmentModel e}
  |]

peopleShowView :: Entity Person
               -> [Entity Application]
               -> Maybe (Entity Membership)
               -> Html
peopleShowView (Entity personId p) applications membership = layout [shamlet|
  <h1>
    #{personFirstName p}
    #{personLastName p}

  <div>
    $case membership
      $of Just (Entity membershipId _)
        <a href="/people/#{personId}/membership/#{membershipId}">
          Manage Membership
      $of Nothing
        <a href="/people/#{personId}/membership/new">
          Add Membership

  <div>
    <a href="/people/#{personId}/applications/new">
      Add Application

    <ul>
      $forall (Entity appId app) <- applications
        <li>
          <a href="/people/#{personId}/applications/#{appId}">
            "#{applicationName app}"

          -

          <a href="/people/#{personId}/applications/#{appId}/edit">
            Edit
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
