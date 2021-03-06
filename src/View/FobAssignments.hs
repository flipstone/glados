{-# LANGUAGE QuasiQuotes #-}
module View.FobAssignments where

import Model
import View.Helpers
import View.Layout

data FobAssignmentView = FobAssignmentView {
    fobAssignmentsKey :: Key FobAssignment
  , person :: Person
  , fob :: Fob
  } deriving Show

fobAssignmentsListView :: [FobAssignmentView]
                       -> Html
fobAssignmentsListView fobAssignments = layout [shamlet|
  <a href="/fobAssignments/new">Add a FobAssignment
  <ul>
    $forall pc <- fobAssignments
      <li>
        #{personFirstName $ person pc}
        #{personLastName $ person pc}'s
        #{fobKey $ fob pc}

        <a href="/fobAssignments/#{fobAssignmentsKey pc}/edit">Edit
  |]

fobAssignmentsNewView :: View Text -> Html
fobAssignmentsNewView view = layout [shamlet|
  <form action="/fobAssignments" method="POST">
    ^{fobAssignmentsFields view}

    <input type="submit" value="save">
  |]

fobAssignmentsEditView :: Entity FobAssignment -> View Text -> Html
fobAssignmentsEditView (Entity id _) view = layout [shamlet|
  <form action="/fobAssignments/#{id}" method="POST">
    ^{fobAssignmentsFields view}

    <input type="submit" value="save">
  |]

fobAssignmentsFields :: View Text -> Html
fobAssignmentsFields view = [shamlet|
  ^{selectField "personId" "Person" view}
  ^{selectField "fobId" "Fob" view}
  ^{dateField "startDate" "Start Date" view}
  ^{dateField "expirationDate" "Expiration Date" view}
  |]
