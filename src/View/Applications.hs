{-# LANGUAGE QuasiQuotes #-}
module View.Applications where

import Model
import View.Helpers
import View.Layout

applicationsShowView :: Entity Person -> Entity Application -> Html
applicationsShowView (Entity _ person) (Entity id a) = layout [shamlet|
  ^{applicationHeader person}

  ^{showField "Name" applicationName a}
  ^{showField "Street Address" applicationStreetAddress a}
  ^{showField "City" applicationCity a}
  ^{showField "State" applicationState a}
  ^{showField "Zip" applicationZip a}
  ^{showField "Cell Phone" applicationCellPhone a}
  ^{showField "Work Phone" applicationWorkPhone a}
  ^{showField "Email Address" applicationEmailAddress a}

  |]

applicationsNewView :: Entity Person -> View Text -> Html
applicationsNewView (Entity personId person) view = layout [shamlet|
  ^{applicationHeader person}

  <form action="/people/#{personId}/applications" method="POST">
    ^{applicationFields view}

    <input type="submit" value="save">
  |]

applicationsEditView :: Entity Person -> Entity Application -> View Text -> Html
applicationsEditView (Entity personId person) (Entity id _) view = layout [shamlet|
  ^{applicationHeader person}

  <form action="/people/#{personId}/applications/#{id}" method="POST">
    ^{applicationFields view}

    <input type="submit" value="save">
  |]

applicationHeader :: Person -> Html
applicationHeader person = [shamlet|
  <h1>
    Application for
    #{personFirstName person}
    #{personLastName person}
  |]

applicationFields :: View Text -> Html
applicationFields view = [shamlet|
  ^{textField "name" "Name" view}
  ^{textField "streetAddress" "Street Address" view}
  ^{textField "city" "City" view}
  ^{textField "state" "State" view}
  ^{textField "zip" "Zip" view}
  ^{textField "cellPhone" "Cell Phone" view}
  ^{textField "workPhone" "Work Phone" view}
  ^{textField "emailAddress" "Email Address" view}
  |]
