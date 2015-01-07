{-# LANGUAGE QuasiQuotes #-}
module View.Applications where

import Model
import View.Helpers
import View.Layout

applicationsShowView :: Entity Person -> Entity Application -> Html
applicationsShowView (Entity _ person) (Entity id a) = layout [shamlet|
  ^{applicationHeader person}

  <h2>Contact Information
  ^{showField "Name" applicationName a}
  ^{showField "Street Address" applicationStreetAddress a}
  ^{showField "City" applicationCity a}
  ^{showField "State" applicationState a}
  ^{showField "Zip" applicationZip a}
  ^{showField "Cell Phone" applicationCellPhone a}
  ^{showField "Work Phone" applicationWorkPhone a}
  ^{showField "Email Address" applicationEmailAddress a}

  <h2>Plan Type
  ^{showField "Plan Type" applicationPlanType a}
  ^{showField "Spouse's Name" applicationNameOfSpouse a}

  <h2>How did you hear about us?
  $with source <- applicationReferralSource a
    <ul>
      $if referralSearchEngine source
        <li>Search Engine

      $if referralTwitter source
        <li>Twitter

      $if referralMeetup source
        <li>Meetup

      $if referralWordOfMouth source
        <li>Word of mouth

      $if referralConference source
        <li>Conference or faire

      $if referralOther source
        <li>Other (see below)

  ^{showField "Other Referral" applicationReferralOther a}

  <h2>Interests
  $with int <- applicationInterests a
    <ul>
      $if interest3DPrinting int
        <li>3D Printing

      $if interestMetalworking int
        <li>Metalworking

      $if interestElectronics int
        <li>Electronics / Microcontrollers

      $if interestWoodworking int
        <li>Woodworking

      $if interestCoworking int
        <li>Coworking

      $if interestGaming int
        <li>Tabletop Gaming

      $if interestOther int
        <li>Other

      $if interestMetalworking int
        <li>Metalworking

  ^{showField "Other Interest" applicationInterestsOther a}
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
  <h2>Contact Information
  ^{textField "name" "Name" view}
  ^{textField "streetAddress" "Street Address" view}
  ^{textField "city" "City" view}
  ^{textField "state" "State" view}
  ^{textField "zip" "Zip" view}
  ^{textField "cellPhone" "Cell Phone" view}
  ^{textField "workPhone" "Work Phone" view}
  ^{textField "emailAddress" "Email Address" view}

  <h2>Plan Type
  ^{selectField "planType" "Plan Type" view}
  ^{textField "nameOfSpouse" "Spouse's Name" view}

  <h2>How did you hear about us?
  ^{checkboxField "referralSource.searchEngine" "Search engine" view}
  ^{checkboxField "referralSource.twitter" "Twitter" view}
  ^{checkboxField "referralSource.meetup" "Meetup" view}
  ^{checkboxField "referralSource.wordOfMouth" "Word of mouth" view}
  ^{checkboxField "referralSource.conference" "Conference or faire" view}
  ^{checkboxField "referralSource.other" "Other" view}

  ^{textField "referralOther" "Other Referral" view}

  <h2>Interests
  ^{checkboxField "interests.3dprinting" "3D Printing" view}
  ^{checkboxField "interests.metalworking" "Metalworking" view}
  ^{checkboxField "interests.electronics" "Electronics / Microcontrollers" view}
  ^{checkboxField "interests.woodworking" "Woodworking" view}
  ^{checkboxField "interests.coworking" "Coworking" view}
  ^{checkboxField "interests.gaming" "Tabletop Gaming" view}
  ^{checkboxField "interests.other" "Other" view}

  ^{textField "interestsOther" "Other Interest" view}
  |]
