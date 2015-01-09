{-# LANGUAGE QuasiQuotes #-}
module View.Memberships where

import Model
import View.Helpers
import View.Layout

membershipsShowView :: Entity Person -> Entity Membership -> Html
membershipsShowView (Entity personId person) (Entity id m) = layout [shamlet|
  ^{membershipHeader person}

  <div>
    $if membershipActive m
      Active
    $else
      Inactive

    \ -

    <a href="/people/#{personId}/membership/#{id}/edit">
      Edit

  <div>
    <a href="/memberships/#{id}/payments">
      Manage Payments

  |]

membershipsNewView :: Entity Person -> View Text -> Html
membershipsNewView (Entity personId person) view = layout [shamlet|
  ^{membershipHeader person}

  <form action="/people/#{personId}/membership" method="POST">
    ^{membershipFields view}

    <input type="submit" value="save">
  |]

membershipsEditView :: Entity Person -> Entity Membership -> View Text -> Html
membershipsEditView (Entity personId person) (Entity id _) view = layout [shamlet|
  ^{membershipHeader person}

  <form action="/people/#{personId}/membership/#{id}" method="POST">
    ^{membershipFields view}

    <input type="submit" value="save">
  |]

membershipHeader :: Person -> Html
membershipHeader person = [shamlet|
  <h1>
    Membership for
    #{personFirstName person}
    #{personLastName person}
  |]

membershipFields :: View Text -> Html
membershipFields view = [shamlet|
  ^{checkboxField "active" "Active" view}
  |]

