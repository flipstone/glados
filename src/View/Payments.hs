{-# LANGUAGE QuasiQuotes #-}
module View.Payments where

import Model
import View.Helpers
import View.Layout

paymentListView :: Entity Membership -> [Entity Payment] -> Html
paymentListView (Entity membershipId _) payments = layout [shamlet|
  <a href="/memberships/#{membershipId}/payments/new">Add Payment
  <ul>
    $forall Entity key payment <-payments
      <li>
        For
        #{show $ paymentMembershipMonth payment}/#{show $ paymentMembershipYear payment}

        (#{show $ paymentAmount payment} on #{show $ paymentReceivedOn payment})

        <a href="/memberships/#{membershipId}/payments/#{key}/edit">Edit
|]

paymentNewView :: Entity Membership -> View Text -> Html
paymentNewView (Entity membershipId _) view = layout [shamlet|
  <form action="/memberships/#{membershipId}/payments" method="POST">
    ^{paymentFields view}

    <input type="submit" value="save">
|]

paymentEditView :: Entity Membership -> Entity Payment -> View Text ->Html
paymentEditView (Entity membershipId _) (Entity id _) view = layout [shamlet|
  <form action="/memberships/#{membershipId}/payments/#{id}" method="POST">
    ^{paymentFields view}

    <input type="submit" value="save">
|]

paymentFields :: View Text -> Html
paymentFields view = [shamlet|
  ^{dateField "receivedOn" "Received On" view}
  ^{textField "amount" "Amount" view}
  ^{textField "membershipMonth" "Membership Month" view}
  ^{textField "membershipYear" "Membership Year" view}
|]
