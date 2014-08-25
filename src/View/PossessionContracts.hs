{-# LANGUAGE QuasiQuotes #-}
module View.PossessionContracts where

import Model
import View.Helpers

data PossessionContractView = PossessionContractView {
    possessionContract :: PossessionContract
  , person :: Person
  , equipment :: Equipment
  } deriving Show

possessionContractsListView :: [(Key PossessionContract, PossessionContractView)]
                            -> Html
possessionContractsListView possessionContracts = [shamlet|
  <html>
    <body>
      <a href="/possessionContracts/new">Add a PossessionContract</a>
      <ul>
        $forall (key,p) <- possessionContracts
          <li>
            #{personFirstName $ person p}
            #{personLastName $ person p}'s
            #{equipmentMake $ equipment p}
            #{equipmentModel $ equipment p}

            <a href="/possessionContracts/edit/#{key}">Edit</a>
  |]

possessionContractsNewView :: View Text -> Html
possessionContractsNewView view = [shamlet|
  <html>
    <body>
      <form action="/possessionContracts" method="POST">
        ^{possessionContractFields view}

        <input type="submit" value="save">
  |]

possessionContractsEditView :: Entity PossessionContract -> View Text -> Html
possessionContractsEditView (Entity id _) view = [shamlet|
  <html>
    <body>
      <form action="/possessionContracts/#{id}" method="POST">
        ^{possessionContractFields view}

        <input type="submit" value="save">
  |]

possessionContractFields :: View Text -> Html
possessionContractFields view = [shamlet|
  ^{selectField "personId" "Person" view}
  ^{selectField "equipmentId" "Equipment" view}
  ^{textField "paymentCents" "Payment (Cents)" view}
  ^{textField "startDate" "Start Date" view}
  ^{textField "expirationDate" "Expiration Date" view}
  |]
