{-# LANGUAGE QuasiQuotes #-}
module View.Agreement where

import Model
import View.Helpers
import View.Layout

agreementListView :: [Entity Agreement] -> Html
agreementListView agreements = layout [shamlet|
  <a href="/agreements/new">Add Agreement
  <ul>
    $forall Entity key agreement <- agreements
      <li>
        #{agreementName agreement}
        #{agreementAuthor agreement}
        #{agreementVersion agreement}

        <a href="/agreements/#{key}/edit">Edit
  |]

agreementNewView :: View Text -> Html
agreementNewView view = layout [shamlet|
  <form action="/agreements" method="POST">
    ^{agreementFields view}

    <input type="submit" value="save">
  |]

agreementEditView :: Entity Agreement -> View Text -> Html
agreementEditView (Entity id _) view = layout [shamlet|
  <form action="/agreements/#{id}" method="POST">
    ^{agreementFields view}

    <input type="submit" value="save">
  |]

agreementFields :: View Text -> Html
agreementFields view = [shamlet|
  ^{textField "name" "Name" view}
  ^{textField "author" "Author" view}
  ^{textField "version" "Version" view}
  |]
