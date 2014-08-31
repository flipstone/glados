{-# LANGUAGE QuasiQuotes #-}
module View.Agreement where

import Model
import View.Helpers
import View.Layout

agreementListView :: [Entity Agreement] -> Html
agreementListView agreement = layout [shamlet|
  <a href="/agreement/new"> Add Agreement </a>
  <ul>
    $forall Entity key e <- agreement
      <li>
        #{agreementName e}
        #{agreementAuthor e}
        #{agreementVersion e}

        <a href="/agreement/edit/#{key}">Edit</a>
  |]

agreementNewView :: View Text -> Html
agreementNewView view = layout [shamlet|
  <form action="/agreement" method="POST">
    ^{agreementFields view}

    <input type="submit" value="save">
  |]

agreementEditView :: Entity Agreement -> View Text -> Html
agreementEditView (Entity id _) view = layout [shamlet|
  <form action="/agreement/#{id}" method="POST">
    ^{agreementFields view}

    <input type="submit" value="save">
  |]

agreementFields :: View Text -> Html
agreementFields view = [shamlet|
  ^{textField "name" "Name" view}
  ^{textField "author" "Author" view}
  ^{textField "version" "Version" view}
  |]
