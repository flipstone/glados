{-# LANGUAGE QuasiQuotes #-}
module View.Helpers.Form where

import Data.Text (Text)
import Text.Digestive
import Text.Hamlet (shamlet, Html)
import Text.Digestive.Blaze.Html5

textField :: Text -> Html -> View Text -> Html
textField path labelText view = [shamlet|
  <div>
    ^{label path view labelText}
    ^{inputText path view}
    ^{fieldErrors path view}
  |]

dateField :: Text -> Html -> View Text -> Html
dateField path labelText view = [shamlet|
  <div>
    ^{label path view labelText}
    ^{inputText path view} (mm/dd/yyyy)
    ^{fieldErrors path view}
  |]

checkboxField :: Text -> Html -> View Text -> Html
checkboxField path labelText view = [shamlet|
  <div>
    <input type="checkbox"
           id="#{ref}"
           name="#{ref}"
           :sel:checked>
    ^{label path view labelText}
    ^{fieldErrors path view}
  |]
  where ref = absoluteRef path view
        sel = fieldInputBool path view

selectField :: Text -> Html -> View Text -> Html
selectField path labelText view = [shamlet|
    <div>
      ^{label path view labelText}
      <select id=#{ref} name=#{ref}>
        $forall (v,c,sel) <- choices
          <option value=#{v} :sel:selected>#{c}
      ^{fieldErrors path view}
    |]
  where ref = absoluteRef path view
        choices = fieldInputChoice path view

fieldErrors :: Text -> View Text -> Html
fieldErrors path view = [shamlet|
  <ul>
    $forall e <- errors path view
      <li>#{e}
  |]
