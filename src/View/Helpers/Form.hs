{-# LANGUAGE QuasiQuotes #-}
module View.Helpers.Form where

import Data.Text (Text)
import Text.Digestive (View)
import Text.Hamlet (shamlet, Html)
import Text.Digestive.Blaze.Html5

textField :: Text -> Html -> View Html -> Html
textField path labelText view = [shamlet|
  <div>
    ^{label path view labelText}
    ^{inputText path view}
    ^{errorList path view}
  |]
