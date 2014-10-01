{-# LANGUAGE QuasiQuotes #-}
module Handler.Style
  ( style
  ) where

import qualified Data.Text.Lazy.Encoding as E
import Text.Cassius

import Handler.Helpers


style :: App Response
style = dir "main.css" $ do
  methodM GET
  ok $ toResponse (styleView Nothing)

instance ToMessage Css where
  toContentType _ = "text/css"
  toMessage = E.encodeUtf8 . renderCss

styleView :: a -> Css
styleView = [cassius|
html
  background-color: #1F272A
  height: 100%

body
  margin: 0 auto 0 auto
  width: 966px
  background-color: #D6CEBC
  min-height: 100%

.container
  margin: 20px

a
  color: #0958D8
  text-decoration: none

a:hover
  color: #1AA7FF

.navbar
  background-color: #1F272A
  padding: 10px

.navbar
  a
    color: #F5D55C
    text-decoration: none
    font-size: large
    margin: 10px
  a:hover
    color: #F8F0B2

dt
  font-weight: bold
  margin-top: 10px

dd
  margin-left: 20px

|]

