module Handler.Welcome
  ( welcome
  ) where

import Handler.Helpers

import View.Welcome

welcome :: App Response
welcome = do
  methodM GET
  ok $ toResponse welcomeView
