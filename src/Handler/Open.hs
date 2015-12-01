{-# LANGUAGE GADTs #-}
module Handler.Open
  ( open
  ) where

import Control.Monad.Trans (liftIO)

import Data.Aeson
import Data.Time.Clock

import Handler.Helpers
import Model

open :: App Response
open = do
  hardwareAddress <- look "doorAddress" <|>
                     openError "doorAddress parameter is required"

  fobKey <- look "fobKey" <|>
            openError "fobKey parameter is required"

  now <- liftIO $ getCurrentTime
  result <- runDB $ tryOpenOn now
                              (UniqueAddress hardwareAddress)
                              (UniqueKey fobKey)

  case result of
    Right Ok -> ok $ openResponse True []
    Left err -> let msgs = [openErrorMessage err]
                in badRequest $ openResponse False msgs

openErrorMessage :: OpenError -> String
openErrorMessage DoorNotFound = "Door not found"
openErrorMessage DoorKeyNotFound = "No key available"
openErrorMessage DoorKeyNotCurrent = "Door key not current"
openErrorMessage FobNotFound = "Fob not found"
openErrorMessage FobNotAssigned = "Fob not assigned"
openErrorMessage FobAssignmentNotCurrent = "Fob assignement is not current"
openErrorMessage DoorKeyInsufficientAccess = "User has insufficient access for requested time"

instance ToMessage Value where
  toContentType _ = "application/json"
  toMessage = encode

openError :: String -> App a
openError msg = do
  setResponseCode 400
  finishWith $ openResponse False [msg]

openResponse :: Bool -> [String] -> Response
openResponse canOpen errors = toResponse $
  object [ "canOpen" .= canOpen
         , "errors" .= errors
         ]

