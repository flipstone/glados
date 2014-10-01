module Model.Open where

import Control.Applicative
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Data.Maybe
import Data.Time.Calendar

import Database.Persist

import App.Types

import Model.DB

data Ok = Ok
type OpenResult = Either OpenError Ok

data OpenError = DoorNotFound
               | DoorKeyNotFound
               | DoorKeyNotCurrent
               | FobNotFound
               | FobNotAssigned
               | FobAssignmentNotCurrent

type Opening = ExceptT OpenError AppBackend

required :: OpenError -> Opening (Maybe a) -> Opening a
required e opening = do
  value <- opening
  case value of
    Just a -> return a
    _ -> throwE e

tryOpenOn :: Day
          -> Unique Door
          -> Unique Fob
          -> AppBackend OpenResult
tryOpenOn today doorKey fobKey = runExceptT $ do
  door <- findDoor doorKey
  fob <- findFob fobKey
  personId <- findPersonId today fob

  checkDoorKey today (entityKey door) personId

findDoor :: Unique Door -> Opening (Entity Door)
findDoor doorKey = required DoorNotFound <$>
                   lift $ getBy doorKey

findFob :: Unique Fob -> Opening (Entity Fob)
findFob fobKey = required FobNotFound <$>
                 lift $ getBy fobKey

findPersonId :: Day -> Entity Fob -> Opening PersonId
findPersonId today fob = do
  assignment <- entityVal <$>
                (required FobNotAssigned <$>
                lift $ getBy (AssignmentByFobId $ entityKey fob))

  if isAssignmentCurrent today assignment then
    return $ fobAssignmentPersonId assignment
  else
    throwE FobAssignmentNotCurrent

checkDoorKey :: Day -> DoorId -> PersonId -> Opening Ok
checkDoorKey today doorId personId = do
  doorKeys <- map entityVal <$>
                selectList [ DoorKeyDoorId ==. doorId
                           , DoorKeyPersonId ==. personId
                           ]
                           []

  if null doorKeys then
    throwE DoorKeyNotFound
  else if any (isDoorKeyCurrent today) doorKeys then
    return Ok
  else
    throwE DoorKeyNotCurrent

isDoorKeyCurrent :: Day -> DoorKey -> Bool
isDoorKeyCurrent = isCurrent doorKeyStartDate
                             doorKeyExpirationDate

isAssignmentCurrent :: Day -> FobAssignment -> Bool
isAssignmentCurrent = isCurrent fobAssignmentStartDate
                                fobAssignmentExpirationDate

isCurrent :: (a -> Day) -- start field
          -> (a -> Maybe Day) -- expiration field
          -> Day
          -> a
          -> Bool
isCurrent start expire today a =
  if today >= start a then
    case expire a of
      Just e -> today <= e
      Nothing -> True
  else
    False


