module Model.Open where

import Control.Applicative
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Data.Maybe
import Data.Time

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
               | DoorKeyInsufficientAccess

type Opening = ExceptT OpenError AppBackend

required :: OpenError -> Opening (Maybe a) -> Opening a
required e opening = do
  value <- opening
  case value of
    Just a -> return a
    _ -> throwE e

tryOpenOn :: UTCTime
          -> Unique Door
          -> Unique Fob
          -> AppBackend OpenResult
tryOpenOn targetTime doorKey fobKey = runExceptT $ do
  let day = utctDay targetTime
  door <- findDoor doorKey
  fob <- findFob fobKey
  personId <- findPersonId day fob

  checkDoorKey targetTime (entityKey door) personId

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

checkDoorKey :: UTCTime
             -> DoorId
             -> PersonId
             -> Opening Ok
checkDoorKey targetTime doorId personId = do
  let today = utctDay targetTime
  doorKeys <- map entityVal <$>
                selectList [ DoorKeyDoorId ==. doorId
                           , DoorKeyPersonId ==. personId
                           ]
                           []

  if null doorKeys then
    throwE DoorKeyNotFound
  else do
    let activeKeys = filter (isDoorKeyCurrent today) doorKeys
    if null activeKeys then
      throwE DoorKeyNotCurrent
    else do
      let accesses = map doorKeyKeyAccess activeKeys
      if any (isAccessibleTime targetTime) accesses then
        return Ok
      else
        throwE DoorKeyInsufficientAccess

isAccessibleTime :: UTCTime
                 -> KeyAccess
                 -> Bool
isAccessibleTime _ Unrestricted = True
isAccessibleTime targetTime _ =
  let diffTime = utctDayTime targetTime
  in  (diffTime >= 54000) && (diffTime <= 82800) -- 10a to 6p EST in UTC

isDoorKeyCurrent :: Day
                 -> DoorKey
                 -> Bool
isDoorKeyCurrent = isCurrent doorKeyStartDate
                             doorKeyExpirationDate

isAssignmentCurrent :: Day
                    -> FobAssignment
                    -> Bool
isAssignmentCurrent = isCurrent fobAssignmentStartDate
                                fobAssignmentExpirationDate

isCurrent :: (a -> Day) -- start field
          -> (a -> Maybe Day) -- expiration field
          -> Day
          -> a
          -> Bool
isCurrent startGetter expireGetter today a =
  if today >= startGetter a then
    case expireGetter a of
      Just e -> today <= e
      Nothing -> True
  else
    False


