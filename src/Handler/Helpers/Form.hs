module Handler.Helpers.Form where

import Data.Time.Calendar (Day)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Text.Digestive

import Model.FieldTypes

foreignKey :: (Monad m, SelectOption a)
           => [Entity a]
           -> Formlet Text m (Key a)
foreignKey entities = choiceWith options
  where options = map toOption entities
        toOption (Entity key val) = (keyText key, (key, toOptionText val))
        keyText (Key (PersistInt64 k)) = T.pack (show k)

class SelectOption a where
  toOptionText :: a -> Text


dateFormat :: String
dateFormat = "%m/%d/%Y"

dateField :: Monad m => Formlet Text m Day
dateField = dateFormlet "%m/%d/%Y"

moneyField :: Monad m => Formlet Text m Money
moneyField = stringRead "Invalid Money format"

optionalDateField :: Monad m => Maybe Day -> Form Text m (Maybe Day)
optionalDateField = optionalDateFormlet "%m/%d/%Y"

