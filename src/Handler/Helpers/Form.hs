module Handler.Helpers.Form where

import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Text.Digestive

foreignKey :: (Monad m, SelectOption a)
           => [Entity a]
           -> Formlet Text m (Key a)
foreignKey entities = choiceWith options
  where options = map toOption entities
        toOption (Entity key val) = (keyText key, (key, toOptionText val))
        keyText (Key (PersistInt64 k)) = T.pack (show k)

class SelectOption a where
  toOptionText :: a -> Text

