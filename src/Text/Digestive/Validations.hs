module Text.Digestive.Validations where

import Data.Char (isSpace)
import Data.String
import Text.Digestive

notEmpty :: IsString a => String -> Result a String
notEmpty s = if any (not . isSpace) s
             then Success s
             else Error "must not be empty"

