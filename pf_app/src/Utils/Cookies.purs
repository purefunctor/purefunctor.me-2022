module PF.Utils.Cookies where

import Prelude

import Biscotti.Cookie (getName, getValue, parseMany)
import Data.Either (Either(..))
import Data.List (find)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Text.Parsing.StringParser (ParseError(..))


foreign import _getDocumentCookie :: Effect String


getDocumentCookie :: String
getDocumentCookie = unsafePerformEffect _getDocumentCookie


newtype XsrfToken = XsrfToken String


derive instance eqXsrfToken :: Eq XsrfToken


getXsrfToken :: Either ParseError XsrfToken
getXsrfToken = do
  cookies <- parseMany getDocumentCookie
  case find (\cookie -> getName cookie == "XSRF-TOKEN") cookies of
    Just cookie -> pure <<< XsrfToken <<< getValue $ cookie
    Nothing -> Left $ ParseError "XSRF-TOKEN not found"
