module PF.Utils.Cookies where

import Prelude

import Biscotti.Cookie (Cookie, getName, parseMany)
import Data.Either (Either(..))
import Data.List (find)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Text.Parsing.StringParser (ParseError(..))


foreign import _getDocumentCookie :: Effect String


getDocumentCookie :: String
getDocumentCookie = unsafePerformEffect _getDocumentCookie


getXsrfToken :: Either ParseError Cookie
getXsrfToken = do
  cookies <- parseMany getDocumentCookie
  case find (\cookie -> getName cookie == "XSRF-TOKEN") cookies of
    Just cookie -> pure cookie
    Nothing -> Left $ ParseError "XSRF-TOKEN not found"
