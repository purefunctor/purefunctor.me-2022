module Website.Server.API.Auth where

import Control.Lens

import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Reader ( asks )

import Data.Aeson ( FromJSON, ToJSON )

import Data.Text ( Text )

import GHC.Generics ( Generic )

import Servant
import Servant.Auth
import Servant.Auth.Server

import Website.Config
import Website.Types


type CookieAuthResult =
  ( Headers '[ Header "Set-Cookie" SetCookie
             , Header "Set-Cookie" SetCookie
             ]
    NoContent
  )


data LoginPayload
  = LoginPayload
      { username :: Text
      , password :: Text
      }
  deriving (FromJSON, FromJWT, Generic, ToJSON, ToJWT)


type LoginAPI =
  "login" :> ReqBody '[JSON] LoginPayload :> Verb 'POST 204 '[JSON] CookieAuthResult


type RequiresAuth = Auth '[JWT, Cookie] LoginPayload


loginServer :: CookieSettings -> JWTSettings -> ServerT LoginAPI WebsiteM
loginServer cookieSettings jwtSettings = verify
  where
    verify :: LoginPayload -> WebsiteM CookieAuthResult
    verify login = do
      creds <- asks $ view (config . admin)

      let cookieSettings' =
            cookieSettings { cookieSameSite = SameSiteStrict }

      if isAdmin creds login
        then do
          mApplyCookies <- liftIO $ acceptLogin cookieSettings' jwtSettings login
          case mApplyCookies of
             Just applyCookies -> pure $ applyCookies NoContent
             Nothing           -> throwError err401
        else
          throwError err401

    isAdmin :: AdminCreds -> LoginPayload -> Bool
    isAdmin (AdminCreds aUser aPass) (LoginPayload cUser cPass) =
      (aUser, aPass) == (cUser, cPass)
