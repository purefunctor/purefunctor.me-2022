{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Website.API.Auth where


import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Database.Persist.Sqlite (createSqlitePool)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth
import Servant.Auth.Server
import Website.Config
import Website.WebsiteM


type CookieAuthResult =
  ( Headers '[ Header "Set-Cookie" SetCookie
             , Header "Set-Cookie" SetCookie
             ]
    NoContent
  )


data LoginPayload = LoginPayload
  { username :: Text
  , password :: Text
  } deriving (Generic, FromJSON, ToJSON, FromJWT, ToJWT)


type LoginAPI =
  "login" :> ReqBody '[JSON] LoginPayload :> Verb 'POST 204 '[JSON] CookieAuthResult


login :: CookieSettings -> JWTSettings -> ServerT LoginAPI WebsiteM
login cookieSettings jwtSettings = verify
  where
    verify :: LoginPayload -> WebsiteM CookieAuthResult
    verify payload@(LoginPayload username password) = do
      config <- ask

      let cookieSettings' = cookieSettings { cookieSameSite = SameSiteStrict }

      if adminUser config == username && adminPass config == password
        then do
          mApplyCookies <- liftIO $ acceptLogin cookieSettings' jwtSettings payload
          case mApplyCookies of
             Just applyCookies -> return $ applyCookies NoContent
             Nothing -> throwError err401
        else
          throwError err401


type DebugProtectedAPI = "protected" :> Get '[JSON] Text


debugProtected :: AuthResult LoginPayload -> ServerT DebugProtectedAPI WebsiteM
debugProtected (Authenticated payload) = return "Success!"
debugProtected _ = throwError err401


type DebugServerAPI = (Auth '[JWT, Cookie] LoginPayload :> DebugProtectedAPI) :<|> LoginAPI


debugServer :: CookieSettings -> JWTSettings -> ServerT DebugServerAPI WebsiteM
debugServer cookieSettings jwtSettings = debugProtected :<|> login cookieSettings jwtSettings


debugAuth :: IO ()
debugAuth = do
  pool <- runStderrLoggingT $ createSqlitePool "database.sqlite" 1

  jwtSettings <- defaultJWTSettings <$> generateKey

  (Right token) <- makeJWT (LoginPayload "pure" "pure") jwtSettings Nothing

  print token

  let api    = Proxy :: Proxy DebugServerAPI

  let ctx    = defaultCookieSettings :. jwtSettings :. EmptyContext
  let ctx'   = Proxy :: Proxy '[CookieSettings, JWTSettings]

  let config = Configuration "pure" "pure" pool
  let server = debugServer defaultCookieSettings jwtSettings

  let app    = serveWithContext api ctx $ hoistServerWithContext api ctx' (runWebsiteM config) server

  void $ run 3000 app
