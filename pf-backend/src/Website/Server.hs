module Website.Server
  ( module Website.Server.API
  , module Website.Server.PseudoSSR
  , FullSite
  , fullSiteServer
  )
  where

import Servant
import Servant.Auth.Server

import Website.Config
import Website.Server.API
import Website.Server.Extra
import Website.Server.PseudoSSR
import Website.Types


type FullSite = PseudoSSR :<|> API :<|> Raw


fullSiteServer
  :: CookieSettings
  -> JWTSettings
  -> Environment
  -> ServerT FullSite WebsiteM
fullSiteServer cookieSettings jwtSettings env = do
  ssrServer :<|> apiServer cookieSettings jwtSettings :<|> custom404 env
