module Website.Server 
  ( module Website.Server.API
  , module Website.Server.PseudoSSR
  , Website
  , websiteServer
  )
  where

import Servant
import Servant.Auth.Server

import Website.Server.API
import Website.Server.PseudoSSR
import Website.Types


type Website = PseudoSSR :<|> API


websiteServer :: CookieSettings -> JWTSettings -> ServerT Website WebsiteM
websiteServer cookieSettings jwtSettings =
  ssrServer :<|> apiServer cookieSettings jwtSettings
