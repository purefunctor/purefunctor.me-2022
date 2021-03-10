module Website.Server 
  ( module Website.Server.API
  , module Website.Server.PseudoSSR
  , FullSite
  , fullSiteServer
  )
  where

import Servant
import Servant.Auth.Server

import Website.Server.API
import Website.Server.PseudoSSR
import Website.Types


type FullSite = PseudoSSR :<|> API


fullSiteServer :: CookieSettings -> JWTSettings -> ServerT FullSite WebsiteM
fullSiteServer cookieSettings jwtSettings =
  ssrServer :<|> apiServer cookieSettings jwtSettings
