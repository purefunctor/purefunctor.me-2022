module Website.Server.API
  ( module Website.Server.API.Auth
  , module Website.Server.API.Blog
  , module Website.Server.API.Repo
  , API
  , apiServer
  ) where

import Servant
import Servant.Auth.Server

import Website.Server.API.Auth
import Website.Server.API.Blog
import Website.Server.API.Repo
import Website.Types


type API = "api" :> ( LoginAPI :<|> BlogPostAPI :<|> RepositoryAPI )


apiServer :: CookieSettings -> JWTSettings -> ServerT API WebsiteM
apiServer cookieSettings jwtSettings =
  loginServer cookieSettings jwtSettings :<|> blogPostServer :<|> repositoryServer
