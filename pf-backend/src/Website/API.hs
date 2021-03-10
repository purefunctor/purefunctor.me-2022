module Website.API
  ( module Website.API.Auth
  , module Website.API.Blog
  , module Website.API.Repo
  , API
  , apiServer
  ) where

import Servant
import Servant.Auth.Server

import Website.API.Auth
import Website.API.Blog
import Website.API.Repo
import Website.Types


type API = "api" :> ( LoginAPI :<|> BlogPostAPI :<|> RepositoryAPI )


apiServer :: CookieSettings -> JWTSettings -> ServerT API WebsiteM
apiServer cookieSettings jwtSettings =
  loginServer cookieSettings jwtSettings :<|> blogPostServer :<|> repositoryServer
