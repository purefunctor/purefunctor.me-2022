module Website.App where

import Control.Monad ( void )

import Database.Persist.Sqlite

import           Network.Wai.Handler.Warp ( Port )
import qualified Network.Wai.Handler.Warp as Warp

import Servant
import Servant.Auth.Server

import Website.API.Auth
import Website.API.Blog
import Website.API.Repo
import Website.Config
import Website.Models
import Website.WebsiteM


type WebsiteAPI = LoginAPI :<|> BlogPostAPI :<|> RepositoryAPI


websiteServer :: CookieSettings -> JWTSettings -> ServerT WebsiteAPI WebsiteM
websiteServer cookieSettings jwtSettings =
  loginServer cookieSettings jwtSettings :<|> blogPostServer :<|> repositoryServer


websiteApp :: JWTSettings -> Configuration -> Application
websiteApp jwtSettings config =
  serveWithContext api ctx $ hoistServerWithContext api ctx' unwrap server
  where
    api :: Proxy WebsiteAPI
    api = Proxy

    ctx :: Context '[CookieSettings, JWTSettings]
    ctx = defaultCookieSettings :. jwtSettings :. EmptyContext

    ctx' :: Proxy '[CookieSettings, JWTSettings]
    ctx' = Proxy

    unwrap :: WebsiteM r -> Handler r
    unwrap = runWebsiteM config

    server :: ServerT WebsiteAPI WebsiteM
    server = websiteServer defaultCookieSettings jwtSettings


run :: Port -> IO ()
run port = do
  config <- mkConfiguration
  jwtSettings <- defaultJWTSettings <$> generateKey
  runSqlPool (runMigration migrateAll) (connPool config)
  void $ Warp.run port (websiteApp jwtSettings config)
