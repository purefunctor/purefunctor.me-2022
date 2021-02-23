module Website.App where

import Control.Lens hiding ( Context )

import Data.List ( foldl' )

import Database.Persist.Sqlite

import           Network.Wai ( Middleware )
import           Network.Wai.Handler.Warp ( Port )
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.Static ((<|>), addBase,  Policy, policy, staticPolicy)

import Servant
import Servant.Auth.Server

import Website.API.Auth
import Website.API.Blog
import Website.API.Repo
import Website.Config
import Website.Models
import Website.Tasks
import Website.WebsiteM


type WebsiteAPI = "api" :> ( LoginAPI :<|> BlogPostAPI :<|> RepositoryAPI )


websiteServer :: CookieSettings -> JWTSettings -> ServerT WebsiteAPI WebsiteM
websiteServer cookieSettings jwtSettings =
  loginServer cookieSettings jwtSettings :<|> blogPostServer :<|> repositoryServer


websiteApp :: JWTSettings -> Environment -> Application
websiteApp jwtSettings env =
  serveWithContext api ctx $ hoistServerWithContext api ctx' unwrap server
  where
    api :: Proxy WebsiteAPI
    api = Proxy

    ctx :: Context '[CookieSettings, JWTSettings]
    ctx = defaultCookieSettings :. jwtSettings :. EmptyContext

    ctx' :: Proxy '[CookieSettings, JWTSettings]
    ctx' = Proxy

    unwrap :: WebsiteM r -> Handler r
    unwrap = runWebsiteM env

    server :: ServerT WebsiteAPI WebsiteM
    server = websiteServer defaultCookieSettings jwtSettings


run :: Port -> IO ()
run port = do
  env <- mkEnvironment
  jwtSettings <- defaultJWTSettings <$> generateKey

  runSqlPool (runMigration migrateAll) (env^.pool)

  let vanillaApp = websiteApp jwtSettings env

  _ <- runTasks env

  Warp.run port $ applyMiddleware env vanillaApp


applyMiddleware :: Environment -> Middleware
applyMiddleware env = middleware
  where
    composeMiddleware ::  [Middleware] -> Middleware
    composeMiddleware = foldl' (.) id

    filterEnabled :: [(Bool, Middleware)] -> [Middleware]
    filterEnabled = fmap snd . filter fst

    middleware :: Middleware
    middleware = composeMiddleware . filterEnabled $
      [ (env^.config.debug.static, staticPolicy serveStaticPolicy)
      ]


serveStaticPolicy :: Policy
serveStaticPolicy = root <|> misc
  where
    root :: Policy
    root = policy $ \uri ->
      if uri == ""
      then Just "dist/index.html"
      else Nothing

    misc :: Policy
    misc = addBase "dist"
