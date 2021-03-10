module Website.App where

import Control.Lens hiding ( Context )

import Data.List ( foldl' )

import Data.Text ( unpack )

import Database.Persist.Sqlite

import           Network.Wai ( Middleware )
import           Network.Wai.Handler.Warp ( Port )
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.Static ( addBase, staticPolicy )

import Servant
import Servant.Auth.Server

import Website.API
import Website.Config
import Website.Models
import Website.PseudoSSR
import Website.Tasks
import Website.Types


type Website = PseudoSSR :<|> API


websiteServer :: CookieSettings -> JWTSettings -> ServerT Website WebsiteM
websiteServer cookieSettings jwtSettings =
  ssrServer :<|> apiServer cookieSettings jwtSettings


websiteApp :: JWTSettings -> Environment -> Application
websiteApp jwtSettings env =
  serveWithContext api ctx $ hoistServerWithContext api ctx' unwrap server
  where
    api :: Proxy Website
    api = Proxy

    ctx :: Context '[CookieSettings, JWTSettings]
    ctx = defaultCookieSettings :. jwtSettings :. EmptyContext

    ctx' :: Proxy '[CookieSettings, JWTSettings]
    ctx' = Proxy

    unwrap :: WebsiteM r -> Handler r
    unwrap = runWebsiteM env

    server :: ServerT Website WebsiteM
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
      [ ( serveStatic_, staticBase_ ) ]
      where
        serveStatic_ :: Bool
        serveStatic_ = env^.config.debug.serveStatic

        staticBase_ :: Middleware
        staticBase_ = staticPolicy . addBase . unpack $
          env^.config.debug.staticBase
