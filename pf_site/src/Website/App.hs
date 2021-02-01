module Website.App where

import Control.Monad ( void )
import Control.Monad.Logger ( runStderrLoggingT )

import Data.Time ( getCurrentTime )

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


type WebsiteAPI = BlogPostAPI :<|> RepositoryAPI


websiteServer :: ServerT WebsiteAPI WebsiteM
websiteServer = blogPostServer :<|> repositoryServer


websiteApp :: JWTSettings -> Configuration -> Application
websiteApp jwtSettings config =
  serveWithContext api ctx $ hoistServerWithContext api ctx' (runWebsiteM config) websiteServer
  where
    api :: Proxy WebsiteAPI
    api = Proxy

    ctx :: Context '[CookieSettings, JWTSettings]
    ctx = defaultCookieSettings :. jwtSettings :. EmptyContext

    ctx' :: Proxy '[CookieSettings, JWTSettings]
    ctx' = Proxy


run :: Port -> IO ()
run port = do
  config <- mkConfiguration
  jwtSettings <- defaultJWTSettings <$> generateKey
  runSqlPool (runMigration migrateAll) (connPool config)
  void $ Warp.run port (websiteApp jwtSettings config)


debug_ :: IO (Configuration, Application)
debug_ = do
  pool <- runStderrLoggingT $ createSqlitePool "debug.sqlite" 1
  jwk <- generateKey

  let (user, pass) = ("pure", "pure")
  let jwtSettings = defaultJWTSettings jwk
  let config = Configuration user pass pool
  let app = websiteApp jwtSettings config

  now <- getCurrentTime

  flip runSqlPool pool $ do
    runMigration migrateAll
    insert $ BlogPost "Haskell Is Simple" "haskell-is-simple" "SOON™" now now
    insert $ BlogPost "Python Is Awesome" "python-is-awesome" "SOON™" now now
    insert $ Repository "amalgam-lisp" "PureFunctor" "https://github.com/PureFunctor/amalgam-lisp" 0 0
    insert $ Repository "purefunctor.me" "PureFunctor" "https://github.com/PureFunctor/purefunctor.me" 0 0

  (Right jwt) <- makeJWT (LoginPayload user pass) jwtSettings Nothing

  putStrLn $ "Authorization: Bearer " <> filter (/= '"') (show jwt)

  return (config, app)


debug :: IO ()
debug = Warp.run 3000 . snd =<< debug_
