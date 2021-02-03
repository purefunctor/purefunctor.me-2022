module Website.Debug where

import Control.Monad ( forM_, unless, void )
import Control.Monad.Logger ( runStderrLoggingT )

import Data.Time ( getCurrentTime )

import Database.Persist.Sqlite

import Servant.Auth.Server

import           Network.Wai ( Application )
import qualified Network.Wai.Handler.Warp as Warp

import Website.App
import Website.Config
import Website.Models


mkDebug :: [BlogPost] -> [Repository] -> IO (Configuration, Application)
mkDebug posts repos = do
  pool <- runStderrLoggingT $ createSqlitePool "debug.sqlite" 1
  jwk <- generateKey

  let userpass = "pure"
  let jwtSettings = defaultJWTSettings jwk
  let config = Configuration userpass userpass pool
  let app = websiteApp jwtSettings config

  flip runSqlPool pool $ do
    runMigration migrateAll

    forM_ posts $ \post -> do
      inDB <- exists [ BlogPostShortTitle ==. blogPostShortTitle post ]
      unless inDB $ void $ insert post

    forM_ repos $ \repo -> do
      inDB <- exists [ RepositoryName ==. repositoryName repo ]
      unless inDB $ void $ insert repo

  return (config, app)


debug_ :: IO (Configuration, Application)
debug_ = do
  now <- getCurrentTime

  let posts =
        [ BlogPost "Haskell Is Simple" "haskell-is-simple" "SOON™" now now
        , BlogPost "Python Is Awesome" "python-is-awesome" "SOON™" now now
        ]

  let repos =
        [ Repository "amalgam-lisp" "PureFunctor" "https://github.com/PureFunctor/amalgam-lisp" 0 0
        , Repository "purefunctor.me" "PureFunctor" "https://github.com/PureFunctor/purefunctor.me" 0 0
        ]

  mkDebug posts repos


debug :: IO ()
debug = Warp.run 3000 . snd =<< debug_
