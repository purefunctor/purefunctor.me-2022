module Website.Debug where

import Control.Lens
import Control.Monad ( forM_, unless, void )

import Database.Persist.Sqlite

import Servant.Auth.Server

import Network.Wai ( Application )

import Website.App
import Website.Config
import Website.Models


mkDebug :: [BlogPost] -> [Repository] -> IO (Environment, Application)
mkDebug posts repos = do
  env <- mkEnvironment

  jwk <- generateKey

  let jwtSettings = defaultJWTSettings jwk
  let app = websiteApp jwtSettings env

  flip runSqlPool (env^.pool) $ do
    runMigration migrateAll

    forM_ posts $ \post -> do
      inDB <- exists [ BlogPostShort ==. blogPostShort post ]
      unless inDB $ void $ insert post

    forM_ repos $ \repo -> do
      inDB <- exists [ RepositoryName ==. repositoryName repo ]
      unless inDB $ void $ insert repo

  pure (env, app)
