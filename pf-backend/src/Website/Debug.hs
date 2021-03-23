module Website.Debug where

import Control.Lens
import Control.Monad ( forM_, unless )

import Data.Maybe ( isJust )

import Database.Beam

import Servant.Auth.Server

import Network.Wai ( Application )

import Website.App
import Website.Config
import Website.Database


mkDebug :: [BlogPost] -> [Repository] -> IO (Environment, Application)
mkDebug posts_ repos_ = do
  env <- mkEnvironment

  jwk <- generateKey

  let jwtSettings = defaultJWTSettings jwk
  let app = websiteApp jwtSettings env

  runMigration env

  runBeamDb env $ do
    forM_ posts_ $ \post_ -> do
      inDb <- runBeamDb env $ runSelectReturningOne $
        lookup_ (websiteDb^.posts) (BlogPostSlug $ post_^.pSlug)

      unless (isJust inDb) $
        runInsert $ insert (websiteDb^.posts) $ insertValues [post_]

    forM_ repos_ $ \repo_ -> do
      inDb <- runBeamDb env $ runSelectReturningOne $
        lookup_ (websiteDb^.repos) (RepositoryName $ repo_^.rName)

      unless (isJust inDb) $
        runInsert $ insert (websiteDb^.repos) $ insertValues [repo_]

  pure (env, app)
