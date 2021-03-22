{-# LANGUAGE ImpredicativeTypes #-}
module Website.Database
  ( module Website.Database.Models
  , module Website.Database.Pool
  , runMigration
  , WebsiteDb
  , websiteDb
  , posts
  , repos
  ) where

import Control.Exception

import qualified Data.Text.IO as TIO

import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple

import Website.Database.Models
import Website.Database.Pool

import Paths_purefunctor_me


data WebsiteDb f = WebsiteDb
  { _posts :: f (TableEntity BlogPostT)
  , _repos :: f (TableEntity RepositoryT)
  } deriving (Generic, Database Sqlite)

websiteDb :: DatabaseSettings Sqlite WebsiteDb
websiteDb = defaultDbSettings

WebsiteDb (TableLens posts)
          (TableLens repos) = dbLenses


runMigration :: MonadIO m => ConnPool -> m ()
runMigration = liftIO . flip withConnPool runMigration_
  where
    runMigration_ conn = do
      defaultMigration <- getDataFileName "migration.sql"

      migration <-
        catch (TIO.readFile "migration.sql")
        (\(SomeException _) -> TIO.readFile defaultMigration)

      execute_ conn (Query migration)
