{-# LANGUAGE ImpredicativeTypes #-}
module Website.Database
  ( module Website.Database.Models
  , WebsiteDb
  , websiteDb
  , posts
  , repos
  ) where

import Database.Beam
import Database.Beam.Sqlite

import Website.Database.Models


data WebsiteDb f = WebsiteDb
  { _posts :: f (TableEntity BlogPostT)
  , _repos :: f (TableEntity RepositoryT)
  } deriving (Generic, Database Sqlite)

websiteDb :: DatabaseSettings Sqlite WebsiteDb
websiteDb = defaultDbSettings

WebsiteDb (TableLens posts)
          (TableLens repos) = dbLenses
