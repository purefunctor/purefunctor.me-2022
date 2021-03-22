module Website.Database.Models
  ( module Website.Database.Models.BlogPost
  , module Website.Database.Models.Repository
  , WebsiteDb
  , websiteDb
  , posts
  , repos
  )
  where

import Database.Beam
import Database.Beam.Sqlite

import Website.Database.Models.BlogPost
import Website.Database.Models.Repository


data WebsiteDb f
  = WebsiteDb
      { _posts :: f (TableEntity BlogPostT)
      , _repos :: f (TableEntity RepositoryT)
      }
  deriving (Database Sqlite, Generic)

websiteDb :: DatabaseSettings Sqlite WebsiteDb
websiteDb = defaultDbSettings

WebsiteDb (TableLens posts)
          (TableLens repos) = dbLenses
