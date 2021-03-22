{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeApplications   #-}
module Website.Database.Models.BlogPost where

import Data.Aeson ( FromJSON(..), ToJSON(..), object, withObject, (.:), (.=) )
import Data.Text ( Text )
import Data.Time ( UTCTime )

import Database.Beam

import Lens.Micro


data BlogPostT f
  = BlogPost
      { _pTitle :: Columnar f Text
      , _pSlug  :: Columnar f Text
      , _pCont  :: Columnar f Text
      , _pPubl  :: Columnar f UTCTime
      , _pUpdt  :: Columnar f UTCTime
      }
  deriving (Beamable, Generic)

type BlogPost = BlogPostT Identity

type BlogPostSlug = PrimaryKey BlogPostT Identity

deriving instance Show BlogPost

deriving instance Eq BlogPost

instance Table BlogPostT where
  data PrimaryKey BlogPostT f
    = BlogPostSlug ( Columnar f Text )
    deriving (Beamable, Generic)
  primaryKey = BlogPostSlug . _pSlug

BlogPost (LensFor pTitle) (LensFor pSlug) (LensFor pCont )
         (LensFor pPubl ) (LensFor pUpdt) = tableLenses

instance FromJSON BlogPost where
  parseJSON = withObject "blog_post" $ \v ->
    BlogPost
      <$> v .: "title"
      <*> v .: "slug"
      <*> v .: "contents"
      <*> v .: "published"
      <*> v .: "updated"

instance ToJSON BlogPost where
  toJSON post = object
    [ "title"     .= ( post^.pTitle )
    , "slug"      .= ( post^.pSlug  )
    , "contents"  .= ( post^.pCont  )
    , "published" .= ( post^.pPubl  )
    , "updated"   .= ( post^.pUpdt  )
    ]
