{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeApplications   #-}
module Website.Database.Models.Repository where

import Data.Aeson ( FromJSON(..), ToJSON(..), object, withObject, (.:), (.=) )
import Data.Text ( Text )

import Database.Beam

import GHC.Int ( Int32 )

import Lens.Micro


data RepositoryT f
  = Repository
      { _rName  :: Columnar f Text
      , _rOwner :: Columnar f Text
      , _rUrl   :: Columnar f Text
      , _rLang  :: Columnar f Text
      , _rDesc  :: Columnar f Text
      , _rComm  :: Columnar f Int32
      , _rStar  :: Columnar f Int32
      }
  deriving (Beamable, Generic)

type Repository = RepositoryT Identity

type RepositoryName = PrimaryKey RepositoryT Identity

deriving instance Show Repository

deriving instance Eq Repository

instance Table RepositoryT where
  data PrimaryKey RepositoryT f
    = RepositoryName ( Columnar f Text )
    deriving (Beamable, Generic)
  primaryKey = RepositoryName . _rName

Repository (LensFor rName) (LensFor rOwner)
           (LensFor rUrl ) (LensFor rLang )
           (LensFor rDesc) (LensFor rComm )
           (LensFor rStar) = tableLenses

instance FromJSON Repository where
  parseJSON = withObject "repository" $ \v ->
    Repository
      <$> v .: "name"
      <*> v .: "owner"
      <*> v .: "url"
      <*> v .: "language"
      <*> v .: "description"
      <*> v .: "commits"
      <*> v .: "stars"

instance ToJSON Repository where
  toJSON repo = object
    [ "name"        .= ( repo^.rName  )
    , "owner"       .= ( repo^.rOwner )
    , "url"         .= ( repo^.rUrl   )
    , "language"    .= ( repo^.rLang  )
    , "description" .= ( repo^.rDesc  )
    , "commits"     .= ( repo^.rComm  )
    , "stars"       .= ( repo^.rStar  )
    ]
