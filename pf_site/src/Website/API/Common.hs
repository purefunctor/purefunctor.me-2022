{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Website.API.Common where

import Data.Aeson ( FromJSON, ToJSON )

import GHC.Generics ( Generic )

import Data.Text ( Text )


data MutableEndpointResult
  = MutableEndpointResult
      { status :: Int
      , reason :: Text
      }
  deriving (FromJSON, Generic, ToJSON)
