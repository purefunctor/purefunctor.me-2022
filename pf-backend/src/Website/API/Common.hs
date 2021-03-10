module Website.API.Common where

import Data.Aeson ( FromJSON, ToJSON )
import Data.Aeson.TH ( defaultOptions, deriveJSON, fieldLabelModifier )

import GHC.Generics ( Generic )

import Data.Text ( Text )


data MutableEndpointResult
  = MutableEndpointResult
      { status :: Int
      , reason :: Text
      }
  deriving (FromJSON, Generic, ToJSON)


deriveJSON' = deriveJSON ( defaultOptions { fieldLabelModifier = tail } )
