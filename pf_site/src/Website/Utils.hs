module Website.Utils where

import Data.Aeson.TH ( defaultOptions, deriveJSON, fieldLabelModifier )


deriveJSON'= deriveJSON ( defaultOptions { fieldLabelModifier = tail } )
