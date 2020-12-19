module PF.Component.Utils where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP


-- | Utility function for applying CSS classes
css :: forall r i. String -> HP.IProp (class :: String | r) i
css = HP.class_ <<< HH.ClassName
