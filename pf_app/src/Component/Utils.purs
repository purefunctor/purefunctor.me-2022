module Website.Component.Utils where

import Prelude

import Data.Array (intercalate, singleton)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP


-- | Utility function for applying CSS classes
css :: forall r i. String -> HP.IProp (class :: String | r) i
css = HP.class_ <<< HH.ClassName


-- | Utility function for applying many CSS classes
css' :: forall r i. Array String -> HP.IProp (class :: String | r) i
css' = css <<< intercalate " "


-- | Utility function for applying many CSS classes,
-- returning an IProp array to be applied to HTML
-- functions.
classes :: forall r i. Array String -> Array (HP.IProp (class :: String | r) i)
classes = singleton <<< css'
