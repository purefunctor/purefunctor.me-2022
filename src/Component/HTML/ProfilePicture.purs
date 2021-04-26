module Website.Component.HTML.ProfilePicture where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Website.Component.Utils (css)


profilePicture :: forall w a. String -> Int -> Int -> HH.HTML w a
profilePicture c w h =
  HH.img
  [ css c
  , HP.src "https://avatars.githubusercontent.com/u/66708316?v=4"
  , HP.width w
  , HP.height h
  , HP.alt "GitHub Profile Picture"
  ]
