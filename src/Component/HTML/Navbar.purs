module Website.Component.HTML.Navbar where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Website.Component.HTML.ProfilePicture (profilePicture)
import Website.Component.Utils (css)
import Website.Data.Routes (Route(..))


navbar :: forall w a. (Route -> a) -> HH.HTML w a
navbar navAction =
  HH.nav [ css "flex justify-between items-center px-8 h-32" ]
  [ HH.span [ css "space-x-6" ]
    [ profilePicture "inline self-center rounded-full" 32 32
    , HH.p [ css "inline" ]
      [ HH.text "Pure's Website"
      ]
    ]
  , HH.ul [ css "space-x-6" ]
    [ navItem "About" AboutR
    , navItem "Projects" ProjectsR
    , navItem "Contact" ContactR
    , HH.a [ css "text-underline cursor-pointer", HP.href "https://blog.purefunctor.me" ]
      [ HH.text "Blog"
      ]
    ]
  ]
  where
    navItem title route =
      HH.button
      [ css "inline text-underline cursor-pointer"
      , HE.onClick \_ -> navAction route
      , HP.tabIndex 0
      , HPA.role "link"
      , HPA.label $ "Navigate to " <> title <> " page"
      ]
      [ HH.text title
      ]
