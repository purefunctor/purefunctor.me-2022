module Website.Data.Routes where

import Data.Eq (class Eq)
import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))


data Route = HomeR | NotFoundR


derive instance genericRoutes :: Generic Route _
derive instance eqRoutes :: Eq Route


routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "HomeR": noArgs
  , "NotFoundR": "null" / noArgs
  }
