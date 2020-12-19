module PF.Data.Routes where

import Data.Eq (class Eq)
import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))


data Routes = HomeR | AboutR


derive instance genericRoutes :: Generic Routes _
derive instance eqRoutes :: Eq Routes


routeCodec :: RouteDuplex' Routes
routeCodec = root $ sum
  { "HomeR": noArgs
  , "AboutR": "about" / noArgs
  }
