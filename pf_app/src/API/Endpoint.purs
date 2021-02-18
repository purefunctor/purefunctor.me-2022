module PF.API.Endpoint where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', root, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))


data Endpoint
  = BlogPosts
  | Repositories
  | BlogPost String
  | Repository String


derive instance genericEndpoint :: Generic Endpoint _


endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum
  { "BlogPosts": "blog" / noArgs
  , "Repositories": "repo" / noArgs
  , "BlogPost": "blog" / string segment
  , "Repository": "repo" / string segment
  }
