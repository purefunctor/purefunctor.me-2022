module Website.Data.PreciseDateTime where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Newtype (unwrap)
import Data.PreciseDateTime (PreciseDateTime, fromRFC3339String, toRFC3339String)
import Data.RFC3339String (RFC3339String(..))


codec :: JsonCodec PreciseDateTime
codec = CA.prismaticCodec from to CA.string
  where
    from = fromRFC3339String <<< RFC3339String
    to = unwrap <<< toRFC3339String
