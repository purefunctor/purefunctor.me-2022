module Test.Utils where

import Test.Hspec.Wai

import Data.Aeson.Micro ( Value , encode )

import           Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as LazyBS

import Text.Printf

import Network.HTTP.Types ( Header, methodPost )
import Network.Wai.Test ( SResponse )


postJSON :: ByteString -> [Header] -> Value -> WaiSession st SResponse
postJSON path headers = request methodPost path headers . encode
