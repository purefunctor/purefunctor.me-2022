module Test.Utils where

import Test.Hspec.Wai

import           Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as LazyBS

import Text.Printf

import Network.HTTP.Types ( Header, methodPost )
import Network.Wai.Test ( SResponse )


type KeyValue = (String, String)


postJSON :: ByteString -> [Header] -> [KeyValue] -> WaiSession st SResponse
postJSON path headers json = request methodPost path headers json'
  where
    toContents :: (String, String) -> String
    toContents = uncurry (printf "%s: %s")

    json' :: LazyBS.ByteString
    json' = LazyBS.pack $ "{" ++ concat (toContents <$> json) ++ "}"
