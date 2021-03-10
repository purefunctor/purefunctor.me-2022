module Website.Server.PseudoSSR.Types where

import Data.ByteString.Lazy as LBS ( ByteString )

import Network.HTTP.Media ( (//), (/:) )

import Servant ( Accept(contentType), MimeRender(..) )

import qualified Text.HTML.TagSoup as TS


data HTML
  = HTML


newtype TagSoupHTML
  = TagSoupHTML { unTagSoupHTML :: [TS.Tag LBS.ByteString] }


instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")


instance MimeRender HTML TagSoupHTML where
  mimeRender _ = TS.renderTags . unTagSoupHTML


data Tags
  = Tags
      { ogTitle :: ByteString
      , ogImage :: ByteString
      , ogUrl   :: ByteString
      , ogDesc  :: ByteString
      }
  deriving (Eq, Show)
