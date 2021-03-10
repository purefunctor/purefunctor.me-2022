module Website.Server.PseudoSSR.Inject where

import Data.ByteString.Lazy ( ByteString )

import Text.HTML.TagSoup ( Tag(TagClose, TagOpen), isTagCloseName )

import Website.Server.PseudoSSR.Types


mkTags :: Tags -> [Tag ByteString]
mkTags tags = concat
  [ meta "og:title"       $ ogTitle tags
  , meta "og:image"       $ ogImage tags
  , meta "og:url"         $ ogUrl tags
  , meta "og:description" $ ogDesc tags
  ]
  where
    meta :: ByteString -> ByteString -> [Tag ByteString]
    meta property content =
      [ TagOpen "meta"
        [ ("property", property)
        , ("content", content)
        ]
      , TagClose "meta"
      ]


injectTags :: Tags -> TagSoupHTML -> TagSoupHTML
injectTags tags (TagSoupHTML original) =
  let
    st = takeWhile (not . isTagCloseName "head") original
    en = dropWhile (not . isTagCloseName "head") original
  in
    TagSoupHTML $ st ++ mkTags tags ++ en
