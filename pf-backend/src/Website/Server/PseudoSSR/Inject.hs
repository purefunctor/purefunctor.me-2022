module Website.Server.PseudoSSR.Inject where

import Data.ByteString.Lazy ( ByteString )

import Text.HTML.TagSoup ( Tag(TagClose, TagOpen), isTagCloseName )

import Website.Server.PseudoSSR.Types ( TagSoupHTML(TagSoupHTML), Tags(Tags) )


mkTags :: Tags -> [Tag ByteString]
mkTags (Tags title image url) = concat
  [ meta "og:title" title
  , meta "og:image" image
  , meta "og:url" url
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
