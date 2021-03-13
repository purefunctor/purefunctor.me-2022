module Website.Server.PseudoSSR.Inject where

import           Data.ByteString.Lazy ( ByteString )
import qualified Data.Foldable as Foldable
import           Data.Maybe ( fromMaybe )
import qualified Data.Sequence as Sequence

import Text.HTML.TagSoup

import Website.Server.PseudoSSR.Types


mkOpenGraph :: MetaTags -> [Tag ByteString]
mkOpenGraph tags = concat
  [ make "og:title"       $ ogTitle tags
  , make "og:image"       $ ogImage tags
  , make "og:url"         $ ogUrl tags
  , make "og:description" $ ogDesc tags
  ]
  where
    make :: ByteString -> ByteString -> [Tag ByteString]
    make property content =
      [ TagOpen "meta"
        [ ("property", property)
        , ("content", content)
        ]
      , TagClose "meta"
      ]


injectMeta :: MetaTags -> TagSoupHTML -> TagSoupHTML
injectMeta tags (TagSoupHTML html) = fromMaybe withOpenGraph withTitle
  where
    withOpenGraph :: TagSoupHTML
    withOpenGraph = TagSoupHTML $ open <> mkOpenGraph tags <> close
      where
        open =
          takeWhile (not . isTagCloseName "head") html

        close =
          dropWhile (not . isTagCloseName "head") html

    withTitle :: Maybe TagSoupHTML
    withTitle = do
      let
        html' = Sequence.fromList $ unTagSoupHTML withOpenGraph

        titleText
          = TagText
          . innerText
          . Foldable.toList
          . snd
          . Sequence.breakr (isTagCloseName "title")
          . Sequence.dropWhileL (not . isTagOpenName "title")
          $ html'

      ix <- Sequence.findIndexL (== titleText) html'

      pure $ TagSoupHTML $ Foldable.toList $
        Sequence.adjust' (const $ TagText $ title tags) ix html'
