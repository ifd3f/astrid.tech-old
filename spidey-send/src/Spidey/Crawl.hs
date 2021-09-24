module Spidey.Crawl where

import Data.ByteString (ByteString)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)

data CrawledPageData = CrawledPageData
  { mf2hash :: ByteString,
    targets :: S.Set Text
  }

crawl :: [URI] -> IO (M.Map Text CrawledPageData)
crawl = undefined

crawlPage :: Document -> Writer [URI] CrawledPageData
crawlPage document = do
  let mf2 = undefined
  output extractCrawlURLs document
  pure
    CrawledPageData
      { mf2hash = extractMF2Hash mf2,
        targets = extractWebmentionUrls undefined mf2
      }

-- | Given a parsed HTML document, extracts URLs for crawling.
extractCrawlURLs :: Document -> [URI]
extractCrawlURLs document = undefined

-- | Given a parsed MF2 object, extracts its hash.
extractMF2Hash :: Value -> ByteString
extractMF2Hash mf2 = undefined

-- | Given a parsed MF2 object, returns all the URLs for sending webmentions to.
extractWebmentionURLs :: URIAuth -> Value -> (S.Set URI)
extractWebmentionURLs authority mf2 = undefined
