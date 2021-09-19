module Spidey.Crawl where
import Data.Text ( Text )
import Spidey.Data (PageData)
import qualified Data.Map as M

data CrawledPageData = CrawledPageData
  { mf2hash :: ByteString,
    targets :: Set Text
  }

crawl :: [Text] -> IO (M.Map Text CrawledPageData)
crawl = undefined 
