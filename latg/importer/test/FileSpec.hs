module FileSpec where 

import Test.Hspec
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import LATG.Importer.FileSchema

spec :: Spec
spec = do 
  describe "GenericDocument" $ do 
    it "loads JSON files" $ do
      json <- BL.readFile "latg/example/2015-01-01.html.json"
      let result = decode json :: Maybe GenericDocument
      pure ()
