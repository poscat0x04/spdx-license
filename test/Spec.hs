module Main where

import qualified Data.Text.IO as T
import Distribution.SPDX.Template
import System.Directory
import Test.Hspec
import Text.Megaparsec

main :: IO ()
main = do
  contents <- listDirectory "test/data"
  licenseFiles <- traverse T.readFile $ fmap ("test/data/" <>) contents
  let parseRes = traverse (uncurry $ runParser license) $ zip contents licenseFiles
  let prettyRes = fmap (map prettyLicense) parseRes
  hspec $ do
    describe "parse and pretty printing" $ do
      it "should roundtrip" $
        prettyRes `shouldBe` Right licenseFiles
