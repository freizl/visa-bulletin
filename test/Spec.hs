{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import qualified Text.HTML.DOM              as HTML
import Data.Either

import Lib

-- import Test.QuickCheck
-- import Control.Exception (evaluate)

main :: IO ()
main = hspec $
  describe "Find Bulletin Link" $ do
    it "returns 2 links" $ do
      xs <- fmap bulletinMain (HTML.readFile "./test/a.html")
      rs <- hasTwoRights xs
      title (head rs) `shouldBe` "Visa Bulletin For November 2016"
      url (head rs) `shouldBe` "https://travel.state.gov/content/visas/en/law-and-policy/bulletin/2017/visa-bulletin-for-november-2016.html"
      title (last rs) `shouldBe` "Visa Bulletin For December 2016"
      url (last rs) `shouldBe` "https://travel.state.gov/content/visas/en/law-and-policy/bulletin/2017/visa-bulletin-for-december-2016.html"

    it "returns eb2 and eb3 filling date" $ do
      xs <- fmap bulletinDetail (HTML.readFile "./test/b.html")
      rs <- hasTwoRights xs
      ebType (head rs) `shouldBe` "2nd"
      date (head rs) `shouldBe` "01MAR13"
      ebType (last rs) `shouldBe` "3rd"
      date (last rs) `shouldBe` "01MAY14"

hasTwoRights :: [Either a b] -> IO [b]
hasTwoRights xs = do
  length xs  `shouldBe` 2
  all isRight xs `shouldBe` True
  return $ rights xs
