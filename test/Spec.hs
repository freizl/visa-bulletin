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
      length xs  `shouldBe` 186
      all isRight xs `shouldBe` True
      let rs = rights xs
      -- TODO verify all data
      title (rs !! 0) `shouldBe` "Visa Bulletin For September 2017"
      url (rs !! 0) `shouldBe` "https://travel.state.gov/content/visas/en/law-and-policy/bulletin/2017/visa-bulletin-for-september-2017.html"
      title (rs !! 1) `shouldBe` "Visa Bulletin For August 2017"
      url (rs !! 1) `shouldBe` "https://travel.state.gov/content/visas/en/law-and-policy/bulletin/2017/visa-bulletin-for-august-2017.html"

    it "returns eb2 and eb3 filling date" $ do
      eitherB <- fmap bulletinDetail (HTML.readFile "./test/b.html")
      isRight eitherB `shouldBe` True
      let bd = head $ rights [eitherB]
      let xs = bulletins bd
      year bd `shouldBe` 2017
      month bd `shouldBe` "September"
      length xs  `shouldBe` 4
      xs !! 0 `shouldBe` BulletinNode FinalAction (Just EB2) "15MAY13"
      xs !! 1 `shouldBe` BulletinNode FinalAction (Just EB3) "01JAN12"
      xs !! 2 `shouldBe` BulletinNode Filing (Just EB2) "01OCT13"
      xs !! 3 `shouldBe` BulletinNode Filing (Just EB3) "01SEP15"
