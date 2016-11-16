{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Network.HTTP.Conduit

import           Control.Monad.IO.Class     (MonadIO)
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as BS
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match

main :: IO ()
main = do
  -- resp <- L.readFile "./a.html"
  resp <- simpleHttp mainPage
  app resp


type URL = String

baseURL :: URL
baseURL = "https://travel.state.gov"

mainPage :: URL
mainPage = baseURL ++ "/content/visas/en/law-and-policy/bulletin.html"

parseHttp :: MonadIO m => URL -> (L.ByteString -> [Tag L.ByteString]) -> m [Tag L.ByteString]
parseHttp url fn = liftM fn (simpleHttp url)

isBulletEls :: Tag L.ByteString -> Bool
isBulletEls = tagOpen isLI hasCurrentBulletClass
  where isLI = (== "li")
        hasCurrentBulletClass = anyAttr currentBulletClass
        currentBulletClass (n, v) = n == "class" && v == "current"

hasAttr :: Eq str => str -> [Attribute str] -> Bool
hasAttr name = anyAttrName (== name)

fromAttrHref :: Tag L.ByteString -> L.ByteString
fromAttrHref = fromAttrib "href"

app :: L.ByteString -> IO ()
app doc = do
  let tags = parseTags doc
  -- mapM_ print tags
  let ls = pageLinks $ filter (~== ("<a>" :: String)) $ take 40 $ dropWhile (~/= ("<li class=current>" :: String)) tags
  mapM_ print ls
  putStrLn  "=========="
  mapM_ (readBulletDate . BS.unpack) ls


readBulletDate :: URL -> IO ()
readBulletDate url = do
  -- doc <- L.readFile "./b.html"
  doc <- simpleHttp (baseURL ++ url)
  let tags = parseTags doc
  mapM_ print $
    take 1 $
    drop 8 $
    dropWhile (~/= TagText ("2nd" :: String)) $
    dropWhile (~/= ("<table>" :: String)) $
    dropWhile (~/= TagText ("DATES FOR FILING OF EMPLOYMENT-BASED" :: String)) tags



pageLinks :: [Tag L.ByteString] -> [L.ByteString]
pageLinks = map fromAttrHref . filter isPageLink
            where isPageLink = tagOpen (== "a") (hasAttr "href")
