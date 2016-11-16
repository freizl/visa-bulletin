{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Lib
import           Network.HTTP.Conduit
import qualified Text.HTML.DOM as HTML
import           Text.XML

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  resp <- makeReq mgr mainPage
  mapM_ (fetchFillingDate mgr) (bulletinMain resp)

fetchFillingDate :: Manager -> Either ErrorResult BulletinLink -> IO ()
fetchFillingDate _ (Left e) = print e
fetchFillingDate mgr (Right r) = do
  T.putStrLn (title r)
  resp <- makeReq mgr (url r)
  let rs = bulletinDetail resp
  mapM_ (either print print) rs


makeReq :: Manager -> Text -> IO Document
makeReq mgr link = do
  req <- parseUrlThrow (T.unpack link)
  fmap (HTML.parseLBS . responseBody) (httpLbs req mgr)
