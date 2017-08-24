{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Network.HTTP.Conduit
import qualified Text.HTML.DOM as HTML
import           Text.XML hiding (writeFile)
import Data.Either
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BS
import Control.Concurrent.Async
import System.IO
import Control.Monad

import           Lib

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  resp <- makeReq mgr mainPage
  xs <- mapConcurrently (fetchFillingDate mgr) (take 20 $ bulletinMain resp)
  -- TODO merge errors
  let rs = rights xs
  let ls = lefts xs
  print $ length xs
  print $ length rs
  unless (null ls) (writeFile "./error.log" (show ls))
  BS.writeFile "./data.json" (encode rs)

fetchFillingDate :: Manager -> Either ErrorResult BulletinLink -> IO (Either [ErrorResult] Bulletin)
fetchFillingDate _ (Left e) = return $ Left [e]
fetchFillingDate mgr (Right r) = do
  T.putStrLn (title r)
  resp <- makeReq mgr (url r)
  return $ bulletinDetail resp


makeReq :: Manager -> Text -> IO Document
makeReq mgr link = do
  req <- parseUrlThrow (T.unpack link)
  fmap (HTML.parseLBS . responseBody) (httpLbs req mgr)
