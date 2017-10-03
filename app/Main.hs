{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent.Async
import           Control.Monad
import           Data.Aeson               (decode)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as BS
import           Data.Either
import           Data.Maybe
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Network.HTTP.Conduit
import qualified Text.HTML.DOM            as HTML
import           Text.XML                 hiding (writeFile)

import           Fetch
import           Types
import           WritePlot

main :: IO ()
main = writePlot

reportFile :: FilePath
reportFile = "./report.json"

writeJSON :: IO ()
writeJSON = do
  mgr <- newManager tlsManagerSettings
  resp <- makeReq mgr mainPage
  xs <- mapConcurrently (fetchFillingDate mgr) (bulletinMain resp)
  -- TODO merge errors and friendly error format
  let rs = rights xs
  let ls = lefts xs
  print $ "Total links have been fetch " ++ show (length xs)
  print $ "Total pages having desired data " ++ show (length rs)
  unless (null ls) (writeFile "./error.log" (show ls))
  BS.writeFile reportFile (encodePretty rs)


writePlot :: IO ()
writePlot = do
  xs <- BS.readFile reportFile
  case decode xs of
    Just ys -> generatePlot ys
    Nothing -> putStrLn "can not decode report.json"


fetchFillingDate :: Manager -> Either ErrorResult BulletinLink -> IO (Either [ErrorResult] Bulletin)
fetchFillingDate _ (Left e) = return $ Left [e]
fetchFillingDate mgr (Right r) = do
  T.putStrLn (title r)
  resp <- makeReq mgr (url r)
  return $ bulletinDetail resp


makeReq :: Manager -> Text -> IO Document
makeReq mgr mainPageLink = do
  req <- parseUrlThrow (T.unpack mainPageLink)
  fmap (HTML.parseLBS . responseBody) (httpLbs req mgr)
