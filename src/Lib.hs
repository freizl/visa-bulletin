{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Monad
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding                  as T
import           Text.XML
import           Text.XML.Cursor

type URL = String

baseURL :: Text
baseURL = "https://travel.state.gov"

mainPage :: Text
mainPage = baseURL `T.append` "/content/visas/en/law-and-policy/bulletin.html"

data ErrorResult = ErrorResult { msg :: Text
                               , lastCursor :: [Cursor]
                               } deriving (Show)
{-

data Bulletin = Bulletin { title :: Text
                         , url :: Text
                         , ebType :: Text
                         , date :: Text
                         } deriving (Show)
-}

data BulletinLink = BulletinLink { title :: Text
                                 , url   :: Text
                                 } deriving (Show)

data BulletinDate = BulletinDate { ebType :: Text
                                 , date   :: Text
                                 }

instance Show BulletinDate where
  show (BulletinDate t d) = T.unpack ("\t" `T.append` t `T.append` ": " `T.append` d)

bulletinMain :: Document -> [Either ErrorResult BulletinLink]
bulletinMain doc = fromDocument doc $// (findCurrentLi &// findBulletLink &| genBulletinLink)

findCurrentLi :: Cursor -> [Cursor]
findCurrentLi = element "li" >=> attributeIs "class" "current"

findBulletLink :: Cursor -> [Cursor]
findBulletLink = element "a"

genBulletinLink :: Cursor -> Either ErrorResult BulletinLink
genBulletinLink c = let t' = attribute "title" c
                        u' = attribute "href" c
                    in
                      if length t' == 1 && length u' == 1
                      then Right (BulletinLink (head t') (baseURL `T.append` head u'))
                      else Left (ErrorResult "Cant find bulletin link" [c])

bulletinDetail :: Document -> [Either ErrorResult BulletinDate]
bulletinDetail doc =
  let c = fromDocument doc
      n1 = c $// categoryDiv &/ sectionDiv
      n2 = filter isEmploymentBased n1
  in
    if length n2 == 1
    then genBulletinDate (head n2)
    else [Left (ErrorResult ("Cant find EB date for filling table = "
                             `T.append`
                             T.pack ((show $ length n1) ++ (show $ length n2)) ) n1)]


genBulletinDate :: Cursor   -- section div
                -> [Either ErrorResult BulletinDate]
genBulletinDate = map dateTd . concatMap dateTr . firstTable

firstTable :: Cursor -> [Cursor]
firstTable = take 1 . ($// element "table")

dateTr :: Cursor          -- Table
       -> [Cursor]
dateTr c = take 2 $ drop 2 (c $// element "tr")

dateTd :: Cursor         -- TR
       -> Either ErrorResult BulletinDate
dateTd c = let tds = c $/ element "td" >=> child >=> content
           in
             if length tds >= 3
             then Right $ BulletinDate (head tds) (tds !! 2)
             else Left $ ErrorResult "Cant find desired TD" [c]


categoryDiv :: Cursor
            -> [Cursor]
categoryDiv = element "div" >=> attributeIs "class" "Visa_Contentpage_Category parsys"

sectionDiv :: Cursor -> [Cursor]
sectionDiv = element "div" >=> attributeIs "class" "simple_richtextarea section"

uTextNode :: Cursor -> [Cursor]
uTextNode c = c $// element "u" >=> child

isEmploymentBased :: Cursor -> Bool
isEmploymentBased = (== 1) . length . filter isEmploymentBasedContent . uTextNode

isEmploymentBasedContent :: Cursor -> Bool
isEmploymentBasedContent = (== ["DATES FOR FILING OF EMPLOYMENT-BASED"]) . content
