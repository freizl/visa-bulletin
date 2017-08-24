{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import           Control.Monad
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding                  as T
import qualified Data.Text.Read                  as T
import           Text.XML
import           Text.XML.Cursor
import Data.Either
import Data.Aeson
import GHC.Generics

type URL = String

baseURL :: Text
baseURL = "https://travel.state.gov"

mainPage :: Text
mainPage = baseURL `T.append` "/content/visas/en/law-and-policy/bulletin.html"

data ErrorResult = ErrorResult { msg :: Text
                               , lastCursor :: [Cursor]
                               } deriving (Show, Eq, Generic)

data VisaType = EB2 | EB3
  deriving (Show, Eq, Generic)

data VisaDateType = FinalAction | Filing
  deriving (Show, Eq, Generic)

data Bulletin = Bulletin
  { year :: Int
  , month :: Text
  , bulletins :: [BulletinNode]
  } deriving (Show, Eq, Generic)

data BulletinNode = BulletinNode {
  visaDateType :: VisaDateType
  , visaType :: Maybe VisaType
  , bulletDate   :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON ErrorResult
instance ToJSON VisaType
instance ToJSON VisaDateType
instance ToJSON Bulletin
instance ToJSON BulletinNode

data BulletinLink = BulletinLink { title :: Text
                                 , url   :: Text
                                 } deriving (Show)

data BulletinDate = BulletinDate { ebType :: Text
                                 , date   :: Text
                                 }

instance Show BulletinDate where
  show (BulletinDate t d) = "\t" ++ show t ++ ": " ++ T.unpack d

bulletinMain :: Document -> [Either ErrorResult BulletinLink]
bulletinMain doc = fromDocument doc
  $// (element "dl" >=> attributeIs "class" "data_show_hide"
       &// element "ul" >=> attributeIs "class" "default"
       &/ element "li"
       &/ element "p"
       &/ element "a"
       >=> check isVisaBulletinLink
       &| genBulletinLink)

isVisaBulletinLink :: Cursor -> Bool
isVisaBulletinLink c = let t' = attribute "title" c
                       in
                         length t' == 1 && "Visa Bulletin" `T.isPrefixOf` head t'

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

bulletinDetail :: Document -> Either [ErrorResult] Bulletin
bulletinDetail doc =
  let c = fromDocument doc
      n1 = c $// categoryDiv &/ sectionDiv
      titleNode = c $// pageTitleTextNode
      ym = if length titleNode == 1
        then getYearAndMonth (head titleNode)
        else Left (ErrorResult "No title found for the page" titleNode)
  in
    if length n1 /= 5
    then Left [ ErrorResult (T.pack $ "Expect 5 tables but gets " ++ show (length n1) ) n1 ]
         -- 4th one is EB final action date
         -- 5th one is EB filling date
    else let bnodes = map ((Right . toBullet FinalAction) =<<) (genBulletinDate (n1 !! 3))
               ++ map ((Right . toBullet Filing) =<<) (genBulletinDate (n1 !! 4))
             brights = rights bnodes
             blefts = lefts bnodes
         in
           case ym of
             Left l -> Left (l : blefts)
             Right (y, m) -> Right (Bulletin y m brights)

getYearAndMonth :: Cursor    -- page title, "Visa Bulletin For November 2016"
                -> Either ErrorResult (Int, Text)
getYearAndMonth tc = let ts = T.splitOn " " (head $ content tc)
                    in
                      if length ts == 5
                      then case T.decimal (ts !! 4) of
                        Right (i, _) -> Right (i, ts !! 3)
                        Left _ -> Left (ErrorResult "Cant parse year from title" [tc])
                      else Left (ErrorResult "Cant parse title" [tc])

toBullet :: VisaDateType -> BulletinDate -> BulletinNode
toBullet vdt (BulletinDate ebt nodeDate) =
  BulletinNode { visaDateType = vdt
               , visaType = vt
               , bulletDate = nodeDate
               }
  where vt = toEBVisaType ebt

toEBVisaType :: Text -> Maybe VisaType
toEBVisaType s
  | s == "2nd" = Just EB2
  | s == "3rd" = Just EB3
  | otherwise = Nothing


genBulletinDate :: Cursor   -- section DIV
                -> [Either ErrorResult BulletinDate]
genBulletinDate = map dateTd . concatMap dateTr . firstTable

firstTable :: Cursor -> [Cursor]
firstTable = take 1 . ($// element "table")

dateTr :: Cursor          -- TABLE
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

pageTitleTextNode :: Cursor -> [Cursor]
pageTitleTextNode c = c $// element "title" >=> child

{-

uTextNode :: Cursor -> [Cursor]
uTextNode c = c $// element "u" >=> child

isEmploymentBased :: Cursor -> Bool
isEmploymentBased = (== 1) . length . filter isEmploymentBasedContent . uTextNode

isEmploymentBasedContent :: Cursor -> Bool
isEmploymentBasedContent = (== ["DATES FOR FILING OF EMPLOYMENT-BASED"]) . content

-}
