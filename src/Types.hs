{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson
import           Data.Text    (Text)
import qualified Data.Text    as T
import           GHC.Generics

type URL = String

data VisaType = EB2 | EB3
  deriving (Show, Eq, Generic)

data VisaDateType = FinalAction | Filing
  deriving (Show, Eq, Generic)

data Bulletin = Bulletin
  { year      :: Int
  , month     :: Text
  , bulletins :: [BulletinNode]
  } deriving (Show, Eq, Generic)

data BulletinNode = BulletinNode {
  visaDateType :: VisaDateType
  , visaType   :: Maybe VisaType
  , bulletDate :: Text
  } deriving (Show, Eq, Generic)

--instance ToJSON ErrorResult
instance ToJSON VisaType
instance ToJSON VisaDateType
instance ToJSON Bulletin
instance ToJSON BulletinNode
instance FromJSON VisaType
instance FromJSON VisaDateType
instance FromJSON Bulletin
instance FromJSON BulletinNode

data BulletinLink = BulletinLink { title :: Text
                                 , url   :: Text
                                 } deriving (Show)

data BulletinDate = BulletinDate { ebType :: Text
                                 , date   :: Text
                                 }

instance Show BulletinDate where
  show (BulletinDate t d) = "\t" ++ show t ++ ": " ++ T.unpack d
