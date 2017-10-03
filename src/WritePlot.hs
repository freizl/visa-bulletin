{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module WritePlot where


import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
import qualified Data.Text.Read                            as T
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import           Graphics.Rendering.Chart.Easy


import           Types

data LineData = LineData
  { reportMonth  :: Day
  , bulletinDate :: Day
  }

generatePlot :: [Bulletin] -> IO ()
generatePlot xs = do
  mapM_ print $ concatMap eb2Filling xs
  toFile def "report.svg" $ do
    layout_title .= "Price History"
    plot (line "EB2 Filling" [ concatMap eb2Filling xs ])
    plot (line "EB2 Final" [ concatMap eb2Final xs ])
    plot (line "EB3 Filling" [ concatMap eb3Filling xs ])
    plot (line "EB3 Final" [ concatMap eb3Final xs ])

prices :: [(Double, Double, Double)]
prices = [ (1, 2, 3), (2, 3, 4), (4, 5, 6) ]

eb2Filling Bulletin{..} = toLineData (Bulletin year month $ filter (\b -> visaDateType b == Filing && visaType b == Just EB2) (bulletins))
eb2Final Bulletin{..} = toLineData (Bulletin year month $ filter (\b -> visaDateType b == FinalAction && visaType b == Just EB2) (bulletins))
eb3Filling Bulletin{..} = toLineData (Bulletin year month $ filter (\b -> visaDateType b == Filing && visaType b == Just EB3) (bulletins))
eb3Final Bulletin{..} = toLineData (Bulletin year month $ filter (\b -> visaDateType b == FinalAction && visaType b == Just EB3) (bulletins))

toLineData :: Bulletin -> [ (LocalTime, LocalTime) ]
toLineData Bulletin{..} = [ (toReportMonth y m, toReportBulletinDate $ bulletDate n) | (n, y, m) <- zip3 bulletins (repeat year) (repeat month) ]


-- Convert date represented in string to Day. (very dummy)
--
toReportBulletinDate :: Text -> LocalTime
toReportBulletinDate node = LocalTime (fromGregorian y m d) midnight
  where node' = T.unpack node
        y = read ("20" ++ (drop 5 node'))
        m = toMonNum $ drop 2 $ take 5 node'
        d = read (take 2 node')

toMonNum :: String -> Int
toMonNum "JAN"       = 01
toMonNum "JANUARY"   = 01
toMonNum "FEB"       = 02
toMonNum "FEBRUARY"  = 02
toMonNum "MAR"       = 03
toMonNum "MARCH"     = 03
toMonNum "APR"       = 04
toMonNum "APRIL"     = 04
toMonNum "MAY"       = 05
toMonNum "JUN"       = 06
toMonNum "JUNE"      = 06
toMonNum "JUL"       = 07
toMonNum "JULY"      = 07
toMonNum "AUG"       = 08
toMonNum "AUGUST"    = 08
toMonNum "SEP"       = 09
toMonNum "SEPTEMBER" = 09
toMonNum "OCT"       = 10
toMonNum "OCTOBER"   = 10
toMonNum "NOV"       = 11
toMonNum "NOVEMBER"  = 11
toMonNum "DEC"       = 12
toMonNum "DECEMBER"  = 12
toMonNum _           = 00

toReportMonth :: Int -> Text -> LocalTime
toReportMonth y m = LocalTime (fromGregorian (fromIntegral y) (toMonNum $ T.unpack $ T.toUpper m) 1) midnight
