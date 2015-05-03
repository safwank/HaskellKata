{-# LANGUAGE OverloadedStrings #-}

module WeatherMunger where

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.List
import qualified Data.Vector as V

data Weather = Weather { day :: !Int, maxTemp :: !Int, minTemp :: !Int }

instance FromNamedRecord Weather where
  parseNamedRecord r = Weather <$> r .: "dy" <*> r .: "mxT" <*> r .: "mnT"

main :: IO ()
main = do
  csvData <- BL.readFile "weather.dat"
  case decodeByName csvData of
    Left err            -> putStrLn err
    Right (_, weathers) -> mapM_ putStrLn $ showWeathers $ sortBy sortByTempSpread $ V.toList weathers

sortByTempSpread :: Weather -> Weather -> Ordering
sortByTempSpread weatherA weatherB = (calcTempSpread weatherA) `compare` (calcTempSpread weatherB)

calcTempSpread :: Weather -> Int
calcTempSpread weather = (maxTemp weather) - (minTemp weather)

showWeathers :: [Weather] -> [String]
showWeathers weathers = map showWeather weathers
  where showWeather = \w -> show (day w) ++ " " ++ show (calcTempSpread w)