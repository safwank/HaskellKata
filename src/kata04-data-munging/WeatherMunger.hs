{-# LANGUAGE OverloadedStrings #-}

module WeatherMunger (Weather(..), compareSpreads, calcSpread, showWeathers) where

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.List
import qualified Data.Vector as V

data Weather = Weather { day :: Int, maxTemp :: Int, minTemp :: Int }

instance FromNamedRecord Weather where
  parseNamedRecord r = Weather <$> r .: "dy" <*> r .: "mxT" <*> r .: "mnT"

main :: IO ()
main = do
  csvData <- BL.readFile "weather.dat"
  case decodeByName csvData of
    Left err            -> putStrLn err
    Right (_, weathers) -> mapM_ putStrLn $ showWeathers $ sortBy compareSpreads $ V.toList weathers

compareSpreads :: Weather -> Weather -> Ordering
compareSpreads weatherA weatherB = (calcSpread weatherA) `compare` (calcSpread weatherB)

calcSpread :: Weather -> Int
calcSpread weather = (maxTemp weather) - (minTemp weather)

showWeathers :: [Weather] -> [String]
showWeathers weathers = map showWeather weathers
  where showWeather = \w -> show (day w) ++ " " ++ show (calcSpread w)