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
sortByTempSpread weatherA weatherB
  | spreadA < spreadB = LT
  | spreadA > spreadB = GT
  | otherwise         = compare (day weatherA) (day weatherB)
  where spreadA = (maxTemp weatherA) - (minTemp weatherA)
        spreadB = (maxTemp weatherB) - (minTemp weatherB)

showWeathers :: [Weather] -> [String]
showWeathers weathers = map (\w ->
  let spread = (maxTemp w) - (minTemp w)
  in show (day w) ++ " " ++ show spread) weathers