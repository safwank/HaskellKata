{-# LANGUAGE OverloadedStrings #-}

module WeatherMunger where

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.List
import qualified Data.Vector as V

data Weather = Weather { dy :: !Int, mxT :: !Int, mnT :: !Int }

instance FromNamedRecord Weather where
  parseNamedRecord r = Weather <$> r .: "dy" <*> r .: "mxT" <*> r .: "mnT"

main :: IO ()
main = do
  csvData <- BL.readFile "weather.dat"
  case decodeByName csvData of
    Left err     -> putStrLn err
    Right (_, v) -> mapM_ putStrLn $ showWeathers $ sortBy sortByTempSpread $ V.toList v

sortByTempSpread a@(Weather _ _ _) b@(Weather _ _ _)
  | spreadA < spreadB = LT
  | spreadA > spreadB = GT
  | otherwise         = compare (dy a) (dy b)
  where spreadA = (mxT a) - (mnT a)
        spreadB = (mxT b) - (mnT b)

showWeathers :: [Weather] -> [String]
showWeathers weathers = map (\w ->
  let day    = dy w
      spread = (mxT w) - (mnT w)
  in show day ++ " " ++ show spread) weathers


