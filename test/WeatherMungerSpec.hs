module WeatherMungerSpec (spec) where

import Test.Hspec
import WeatherMunger

spec :: Spec
spec = do
  describe "#compareSpreads" $ do
    it "is EQ when the spreads are equal" $do
      let first = Weather 1 20 10
          second = Weather 1 20 10
      compareSpreads first second `shouldBe` EQ

    it "is LT when the first spread is smaller than the second" $do
      let first = Weather 1 20 15
          second = Weather 1 20 10
      compareSpreads first second `shouldBe` LT

    it "is GT when the first spread is bigger than the second" $do
      let first = Weather 1 20 10
          second = Weather 1 20 15
      compareSpreads first second `shouldBe` GT

  describe "#calcSpread" $ do
    it "is positive when the temperatures differ" $ do
      let weather = Weather 1 23 12
      calcSpread weather `shouldBe` 11

    it "is zero when the temperatures are the same" $ do
      let weather = Weather 1 23 23
      calcSpread weather `shouldBe` 0

  describe "#showWeathers" $ do
    it "shows blank when there is no weather to display" $ do
      showWeathers [] `shouldBe` []

    it "shows day and spread for a single weather" $ do
      showWeathers [Weather 1 20 15] `shouldBe` ["1 5"]

    it "shows day and spread for multiple weathers" $ do
      let weatherA = Weather 1 20 15
          weatherB = Weather 2 25 20
      showWeathers [weatherA, weatherB] `shouldBe` ["1 5", "2 5"]