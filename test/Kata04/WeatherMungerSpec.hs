module Kata04.WeatherMungerSpec (spec) where

import Test.Hspec
import Kata04.WeatherMunger

spec :: Spec
spec = do
  describe "#compareSpreads" $ do
    context "when the spreads are equal" $ do
      let first = Weather 1 20 10
          second = Weather 1 20 10

      it "is EQ" $ do
        compareSpreads first second `shouldBe` EQ

    context "when the first spread is smaller than the second" $ do
      let first = Weather 1 20 15
          second = Weather 1 20 10

      it "is LT" $ do
        compareSpreads first second `shouldBe` LT

    context "when the first spread is bigger than the second" $ do
      let first = Weather 1 20 10
          second = Weather 1 20 15

      it "is GT" $ do
        compareSpreads first second `shouldBe` GT

  describe "#calcSpread" $ do
    context "when the temperatures differ" $ do
      let weather = Weather 1 23 12

      it "is positive" $ do
        calcSpread weather `shouldBe` 11

    context "when the temperatures are the same" $ do
      let weather = Weather 1 23 23

      it "is zero" $ do
        calcSpread weather `shouldBe` 0

  describe "#showWeathers" $ do
    context "when there is no weather to display" $ do
      it "shows blank" $ do
        showWeathers [] `shouldBe` []

    context "for a single weather" $ do
      it "shows day and spread" $ do
        showWeathers [Weather 1 20 15] `shouldBe` ["1 5"]

    context "for multiple weathers" $ do
      let weatherA = Weather 1 20 15
          weatherB = Weather 2 25 20

      it "shows day and spread" $ do
        showWeathers [weatherA, weatherB] `shouldBe` ["1 5", "2 5"]