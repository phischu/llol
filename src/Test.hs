module Main where


import InteractionNet (
  InteractionNet, Cell(..), evaluate)

import Test.Hspec (
  hspec, describe, specify,
  Expectation, shouldBe)


main :: IO ()
main = hspec (do
  describe "interaction net" (do
    specify "test" (test `reducesTo` test')
    specify "testTwice" (testTwice `reducesTo` testTwice')))


reducesTo :: InteractionNet -> InteractionNet -> Expectation
reducesTo i1 i2 = evaluate i1 `shouldBe` i2

test :: InteractionNet
test = [
  Construct "p" "p1" "p2",
  Construct "p" "a1" "a2"]

test' :: InteractionNet
test' = [
  Wire "p1" "a1",
  Wire "p2" "a2"]

twice :: InteractionNet
twice = [
  Construct "twice" "f" "ff",
  Duplicate "f" "f1" "f2",
  Construct "f1" "f1x" "f1r",
  Construct "f2" "f2x" "f2r",
  Wire "f1r" "f2x",
  Construct "ff" "f1x" "f2r"]

testTwice :: InteractionNet
testTwice = [
  Construct "i" "x" "r",
  Wire "x" "r",
  Construct "twice" "i" "test"] ++ twice

testTwice' :: InteractionNet
testTwice' = [
  Construct "test" "x" "r",
  Wire "x" "r"]

