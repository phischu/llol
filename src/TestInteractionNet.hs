module Main where


import InteractionNet (
  InteractionNet, Environment, Statement(..), Term(..), evaluate, execEvaluator)

import Test.Hspec (
  hspec, describe, specify,
  Expectation, shouldBe)

import qualified Data.Map as Map (
  fromList)


main :: IO ()
main = hspec (do
  describe "interaction net" (do
    specify "test" (test `evaluatesTo` test')
    specify "testTwice" (testTwice `evaluatesTo` testTwice')))


evaluatesTo :: InteractionNet -> Environment -> Expectation
evaluatesTo net environment = execEvaluator (evaluate net) `shouldBe` environment

test :: InteractionNet
test = [
  Variable "p" := Construct "p1" "p2",
  Variable "p" := Construct "a1" "a2"]

test' :: Environment
test' = Map.fromList [
  ("p1", Variable "a1"),
  ("p2", Variable "a2")]

twice :: InteractionNet
twice = [
  Variable "twice" := Construct "f" "ff",
  Variable "f" := Duplicate "f1" "f2",
  Variable "f1" := Construct "f1x" "f1r",
  Variable "f2" := Construct "f2x" "f2r",
  Variable "f1r" := Variable "f2x",
  Variable "ff" := Construct "f1x" "f2r"]

testTwice :: InteractionNet
testTwice = [
  Variable "i" := Construct "x" "r",
  Variable "x" := Variable "r",
  Variable "twice" := Construct "i" "test"] ++ twice

testTwice' :: Environment
testTwice' = Map.fromList [
  ("f1x", Variable "f2r"),
  ("test", Construct "f1x" "f2r")]

