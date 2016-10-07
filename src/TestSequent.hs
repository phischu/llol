module Main where


import Sequent (
  Statement((:=)), Binder(..), Value(..), evaluate)

import Test.Hspec (
  hspec, describe, specify,
  shouldBe)


main :: IO ()
main = hspec (do
  describe "sequent" (do
    specify "test" (evaluate test `shouldBe` test')
    specify "testOnce" (evaluate testOnce `shouldBe` testOnce')
    specify "testTwice" (evaluate testTwice `shouldBe` testTwice')
    specify "testSubstitution" (evaluate testSubstitution `shouldBe` testSubstitution')))


test :: [Statement]
test = [
  Bind "i" := Thunk (Match "x" "r"),
  Force (Use "x") := (Use "r"),
  Bind "o" := Thunk (Bind "hi"),
  Bind "p" := Pair "5" "o",
  Force (Use "p") := Use "i"]

test' :: [Statement]
test' = [
  Bind "hi" := Use "5"]


once :: [Statement]
once = [
  Bind "once" := Thunk (Match "f" "ff"),
  Match "fx" "fr" := Use "f",
  Force (Use "ff") := Pair "fx" "fr"]

testOnce :: [Statement]
testOnce = [
  Bind "i" := Thunk (Match "x" "r"),
  Force (Use "r") := Use "x",
  Force (Use "once") := Pair "i" "test"] ++ once

testOnce' :: [Statement]
testOnce' = [
  Force (Use "f2r'") := Use "f1x'",
  Bind "test" := Thunk (Match "f1x'" "f2r'")]


twice :: [Statement]
twice = [
  Bind "twice" := Thunk (Match "f" "ff"),
  Copy "f1" "f2" := Use "f",
  Match "f1x" "f1r" := Use "f1",
  Match "f2x" "f2r" := Use "f2",
  Force (Use "f1r") := Use "f2x",
  Force (Use "ff") := Pair "f1x" "f2r"]

testTwice :: [Statement]
testTwice = [
  Bind "i" := Thunk (Match "x" "r"),
  Force (Use "r") := Use "x",
  Force (Use "twice") := Pair "i" "test"] ++ twice

testTwice' :: [Statement]
testTwice' = [
  Force (Use "f2r'") := Use "f1x'",
  Bind "test" := Thunk (Match "f1x'" "f2r'")]


testSubstitution :: [Statement]
testSubstitution = [
  (:=) (Bind "test'") (Thunk (Match "f1x'" "f2r'")),
  (:=) (Force (Use "test'")) (Use "test")]

testSubstitution' :: [Statement]
testSubstitution' = [
  (:=) (Match "f1x'" "f2r'") (Use "test")]

