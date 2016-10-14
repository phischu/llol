{-# LANGUAGE OverloadedStrings #-}
module Main where


import InteractionNet (
  InteractionNet, Statement(..), Term(..))

import Morte.Core (
  Expr(Lam), X)

import Lambda (
  morteNet)

import Test.Hspec (
  hspec, describe, specify,
  shouldBe)


main :: IO ()
main = hspec (do
  describe "lambda" (do
    describe "morteNet" (do
      specify "identity" (morteNet "i" identityExpr `shouldBe` identityNet))))

identityExpr :: Expr X
identityExpr = Lam "x" no "x"

identityNet :: InteractionNet
identityNet = [
  Variable "i" := Construct "x" "r",
  Variable "r" := Variable "x"]

no :: a
no = error "Looking at type"

