{-# LANGUAGE OverloadedStrings #-}
module Main where


import InteractionNet (
  InteractionNet, Statement(..), Term(..))

import Morte.Core (
  Expr(Var, Lam, App), X)

import Lambda (
  morteNet)

import Test.Hspec (
  hspec, describe, specify,
  shouldBe)


main :: IO ()
main = hspec (do
  describe "lambda" (do
    describe "morteNet" (do
      specify "identity" (morteNet "identity" identityExpr `shouldBe` identityNet)
      specify "once" (morteNet "once" onceExpr `shouldBe` onceNet))))

identityExpr :: Expr X
identityExpr = Lam "x" no "x"

identityNet :: InteractionNet
identityNet = [
  Construct "x" "r0" := Variable "identity",
  Variable "r0" := Variable "x"]

onceExpr :: Expr X
onceExpr = Lam "f" no (Lam "x" no (App (Var "f") (Var "x")))

onceNet :: InteractionNet
onceNet = [
  Construct "f" "r0" := Variable "once",
  Construct "x" "r1" := Variable "r0",
  Variable "f" := Construct "x" "r1"]

no :: a
no = error "Looking at type"

