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
      specify "once" (morteNet "once" onceExpr `shouldBe` onceNet)
      specify "composition" (morteNet "composition" compositionExpr `shouldBe` compositionNet)
      specify "twice" (morteNet "twice" twiceExpr `shouldBe` twiceNet))))

identityExpr :: Expr X
identityExpr = Lam "x" no "x"

identityNet :: InteractionNet
identityNet = [
  Construct "x" "r0" := Variable "identity",
  Variable "r0" := Variable "x"]

onceExpr :: Expr X
onceExpr =
  Lam "f" no (Lam "x" no (App (Var "f") (Var "x")))

onceNet :: InteractionNet
onceNet = [
  Construct "f" "r0" := Variable "once",
  Construct "x" "r1" := Variable "r0",
  Variable "f" := Construct "x" "r1"]

compositionExpr :: Expr X
compositionExpr =
  Lam "f" no (Lam "g" no (Lam "x" no (App (Var "f") (App (Var "g") (Var "x")))))

compositionNet :: InteractionNet
compositionNet = [
  Construct "f" "r0" := Variable "composition",
  Construct "g" "r1" := Variable "r0",
  Construct "x" "r2" := Variable "r1",
  Variable "g" := Construct "x" "r3",
  Variable "f" := Construct "r3" "r2"]

twiceExpr :: Expr X
twiceExpr =
  Lam "f" no (Lam "x" no (App (Var "f") (App (Var "f") (Var "x"))))

twiceNet :: InteractionNet
twiceNet = [
  Construct "f" "r0" := Variable "twice",
  Construct "x" "r1" := Variable "r0",
  Variable "f2" := Construct "x" "r2",
  Variable "f1" := Construct "r2" "r1",
  Duplicate "f1" "f2" := Variable "f"]


no :: a
no = error "Looking at type"

