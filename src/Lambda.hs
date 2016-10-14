module Lambda where

import InteractionNet (
  InteractionNet, Statement(..), Term(..), Variable)

import Morte.Core (
  Expr(Var, Lam), X,
  pretty)

import Data.Text.Lazy (
  unpack)


morteNet :: Variable -> Expr X -> InteractionNet
morteNet v (Var x) = [
  Variable v := Variable (unpack (pretty x))]
morteNet v (Lam x _ e) = [
  Variable v := Construct (unpack x) "r"] ++ morteNet "r" e


