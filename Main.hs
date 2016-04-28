module Main where

import Data.Maybe (fromMaybe)

-- Tensor, Par, With, Plus, Top, Unit, Bottom, Void
-- * | & + ^ () {}

type Variable =
  String

type Label =
  String

data Value =
  Variable Variable |
  Continuation Label Command |
  Pair Value Value |
  CaseL Value | CaseR Value |
  Selector Label Command Label Command |
  Methods Label Label Command

data Computation =
  Goto Label |
  Abstraction Variable Command |
  Match Variable Variable Command |
  Switch Variable Command Variable Command |
  DispatchL Computation | DispatchR Computation |
  Object Computation Computation

data Command =
  Run Value Computation

type Bindings =
  [(Variable,Value)]

type JumpTable =
  [(Label,Computation)]

run :: Bindings -> Command -> IO ()
run bindings (Run (Variable valueVariable) (Abstraction binderVariable command)) =
  run (push (binderVariable,value) bindings) command where
    value = fromMaybe (Variable valueVariable) (lookup valueVariable bindings)

push :: (Variable,Value) -> Bindings -> Bindings
push = (:)

test :: Command
test = Run (Variable "x") (Abstraction "y" undefined)

