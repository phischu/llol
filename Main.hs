module Main where

import Control.Applicative

type Variable = String

data Term = 
    Variable Variable |
    Abstraction Variable Command |
    Pair Term Term |
    Match Variable Variable Command |
    Unit |
    Delay Command |
    One Term |
    Two Term |
    Select Variable Command Variable Command
        deriving (Show,Eq,Ord)

data Command = Command Term Term deriving (Show,Eq,Ord)

run :: Command -> [Command]
run c = case runOnce c of
    [] -> pure c
    cs -> concatMap run cs

runOnce :: Command -> [Command]
runOnce c =
    runAbstraction c <|>
    runSwapped runAbstraction c <|>
    runMatch c <|>
    runSwapped runMatch c <|>
    runDelay c <|>
    runSwapped runDelay c <|>
    runSelect c <|>
    runSwapped runSelect c

runSwapped :: (Command -> [Command]) -> Command -> [Command]
runSwapped f (Command t u) = f (Command u t)

runAbstraction :: Command -> [Command]
runAbstraction (Command (Abstraction x c) t) = pure (substituteC t x c)
runAbstraction _ = empty

runMatch :: Command -> [Command]
runMatch (Command (Match x y c) (Pair t u)) = pure (substituteC t x (substituteC u y c))
runMatch _ = empty

runDelay :: Command -> [Command]
runDelay (Command (Delay c) Unit) = pure c
runDelay _ = empty

runSelect :: Command -> [Command]
runSelect (Command (Select x c1 _ _ ) (One t)) = pure (substituteC t x c1)
runSelect (Command (Select _ _  y c2) (Two t)) = pure (substituteC t y c2)
runSelect _ = empty


substituteC :: Term -> Variable -> Command -> Command
substituteC t x (Command l r) = Command (substituteT t x l) (substituteT t x r)

substituteT :: Term -> Variable -> Term -> Term
substituteT t x (Variable y)
    | x == y    = t
    | otherwise = Variable y
substituteT t x (Abstraction y c)
    | x == y    = Abstraction y c
    | otherwise = Abstraction y (substituteC t x c)
substituteT t x (Pair l r)
                = Pair (substituteT t x l) (substituteT t x r)
substituteT t x (Match a b c)
    | x == a    = Match a b c
    | x == b    = Match a b c
    | otherwise = Match a b (substituteC t x c)
substituteT _ _ Unit
                = Unit
substituteT t x (Delay c)
                = Delay (substituteC t x c)
substituteT t x (One u)
                = One (substituteT t x u)
substituteT t x (Two u)
                = Two (substituteT t x u)
substituteT t x (Select a c1 b c2)
    | x == a && x == b = Select a c1 b c2
    | x == a    = Select a c1 b (substituteC t x c2)
    | x == b    = Select a (substituteC t x c1) b c2
    | otherwise = Select a (substituteC t x c1) b (substituteC t x c2)

example1 :: Command
example1 = Command (Variable "a") (Abstraction "x" (Command (Variable "x") (Variable "z")))

example2 :: Command
example2 = Command (apply (lambda "x" (Variable "x")) (Variable "y")) Unit

example3 :: Command
example3 = Command (Select "x" (Command (Variable "x") Unit) "x" (Command Unit Unit)) (One (Variable "z"))

apply :: Term -> Term -> Term
apply t u = Abstraction "alpha" (Command t (Pair u (Variable "alpha")))

lambda :: Variable -> Term -> Term
lambda x t = Match x "alpha" (Command t (Variable "alpha"))

main :: IO ()
main = print (run example3)


