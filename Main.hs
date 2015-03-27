module Main where

import Control.Applicative

type Variable = String

data Term = 
    Variable Variable |
    Abstraction String Command |
    Pair Term Term |
    Match Variable Variable Command
        deriving (Show,Eq,Ord)

data Command = Command Term Term deriving (Show,Eq,Ord)

data Type = Usual Connective | Dual Connective

data Connective = Unit

run :: Command -> [Command]
run c = runLeft c <|> runRight c

runLeft :: Command -> [Command]
runLeft (Command (Abstraction x c) t) = pure (substituteC t x c)
runLeft _ = empty

runRight :: Command -> [Command]
runRight (Command t (Abstraction x c)) = pure (substituteC t x c)
runRight _ = empty

runPairLeft :: Command -> [Command]
runPairLeft (Command (Match x y c) (Pair t u)) = pure (substituteC t x (substituteC u y c))
runPairLeft _ = []

runPairRight :: Command -> [Command]
runPairRight (Command (Pair t u) (Match x y c)) = pure (substituteC t x (substituteC u y c))
runPairRight _ = []

substituteC :: Term -> Variable -> Command -> Command
substituteC t x (Command l r) = Command (substituteT t x l) (substituteT t x r)

substituteT :: Term -> Variable -> Term -> Term
substituteT t x (Variable y)
    | x == y    = t
    | otherwise = Variable y
substituteT t x (Abstraction y c)
    | x == y    = Abstraction y c
    | otherwise = Abstraction y (substituteC t x c)

example :: Command
example = Command (Variable "a") (Abstraction "x" (Command (Variable "x") (Variable "z")))

--times
--plus
--with
--par


main :: IO ()
main = print (run example)


