module Main where

type Variable = String

data Term = Variable Variable | Abstraction String Command deriving (Show,Eq,Ord)

data Command = Command Term Term deriving (Show,Eq,Ord)

data Type = Usual Connective | Dual Connective

data Connective = Unit

run :: Command -> [Command]
run c = concatMap run [runC c,runC (swap c)]

swap :: Command -> Command
swap (Command l r) = Command r l

runC :: Command -> Command
runC (Command (Abstraction x c) t) = substituteC t x c

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


