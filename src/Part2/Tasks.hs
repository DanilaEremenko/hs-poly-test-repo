module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) t1 t2 = BinaryTerm Plus t1 t2
(|-|) :: Term -> Term -> Term
(|-|) t1 t2 = BinaryTerm Minus t1 t2
(|*|) :: Term -> Term -> Term
(|*|) t1 t2 = BinaryTerm Times t1 t2
infixl 8 |-|
infixl 8 |+|
infixl 9 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar = notImplementedYet

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate = notImplementedYet
