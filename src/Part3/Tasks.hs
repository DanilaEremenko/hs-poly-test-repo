module Part3.Tasks where

import Util (notImplementedYet)
import Data.List
import Data.Function
-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = (f n) : finc f (n + 1)
    

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x) 

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
splitInt :: Int -> [Int]
splitInt 0 = []
splitInt x = x `mod` 10 : splitInt (x `div` 10)

mostFreq :: [Int] -> Int
mostFreq lst = fst $ maximumBy (compare `on` snd) (freqHist $ concat $ map splitInt lst)
    where 
        filtNumber :: [Int] -> Int -> [Int]
        filtNumber [] _ = []
        filtNumber (x:xs) n
            | x == n            = x : filtNumber xs n 
            | otherwise         =     filtNumber xs n

        countElPrt :: [Int] -> Int -> Int
        countElPrt = (\lst -> (\n -> length (filtNumber lst n)))

        freqOfList :: [Int] -> [Int] 
        freqOfList lst = map (countElPrt lst) lst

        freqHist :: [Int] -> [(Int, Int)]
        freqHist lst = zip lst (freqOfList lst)

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x:xs) 
    | x `elem` xs   = uniq xs
    | otherwise     = x : uniq xs

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = notImplementedYet
