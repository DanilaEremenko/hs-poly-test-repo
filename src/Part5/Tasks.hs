module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc []     =  acc
myFoldl f acc (x:xs) = myFoldl f (acc `f` x) xs  

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc []     =  acc  
myFoldr f acc (x:xs) = x `f` myFoldr f acc xs

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr ((:) . f) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f lst = myFoldr ((++) . f) [] (myReverse lst)

myConcat :: [[a]] -> [a]
myConcat lstlst = myFoldl (++) [] lstlst

myReverse :: [a] -> [a]
myReverse lst = myFoldr (\x acc -> acc ++ [x]) [] lst

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f lst = myFoldl (\acc x -> if f x then acc ++ [x] else acc) [] lst

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = notImplementedYet

