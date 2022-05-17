module Part1.Tasks where

import Util(notImplementedYet)
import Data.List

fact :: Double -> Double
fact n
    | n <=1     = 1
    | otherwise = n * fact (n-1)


taylorPrec = 50

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = sinTaylorAcc x 0
        where
            sinTaylorAcc :: Double -> Double -> Double
            sinTaylorAcc x acc
                | acc == taylorPrec = sinTaylor x acc
                | otherwise         = sinTaylor x acc + sinTaylorAcc x (acc + 1)
                where
                    sinTaylor :: Double -> Double -> Double
                    sinTaylor x acc = ((-1) ** acc * x ** (2 * acc + 1)) / fact (2 * acc + 1)


-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = cosTaylorAcc x 0
        where
            cosTaylorAcc :: Double -> Double -> Double
            cosTaylorAcc x acc
                | acc == taylorPrec = cosTaylor x acc
                | otherwise         = cosTaylor x acc + cosTaylorAcc x (acc + 1)
                where
                    cosTaylor :: Double -> Double -> Double
                    cosTaylor x acc = ((-1) ** acc * x ** (2 * acc)) / fact (2 * acc)

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a b = myGCDAcc a b 1 1
        where
            myGCDAcc :: Integer -> Integer -> Integer -> Integer -> Integer
            myGCDAcc a b currDivisor bestDivisor
                | (a == 0) && (b == 0)                                          = 0
                | (currDivisor > abs a) && (currDivisor > abs b)            = bestDivisor 
                | ((a `mod` currDivisor) == 0) && ((b `mod` currDivisor) == 0)  = myGCDAcc a b (currDivisor + 1) currDivisor
                | otherwise                                                     = myGCDAcc a b (currDivisor + 1) bestDivisor
    

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов? 
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
    | day <= 0                                         = False
    | month == 2 && isYearLeap year       && day <= 29 = True
    | month == 2 && not (isYearLeap year) && day <= 28 = True
    | elem month [4, 6, 9, 7, 11]         && day <= 30 = True
    | elem month [1, 3, 5, 8, 10, 12]     && day <= 31 = True
    | otherwise                                        = False
        where
            isYearLeap :: Integer -> Bool
            isYearLeap year = (mod year 4 == 0) && not (mod year 100 == 0) || (mod year 400 == 0)


-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow n pow
    | pow == 0  = 1
    | pow == 1  = n
    | otherwise = myPowAcc n n 1 pow
            where
                myPowAcc :: Integer -> Integer -> Integer -> Integer -> Integer
                myPowAcc n factor acc maxAcc
                    | acc >= maxAcc  = n
                    | otherwise      = myPowAcc (n * factor)  factor (acc + 1) maxAcc

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime = notImplementedYet

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea = notImplementedYet

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c = notImplementedYet
