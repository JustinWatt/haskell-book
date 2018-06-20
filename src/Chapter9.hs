{-# LANGUAGE OverloadedStrings #-}
module Chapter9 where

import           Data.Char

parseUpper :: String -> String
parseUpper = filter isUpper

capitalize :: String -> String
capitalize ""     = ""
capitalize (x:xs) = toUpper x : xs

capitalize' :: String -> String
capitalize' s = foldr ((:) . toUpper) "" s

firstLetter :: String -> Char
firstLetter = toUpper . head

shiftRight :: Int -> String -> String
shiftRight i s = shiftCharRight i <$> s

shiftLeft :: Int -> String -> String
shiftLeft i s = shiftCharLeft i <$> s

shiftCharLeft :: Int -> Char -> Char
shiftCharLeft i = chr . (subtract (mod i 26)) . ord

shiftCharRight :: Int -> Char -> Char
shiftCharRight i = chr . (+ (mod i 26)) . ord


-------

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem :: Eq a => a -> [a] -> Bool
myElem a = myAny (== a)

myReverse :: [a] -> [a]
myReverse = foldr (\x acc -> acc ++ [x]) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
