{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n =  n `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n = let newNum = dropLastDigit n
                in case newNum of
                   0 ->  [n]
                   _ -> (lastDigit n : (toRevDigits . dropLastDigit $ n))

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = snd . foldl foldWith  (0,[])
                   where foldWith :: (Integer,[Integer]) ->Integer-> (Integer,[Integer])
                         foldWith acc i =
                           case ((fst acc) `mod` 2) of
                             1 ->  ((fst acc) + 1, ((snd acc) ++ [2*i]))
                             _ ->  ((fst acc) + 1, ((snd acc) ++ [i]))


-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap (toRevDigits)


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn =   (== 0) . (`mod` 2) . sumDigits . doubleEveryOther  . toRevDigits

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 p1 p2 _ = [(p1,p2)]
hanoi n p1 p2 p3 = hanoi (n - 1) p1 p3 p2  ++ [(p1,p2)] ++ hanoi (n - 1) p3 p2 p1
