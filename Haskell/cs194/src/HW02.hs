{-# OPTIONS_GHC -Wall #-}
module HW02 where
--import Debug.Trace (traceShow)

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches c1 c2 = sum . zipWith (asEqual) c1 $ c2
                 where asEqual :: Peg->Peg->Int
                       asEqual p1 p2 | p1 == p2 = 1
                                     | otherwise = 0

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors c = let lst = map toList c
                in foldl addLst [0,0,0,0,0,0] lst
                where toList x = case x of
                                 Red->   [1,0,0,0,0,0]
                                 Green-> [0,1,0,0,0,0]
                                 Blue -> [0,0,1,0,0,0]
                                 Yellow->[0,0,0,1,0,0]
                                 Orange->[0,0,0,0,1,0]
                                 Purple->[0,0,0,0,0,1]
                      addLst xs ys = zipWith (\x y -> x + y) xs ys

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c1 c2 = let count1 = countColors c1
                    count2 = countColors c2
                in sum . map (\(x,y)-> min x y) .  zip count1 $ count2
-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove c1 c2 = let exact = exactMatches c1 c2
                    nonexact = (matches c1 c2) - exact
                in Move c2 exact $ nonexact

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move c1 i j) c2 = let (Move _ x y) = getMove c1 c2
                                in i == x && j == y

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m = filter (isConsistent m)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 1 = map (:[])  colors
allCodes n = concatMap  withColors . allCodes $ (n - 1)
             where withColors lst = [(Red:lst), (Green:lst), (Blue:lst), (Yellow:lst),(Orange:lst),(Purple:lst)]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve c = let initialMove = (getMove c [Red,Red,Red,Red])
          in solve' [initialMove] (filterCodes initialMove (allCodes $ (length c)))
          where
          solve' [] _  = []
          solve' moves [] = moves
          solve' moves (x:[])  = moves ++ [getMove c x]
          solve' moves (x:xs)  = let newMove = (getMove c x)
                                  in
                                     solve' (moves ++ [newMove]) (filterCodes newMove xs)
-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
