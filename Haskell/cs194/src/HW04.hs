{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [1]

-- Exercise 2 ----------------------------------------

dropIt :: (Num a, Eq a) => [a] -> [a]
dropIt = foldr foldDrop []
         foldDrop a [] = case a == 0 of
                         True -> []
                         False ->[a]
         foldDrop a ts  = (a:ts)

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P xs) (P ys) = let newXs = dropIt xs
                             newYs = dropIt ys
                         in newXs == newYs

-- Exercise 3 -----------------------------------------


instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P xs) = fst . foldl showFold ("",0) $ xs
                  where showFold ::(Show a) => (String,Int) -> a ->  (String,Int)
                        showFold  (_,0) a = (show a,1)
                        showFold  (str,1) a = ((show a ++ "x + " ++ str) , 2)
                        showFold  (str,i) a  = ((show a) ++ "x^" ++ (show i) ++ " + " ++ str,(i + 1))

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus = undefined

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined
