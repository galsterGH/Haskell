import Data.List
import Data.Char
import           Control.Monad (foldM, replicateM,filterM)
import           System.IO (Handle, IOMode(..), hGetChar, withFile)
import Control.Applicative
import Data.Maybe
import Data.Bits

                                       
data Reader = Reader String Handle

read4 :: Handle -> IO (String)
read4 = replicateM 4 . hGetChar

mkReader h = Reader [] h

readHead :: Reader -> IO (Char,Reader)
readHead (Reader [] h) = do
       str <- read4 h
       return (head $ str, Reader (tail str)  h)
       
readHead (Reader (x:xs) h) = do
  return (x, Reader xs h)
  
readN :: Int -> Handle -> IO (String,Reader)
readN 0 h = return ("",mkReader h)
readN 1 h = readHead (mkReader h) >>= (\(c,r) -> return (show c, r))
readN n h = 
      let newReader = mkReader h
      in readHead newReader >>= (\(c,r) -> (readN (n - 1) h) >>= (\(s1,r1) -> return (c:s1,r1)))
       

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs)  = [(x : l)|l <- powerset xs] ++ (powerset xs)


perms :: [a] -> [[a]]
perms [] = [[]]
perms [a] = [[a]]
perms (x:lst) = let others = perms lst          
                in concatMap (interleave x) others     
                where interleave x [] = [[x]]
                      interleave x (y:ys) = [(x:(y:ys))] ++
                                            map (y:) (interleave x ys)
                                            
type Prime = Int
areFactors :: [Prime]->Int-> Bool
areFactors _ 1 = True
areFactors [] n = False 
areFactors primes@(x:xs) n | n `mod` x == 0 = areFactors primes (n `div` x)
                           | otherwise = areFactors xs n

     
buildFactors :: Int->[Prime]-> ([Prime]->Int -> Bool)->Int
buildFactors n primes with = last . take n . filter (with primes) $ [1..]


data Expression = Empty |
                  Number {num :: Int} |
                  Plus {expr1 :: Expression,
                        expr2 :: Expression} |
                  Minus {expr1 :: Expression,
                         expr2 :: Expression} |
                  Mult {expr1 :: Expression,
                        expr2 :: Expression}
                         
                          
instance Show Expression where
  show (Number n)  = show $ n
  show (Plus e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
  show (Minus e1 e2) = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"
  show (Mult e1 e2) = "(" ++ show e1  ++ "*" ++ show e2 ++ ")"
  show Empty = ""

                 
makeNumber :: Int -> Expression
makeNumber n = Number n

makePlus e1 e2 = Plus e1 e2
makeMinus e1 e2 = Minus e1 e2
makeMult e1 e2 = Mult e1 e2


apply :: Expression -> Int
apply (Number x) = x
apply (Plus e1 e2) = apply e1 + apply e2
apply (Minus e1 e2) = apply e1 - apply e2
apply (Mult e1 e2) = apply e1 * apply e2

getExps :: String -> [Expression]
getExps [] = [Empty]
getExps str = let prefixes = init . tail . inits $ str
                  tls    = tail . init . tails $ str
                  nums   = map (read::String->Int) prefixes
                  ziped  = zipWith (,) nums tls                            
              in  makeNumber (read str :: Int):(concatMap applyIt ziped)
              where applyIt (i,rst) = map (\o -> o.makeNumber $ i) ops <*> (getExps rst) 
                    ops = [makePlus,makeMinus,makeMult] 
                    
addOps :: String -> Int -> [Expression]
addOps str target = filter (\e -> apply e == target) . getExps  $ str 


subsetSum ::[Int]->Int-> [Maybe [Int]]
subsetSum xs n = subsetSum' (sort xs) n
subsetSum' :: [Int] -> Int ->  [Maybe [Int]]
subsetSum' _ 0 =  [Just []]
subsetSum' [] t = [Nothing]
subsetSum' lst@(x:xs) t  | t < 0 = [Nothing]
                         | otherwise =  
                           let ss1 = filter isJust $ subsetSum' xs t
                               ss2 = filter isJust [Just (x:) <*> y| y <- subsetSum' lst (t - x)]
                           in  ss1 ++ ss2
                           


type PhoneNumMap = [(Int,String)]
buildPhone :: PhoneNumMap
buildPhone = [(2,"abc"),(3,"def"),(4,"ghi"),(5,"jkl"),(6,"mno"),(7,"pqrs"),(8,"tuv"),(9,"wxyz")]

getAllLetterComb :: String -> PhoneNumMap -> [String]
getAllLetterComb [] _ = [""]
getAllLetterComb _ [] = []
getAllLetterComb (x:xs) pnmap = let str = findInMap pnmap . digitToInt $ x
                                    rec = getAllLetterComb xs pnmap
                                in [(c:r)| c <- str, r <- rec] 
                                where findInMap :: PhoneNumMap -> Int -> String
                                      findInMap [] _  = ""
                                      findInMap ((y,str1):ys) x | x == y = str1
                                                                | otherwise = findInMap ys x
                                                                


toList 0 = [0]
toList x = case x `div` 10 == 0 of
            True-> [x `mod` 10]
            False -> toList (x `div` 10) ++ [x `mod` 10]
            
            
fromList :: [Int] -> Int
fromList = foldl (\acc x -> acc *10 + x) 0  
            
bitLXor a b = let aLst = toList a
                  bLst = toList b
              in fromList . zipWith (\x y -> x `xor` y) aLst $ bLst
            
                   