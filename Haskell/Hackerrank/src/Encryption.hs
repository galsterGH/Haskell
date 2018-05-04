import Control.Applicative
import Control.Monad
import System.IO
import Data.Char
import Data.List

removeSpaces :: String -> String
removeSpaces  = foldr remove []
                where remove c acc | isSpace c = acc
                                   | otherwise = (c:acc)

stringToGrid :: String -> Int -> Int -> [String] -> [String]
stringToGrid [] _ _ res = res
stringToGrid str 0 c res = res ++ [take c str]
stringToGrid str r c res = stringToGrid (drop c str) (r - 1) c (res ++ [take c str])

encrypt :: String -> String
encrypt str = let nospaces = removeSpaces str
                  sq = sqrt . fromIntegral . length $ nospaces
                  rows = floor sq
                  cols = ceiling sq
              in  unwords . transpose . stringToGrid nospaces rows cols $ []

main :: IO ()
main = do
    s <- getLine
    putStrLn (encrypt s)