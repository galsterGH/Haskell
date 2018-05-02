module TailOfTwoStacks (tailOfTwoStacks) where

import Data.Maybe
import Data.List
import Data.Monoid

type Stack a = [a]
push :: a -> Stack a -> Stack a
push a stk = (a:stk)

pop :: Stack a ->Stack a
pop [] = []
pop (x:xs) = xs

top :: Stack a -> a
top (x:xs) = x

empty :: Stack a -> Bool
empty [] = True
empty _ = False

newtype PopQ a = PopQ (Stack a)
newtype PushQ a = PushQ (Stack a)

type Queue a = (PushQ a, PopQ a)


emptyQueue :: (Monoid a) => Queue a
emptyQueue = (PushQ mempty , PopQ mempty)

pushq :: a -> Queue a -> Queue a
pushq a ((PushQ stk), popq) = (PushQ . push a $ stk,popq)

popq :: Queue a -> Queue a
popq q@(PushQ [], PopQ []) = q
popq (pq,PopQ (x:xs)) =  (pq,PopQ xs)
popq (PushQ xs, PopQ []) = popq (PushQ [], PopQ . reverse $ xs)

topq :: Queue a -> (Queue a,a)
topq q@(pq, PopQ (x:xs)) = (q,x)
topq (PushQ xs, PopQ []) =  let rev = reverse xs
                            in ((PushQ [], PopQ  rev), head rev)

parseStr :: [String]-> Queue String -> (Queue String,IO())
parseStr []  q = (q,return ())
parseStr ("1":xs) q = (pushq (head xs) q,return ())
parseStr ("2":[]) q = (popq  q, return ())
parseStr ("3":[]) q = putStrLn <$> (topq q)


handleQueries :: Queue String -> [String] ->IO()
handleQueries _ [] = return ()
handleQueries q (x:xs) = let (q',io) = parseStr (words x) q
                         in io >> handleQueries q' xs
tailOfTwoStacks :: IO()
tailOfTwoStacks = do
 numberOfQ <- getLine
 let numOfQInt = (read numberOfQ)::Int
 (sequence. replicate numOfQInt $ getLine) >>= handleQueries emptyQueue
