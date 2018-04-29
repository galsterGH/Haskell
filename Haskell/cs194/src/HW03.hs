module HW03 where

import Debug.Trace (trace)

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend state str i = (\s-> case s==str of
                            True -> i
                            _ -> state s)


empty :: State
empty = (\_->0 )

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE  _ (Val x) = x
evalE state (Var x) = state x
evalE state (Op e1 b e2) = let ev1 =  evalE state e1
                               ev2 =  evalE state e2
                               operatorMap = [(Plus,(+)),
                                              (Minus,(-)),
                                              (Times,(*)),
                                              (Divide, (div)),
                                              (Gt, (\x y-> if x > y then 1 else 0)),
                                              (Ge, (\x y -> if x >= y then 1 else 0)),
                                              (Lt, (\x y -> if x < y then 1 else 0)),
                                              (Le, (\x y -> if x <= y then 1 else 0)),
                                              (Eql,(\x y -> if x == y then 1 else 0))]
                               actualOp = lookUp operatorMap b
                          in actualOp ev1 ev2
                          where lookUp :: [(Bop,(Int->Int->Int))] -> Bop-> (Int->Int->Int)
                                lookUp [] _ = (\x y -> 0)
                                lookUp ((a,b):xs) bop = case (a == bop) of
                                                          True-> b
                                                          False -> lookUp xs bop

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign s e) = DAssign s $ e
desugar (If e s1 s2) = DIf e (desugar s1) $ (desugar s2)
desugar (While e s)  =  DWhile e $ (desugar s)
desugar (Sequence s1 s2) = DSequence (desugar s1) $ (desugar s2)
desugar Skip = DSkip
desugar (Incr str) = DAssign str (Op (Var str) Plus (Val 1))
desugar (For s1 e s2 s3) = DSequence (desugar s1) (DWhile e $ DSequence (desugar s3) (desugar s2))

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign s e) = let evaluated = evalE state e
                                 in extend state s evaluated

evalSimple state (DIf e s1 s2) = let eval = evalE state e
                                 in case eval of
                                    0 -> evalSimple state s2
                                    _ -> evalSimple state s1

--DWhile (Op (Var "a") Gt (Var "b")) (DAssign "a" (Op (Var "b") Times (Val 3)))
evalSimple state (DWhile e s1) = let eval = evalE state e
                                 in  case eval of
                                     0 -> state
                                     _ -> evalSimple (evalSimple state s1) (DWhile e s1)

evalSimple state (DSequence s1 s2) = evalSimple (evalSimple state s1) s2
evalSimple state DSkip = state

run :: State -> Statement -> State
run state stmt = evalSimple state (desugar stmt)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
