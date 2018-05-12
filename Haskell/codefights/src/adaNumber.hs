import Data.Char
data BaseType = T | Th | Fo | Fi | Si | Se | E | N | Te |El | Tw |Thi |Ft | Fvt | Sxt | Bad deriving (Show, Eq)

lu = [(T,"01"),(Th,"02"),(Fo,"03"),(Fi,"04"),(Si,"05"),(Se,"06"),(E,"07"),(N,"08"),(Te,"09"),(El,"0a"),(Tw,"0b"),(Thi,"0c"),(Ft,"0d"),(Fvt,"0e"),(Sxt,"0f")]

isLegalChar :: (Maybe String) -> Char -> Maybe Bool
isLegalChar base char  = base >>= compareRange char

compareRange :: Char -> String -> Maybe Bool
compareRange '_' _ = (Just True)
compareRange c (x:xs) =  let ordc = (ord c)
                             ordx = (ord x)
                             ordy = (ord . head $ xs)
                         in if ordc >= ordx && ordc <= ordy then (Just True) else Nothing

adaNumber line = let (base,string) = getBaseAndString line
                     actualBase = lookup (getBase . getBaseTrimed $ base) lu
                 in case mapM (isLegalChar actualBase) (checkString string) of
                    (Just _) -> True
                    Nothing -> False

checkString :: String -> String
checkString [] = "ppp"
checkString ('_':xs) = checkString xs
checkString (x:[]) =  [x]
checkString (x:xs) = (x:checkString xs)

getBaseAndString :: String -> (String,String)
getBaseAndString str = case foldl (onFold) (([],[]),-1) str of
                      ((b,s),-1) -> (b,s)
                      (([],s),1) -> ("100",s)
                      ((b,s),1) -> (b,s)
                      ((b,s),0) -> ("100",s)

  where onFold ((b,s),-1) '#' = ((s,[]),0)
        onFold ((b,s),0)  '#' = ((b,s),1)
        onFold ((b,s),-1) c = ((b,s ++ [c]),-1)
        onFold ((b,s),0) c = ((b,s ++ [c]),0)
        onFold ((b,s),1) c = (("100",s ++ [c]),1)

getBaseTrimed :: String ->String
getBaseTrimed [] = []
getBaseTrimed ('_':xs) = xs
getBaseTrimed (x:xs) = (x:getBaseTrimed xs)
getBase :: String -> BaseType
getBase [] = Te
getBase "2" = T
getBase "3" = Th
getBase "4" = Fo
getBase "5" = Fi
getBase "6" = Si
getBase "7" = Se
getBase "8" = E
getBase "9" = N
getBase "10" = Te
getBase "11" = El
getBase "12" = Tw
getBase "13" = Thi
getBase "14" = Ft
getBase "15" = Fvt
getBase "16" = Sxt
getBase  _ = Bad
