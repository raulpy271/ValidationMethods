module Tools.StringsManipulation
  ( sublist
  , getIndexOfFirstOperation
  , getIndexOfLastOperation
  , getIndexOfOperands 
  , getComplexity
  , isOnlyEspaces
  , addParenthesis
  , addStringBetweenOtherTwoStrings 
  ) 
  where


import Text.Regex.Posix


type FormulaInString = String


getComplexity :: FormulaInString -> Int
getComplexity p = p =~ "[V^~>=]" :: Int


removeDuplicate string = remove string []
  where
    remove (x:xs) newstring
      | null xs = newstring ++ [x]
      | elem x xs = remove xs newstring
      | otherwise = remove xs (newstring ++ [x])


isOnlyEspaces :: String -> Bool
isOnlyEspaces str = null $ fst $ (!!0) $ lex str 


addParenthesis :: String -> String
addParenthesis string = "(" ++ string ++ ")"


addStringBetweenOtherTwoStrings :: String -> String -> String -> String 
addStringBetweenOtherTwoStrings 
  stringLeft stringRigth stringCenter 
    = stringLeft ++ stringCenter ++ stringRigth


getPreposicoes :: FormulaInString -> String
-- e.g: getPreposicoes "(p > q) = s" >---> "pqs"
getPreposicoes string = 
  if null preposicoes 
    then ""
    else removeDuplicate (convertlist2string preposicoes "")
  where
    preposicoes = string =~ "[a-z]" :: [[String]]
    convertlist2string :: [[String]] -> String -> String
    convertlist2string (x:xs) string
      | null xs = (x !! 0) ++ string
      | otherwise = convertlist2string xs ((x !! 0) ++ string) 


sublist start end text
  | (end > length text) || start < 0 = "" 
  | otherwise = take (end - start) (drop start text)


getIndexOfLastOperation :: FormulaInString -> Int
getIndexOfLastOperation s = 
  (length s - 1) - fst (reverse s =~ "[V^=~>]" :: (Int, Int))
getIndexOfFirstOperation :: FormulaInString -> Int
getIndexOfFirstOperation s = fst (s =~ "[V^>=~]" :: (Int, Int))

getIndexOfOperands :: FormulaInString -> [Int]
getIndexOfOperands string = decompor string 0 0 0 []

-- retorna lista com indices dos operandos
-- e.g.: "(p > q) = (a > c)" -> [0, 6, 10, 16]
--        ^     ^   ^     ^
  where
    
    decompor string index openeds paresFechados list
      | index >= length string = list
      | string !! index == '(' = 
        if openeds == 0 
          then decompor string (index+1) (openeds + 1) paresFechados (list ++ [index])
        else decompor string (index+1) (openeds + 1) paresFechados list
      | string !! index  == ')' = 
        if (openeds - 1) == 0
          then decompor string (index+1) (openeds - 1) (paresFechados+1) (list ++ [index])
        else decompor string (index+1) (openeds -1) paresFechados list
      | otherwise = decompor string (index+1) openeds paresFechados list

