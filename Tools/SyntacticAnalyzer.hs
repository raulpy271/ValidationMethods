module Tools.SyntacticAnalyzer (isValid) where


import Tools.StringsManipulation
  ( sublist
  , getIndexOfOperands
  , getComplexity
  )


type FormulaInString = String


removeOperands string

--retorna o que est� fora de parentese
-- necess�rio para identificar a quantidade de opera��es
--e.g.: removeOperands "(p > a) > (acsa) = (ewd) " -> ">="

  | null indexs = string
  | lengthIndexs == 2 =
    sublist 0 (indexs !! 0) string ++
    sublist (indexs !! 1 + 1) (length string) string 
  | lengthIndexs == 4 =
    sublist 0 (indexs !! 0) string ++
    sublist (indexs !! 1 + 1) (indexs !! 2) string ++
    sublist (indexs !! 3 + 1) (length string) string 
  | otherwise = "erro"
  where
    indexs = getIndexOfOperands string
    lengthIndexs = length indexs


isValid :: FormulaInString -> Bool
isValid string = analiseFinal
  where
    
    analise = analisar (string ++ " ") ' ' 0
    analiseFinal = analise && 
      if analise 
        then correctOperands string 
      else False
    
    isConec char = elem char ">V^="
    isPrep char = elem char ['a'..'z']  || elem char "01X"
    
    analisar :: String -> Char -> Int -> Bool
    analisar (x:xs) lastChar openeds
      | null xs = if openeds /= 0 || isConec lastChar then False else True
      | x == ' ' = analisar xs lastChar openeds
      | x == '~' = 
        if lastChar == '(' || lastChar == ' '
          then analisar xs x openeds 
        else False 
      | lastChar == '~' = 
        if isPrep x 
          then analisar xs x openeds 
        else 
          if x == '(' 
            then analisar xs x (openeds+1) 
          else False
      | x == '(' = 
        if lastChar == ')' || isPrep lastChar 
          then False 
        else analisar  xs x (openeds + 1)
      | x == ')' = 
        if lastChar == '(' || isConec lastChar || lastChar == '~' 
          then False 
        else analisar xs x (openeds - 1)
      | isConec x = 
        if lastChar == '(' || lastChar == ' ' || isConec lastChar 
          then False 
        else analisar xs x openeds
      | otherwise = 
        if isValidChar x && not (isPrep lastChar) 
          then analisar xs x openeds 
          else False
    isValidChar char = isConec char || isPrep char
    

    correctOperands :: String -> Bool
    correctOperands string

    -- verifica se h� a quantidade correta de operadores
    --e.g: "(p > a) = (b V a) > (a = s)" retornaria false 
    -- porque h� mais de um operandor fora de parentese, o que causaria ambiguidade na momento de fazer a opera��o 

      | null indexOperands' = if complexity > 1 then False else True
      | complexity > 1 || newstring == "erro" = False
      | length indexOperands' == 2 = 
        correctOperands (sublist (indexOperands' !! 0 + 1) (indexOperands' !! 1) string)
      | otherwise = correctOperands (sublist (indexOperands' !! 0 + 1) (indexOperands' !! 1) string) &&
        correctOperands (sublist (indexOperands' !! 2 + 1) (indexOperands' !! 3) string)
      where
        indexOperands' = getIndexOfOperands string
        newstring = removeOperands string
        complexity = getComplexity newstring

