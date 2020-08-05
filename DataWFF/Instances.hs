{-# LANGUAGE FlexibleInstances #-}
module DataWFF.Instances 
  ( FormulaInString(..)
  , Preposition(..)
  , WFF(..)
  , WFFProperties(..)
  , readWFF
  )
  where


import DataWFF.Datatypes
import Tools.Decompose ( decomposeFormulaInString)
import Tools.SyntacticAnalyzer (isValid)
import Tools.StringsManipulation 
  ( isOnlyEspaces
  , addParenthesis
  , addStringBetweenOtherTwoStrings 
  )

instance Eq Preposition
  where
    (==) (PrepositionWithUnknownValue leftChar)
         (PrepositionWithUnknownValue rigthChar)
         = leftChar == rigthChar
    (==) (PrepositionWithKnownValue _ leftValue)
         (PrepositionWithKnownValue _ rigthValue)
         = leftValue == rigthValue
    (==) _ _ = False 


instance Show Preposition
  where
    show (PrepositionWithUnknownValue char)= ([char])
    show (PrepositionWithKnownValue _ True) = "1"
    show (PrepositionWithKnownValue _ False) = "0"


instance  Show (WFF Preposition)
  where
    show (Atomic preposition) = show preposition
    show wff = case wff of
      Not preposition -> 
        notSymbol ++ (addParenthesis (show preposition))
      And   prepositionLeft prepositionRigth -> 
        addStringBetweenOtherTwoStrings 
        (addParenthesis (show prepositionLeft)) 
        (addParenthesis (show prepositionRigth)) addSymbol
      Or prepositionLeft prepositionRigth -> 
        addStringBetweenOtherTwoStrings 
        (addParenthesis (show prepositionLeft)) 
        (addParenthesis (show prepositionRigth)) orSymbol
      Iff prepositionLeft prepositionRigth -> 
        addStringBetweenOtherTwoStrings 
        (addParenthesis (show prepositionLeft)) 
        (addParenthesis (show prepositionRigth)) iffSymbol
      Imply prepositionLeft prepositionRigth -> 
        addStringBetweenOtherTwoStrings 
        (addParenthesis (show prepositionLeft)) 
        (addParenthesis (show prepositionRigth)) implySymbol
      where
        notSymbol   = " ~ "
        addSymbol   = " ^ "
        orSymbol    = " V "
        iffSymbol   = " = "
        implySymbol = " > "


readWFF :: String -> WFF Preposition
readWFF str = case maybeWFF of
  Just preposition -> preposition 
  Nothing -> error "Invalid Formula"
  where
    maybeWFF :: Maybe (WFF Preposition)
    maybeWFF = readWFFMaybe str


readWFFMaybe :: String -> Maybe (WFF Preposition)
readWFFMaybe input = 
  if isValid input 
  then Just (readWFFHelper input)
  else Nothing 


readWFFHelper :: String -> WFF Preposition
readWFFHelper input
  | firstLexeme == "~" = Not (
      readWFFHelper leftOperand)
  | isAtomicFormula = Atomic (
      putCharInPreposition (firstLexeme !! 0))
  | elem '^' operation = And 
      (readWFFHelper leftOperand) (readWFFHelper rigthOperand)
  | elem 'V' operation = Or 
      (readWFFHelper leftOperand) (readWFFHelper rigthOperand)
  | elem '=' operation = Iff
      (readWFFHelper leftOperand) (readWFFHelper rigthOperand)
  | elem '>' operation = Imply
      (readWFFHelper leftOperand) (readWFFHelper rigthOperand)
  where 
    decomposedFormula = decomposeFormulaInString input 
    firstLexeme = fst $ (lex input) !! 0
    isAtomicFormula = isOnlyEspaces $ snd $ (lex input) !! 0
    (leftOperand, operation, rigthOperand) =
      ( decomposedFormula !! 0
      , decomposedFormula !! 1
      , decomposedFormula !! 2
      )
  

putCharInPreposition :: Char -> Preposition
putCharInPreposition char
  | char == '0' = PrepositionWithKnownValue '0' False
  | char == '1' = PrepositionWithKnownValue '1' True
  | otherwise = PrepositionWithUnknownValue char

