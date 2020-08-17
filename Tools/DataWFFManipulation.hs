module Tools.DataWFFManipulation 
  ( mapWFF
  , foldWFF
  , getAllAtomicPreposition 
  , replacePrepositionInFormula 
  , getUnkownPreposition 
  , getUnkownPrepositionInWFF 
  , truePrepositionValue
  , falsePrepositionValue
  , negateWFF 
  , hasUnknownPrepositionInWFF
  , hasKnownPrepositionInWFF
  )
  where

import DataWFF.PropositionalCalculus


(truePrepositionValue, falsePrepositionValue) = 
  ( PrepositionWithKnownValue '1' True
  , PrepositionWithKnownValue '0' False
  )


mapWFF :: (Preposition -> Preposition) -> WFF Preposition -> WFF Preposition
mapWFF f (Atomic preposition) = Atomic (f preposition)
mapWFF f (Not wff)            = Not (mapWFF f wff)
mapWFF f (And left rigth)     = And (mapWFF f left) (mapWFF f rigth)
mapWFF f (Or left rigth)      = Or (mapWFF f left) (mapWFF f rigth)
mapWFF f (Iff left rigth)     = Iff (mapWFF f left) (mapWFF f rigth)
mapWFF f (Imply left rigth)   = Imply (mapWFF f left) (mapWFF f rigth)


foldWFF 
  :: (Preposition -> Preposition -> Preposition) 
  -> Preposition 
  -> WFF Preposition 
  ->  Preposition 
foldWFF f acumulator (Atomic preposition) = f acumulator preposition
foldWFF f acumulator (Not wff) = foldWFF f acumulator wff
foldWFF f acumulator (And left rigth) = 
  foldWFF f (foldWFF f acumulator left) rigth
foldWFF f acumulator (Or left rigth) = 
  foldWFF f (foldWFF f acumulator left) rigth
foldWFF f acumulator (Iff left rigth) = 
  foldWFF f (foldWFF f acumulator left) rigth
foldWFF f acumulator (Imply left rigth) = 
  foldWFF f (foldWFF f acumulator left) rigth


getAllAtomicPreposition :: WFF Preposition -> [Preposition]
getAllAtomicPreposition wff = getAllPrepositionHelper wff []


getAllPrepositionHelper :: WFF Preposition -> [Preposition] -> [Preposition]
getAllPrepositionHelper (Atomic preposition) listOfPrepositionFound 
  | elem preposition listOfPrepositionFound = listOfPrepositionFound
  | otherwise = preposition:listOfPrepositionFound


getAllPrepositionHelper (Not preposition) listOfPrepositionFound
  = getAllPrepositionHelper preposition listOfPrepositionFound


getAllPrepositionHelper (And  left rigth) listOfPrepositionFound
  = (getAllPrepositionHelper left 
      (getAllPrepositionHelper rigth listOfPrepositionFound)
    )


getAllPrepositionHelper (Or left rigth) listOfPrepositionFound
  = (getAllPrepositionHelper left 
      (getAllPrepositionHelper rigth listOfPrepositionFound)
    )


getAllPrepositionHelper (Iff left rigth) listOfPrepositionFound
  = (getAllPrepositionHelper left 
      (getAllPrepositionHelper rigth listOfPrepositionFound)
    )


getAllPrepositionHelper (Imply left rigth) listOfPrepositionFound
  = (getAllPrepositionHelper left 
      (getAllPrepositionHelper rigth listOfPrepositionFound)
    )



replacePrepositionInFormula 
  :: WFF Preposition -> Preposition  -> Preposition -> WFF Preposition 
replacePrepositionInFormula wff prepositionToChange newPrepostion 
  = mapWFF replacePreposition wff
  where
    replacePreposition :: Preposition -> Preposition 
    replacePreposition preposition = 
      if preposition == prepositionToChange
      then newPrepostion
      else preposition 


getUnkownPreposition :: [Preposition] -> [Preposition]
getUnkownPreposition [] = []
getUnkownPreposition 
  ((PrepositionWithKnownValue _ _ ):xs) = getUnkownPreposition xs
getUnkownPreposition (x:xs) = x : (getUnkownPreposition xs)


getUnkownPrepositionInWFF :: WFF Preposition -> [Preposition]
getUnkownPrepositionInWFF = (getUnkownPreposition . getAllAtomicPreposition)


negateWFF :: WFF Preposition -> WFF Preposition  
negateWFF wff = Not (wff)


convertPrepositionToBool :: Preposition -> Bool
convertPrepositionToBool (PrepositionWithKnownValue _ True) = True
convertPrepositionToBool _ = False


hasKnownPreposition :: Preposition -> Bool
hasKnownPreposition (PrepositionWithKnownValue _ _ ) = True
hasKnownPreposition _ = False
hasKnownPreposition2 left rigth 
  = (hasKnownPreposition left) 
  || (hasKnownPreposition rigth)


hasKnownPrepositionInWFF :: WFF Preposition -> Bool
hasKnownPrepositionInWFF wff 
  = convertPrepositionToBool 
  $ foldWFF checkIfHasPrepositionAndConvertBoolToPreposition identity wff 
  where
    identity = PrepositionWithUnknownValue 'i'
    getKnownPrepositionIfParameterIsTrue True = truePrepositionValue
    getKnownPrepositionIfParameterIsTrue _ = identity
    checkIfHasPrepositionAndConvertBoolToPreposition 
      :: Preposition -> Preposition -> Preposition
    checkIfHasPrepositionAndConvertBoolToPreposition left rigth 
      = getKnownPrepositionIfParameterIsTrue
      $ (hasKnownPreposition2 left rigth)


hasUnknownPreposition2 left rigth
  = (not $ hasKnownPreposition left)
  || (not $ hasKnownPreposition rigth)


hasUnknownPrepositionInWFF :: WFF Preposition -> Bool
hasUnknownPrepositionInWFF wff
  = not
  $ convertPrepositionToBool
  $ foldWFF checkIfHasPrepositionAndConvertBoolToPreposition identity wff 
  where
    identity = truePrepositionValue
    getUnkownPrepositionIfParameterIsTrue True = 
      PrepositionWithUnknownValue 's'
    getUnkownPrepositionIfParameterIsTrue _ = identity
    checkIfHasPrepositionAndConvertBoolToPreposition 
      :: Preposition -> Preposition -> Preposition
    checkIfHasPrepositionAndConvertBoolToPreposition left rigth 
      = getUnkownPrepositionIfParameterIsTrue
      $ (hasUnknownPreposition2 left rigth)

