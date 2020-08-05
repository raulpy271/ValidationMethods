module Tools.DataWFFManipulation 
  ( mapWFF
  , getAllAtomicPreposition 
  , replacePrepositionInFormula 
  , getUnkownPreposition 
  , getUnkownPrepositionInWFF 
  )
  where

import DataWFF.PropositionalCalculus


mapWFF :: (Preposition -> Preposition) -> WFF Preposition -> WFF Preposition
mapWFF f (Atomic preposition) = Atomic (f preposition)
mapWFF f (Not wff)            = Not (mapWFF f wff)
mapWFF f (And left rigth)     = And (mapWFF f left) (mapWFF f rigth)
mapWFF f (Or left rigth)      = Or (mapWFF f left) (mapWFF f rigth)
mapWFF f (Iff left rigth)     = Iff (mapWFF f left) (mapWFF f rigth)
mapWFF f (Imply left rigth)   = Imply (mapWFF f left) (mapWFF f rigth)


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
