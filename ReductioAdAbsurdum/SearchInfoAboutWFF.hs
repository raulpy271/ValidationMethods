module ReductioAdAbsurdum.SearchInfoAboutWFF
  ( PairOfPrepositionAndYourValue 
  , applyingDiscoveredValuesInWFF 
  , searchForAbsurdInList
  , searchValuesOfPrepositionsInTautology 
  , searchValuesOfPrepositionsInContradiction
  , getInfoAssumingItsNotTautology 
  )
  where


import DataWFF.PropositionalCalculus
import DataWFF.Interpretation (interpretation)
import Tools.DataWFFManipulation
  ( truePrepositionValue
  , falsePrepositionValue
  , negateWFF
  , replacePrepositionInFormula
  )


type PairOfPrepositionAndYourValue = (Preposition, Preposition)


getInfoAssumingItsNotTautology 
  :: WFF Preposition -> [PairOfPrepositionAndYourValue]
getInfoAssumingItsNotTautology = 
  (searchValuesOfPrepositionsInTautology . negateWFF)


applyingDiscoveredValuesInWFF 
  :: WFF Preposition 
  -> [PairOfPrepositionAndYourValue]
  -> WFF Preposition 
applyingDiscoveredValuesInWFF wff [] = wff
applyingDiscoveredValuesInWFF wff (firstPair:otherPairs) = 
  applyingDiscoveredValuesInWFF newWff otherPairs 
    where 
      newWff =
        ( replacePrepositionInFormula wff
          ( fst firstPair) 
          ( snd firstPair) 
        )


searchForAbsurdInList
  :: [PairOfPrepositionAndYourValue] -> Maybe Preposition
searchForAbsurdInList listPairPrepostionValue =
  case maybeListOfAbsurd of
    Nothing -> Nothing
    Just listOfAbsurd -> Just 
      (getFirstPrepositionInListOfAbsurd listOfAbsurd)
  where
    maybeListOfAbsurd = searchHelper listPairPrepostionValue


    searchHelper 
      :: [PairOfPrepositionAndYourValue] 
      -> Maybe [PairOfPrepositionAndYourValue]
    searchHelper [] = Nothing
    searchHelper (fistPair:otherPairs) =
      if null listOfAbsurdsFound 
      then searchHelper otherPairs
      else Just listOfAbsurdsFound
      where
        listOfAbsurdsFound = filter searchAbsurdInFistPair otherPairs 
        checksIfTwoPairsHaveTheSamePrepositionButDifferentValue
          :: PairOfPrepositionAndYourValue
          -> PairOfPrepositionAndYourValue
          -> Bool
        checksIfTwoPairsHaveTheSamePrepositionButDifferentValue 
          = (\x y -> ((fst x) == (fst y)) && ( (snd x)  /= (snd y)) ) 
        searchAbsurdInFistPair =
          checksIfTwoPairsHaveTheSamePrepositionButDifferentValue fistPair
          
    getFirstPrepositionInListOfAbsurd = (\x -> fst (x !! 0) )


searchValuesOfPrepositionsInTautology 
  :: WFF Preposition -> [PairOfPrepositionAndYourValue]
searchValuesOfPrepositionsInTautology (Atomic preposition) = 
  case preposition of
    PrepositionWithKnownValue _ _ -> []
    PrepositionWithUnknownValue char -> 
      [ (preposition, truePrepositionValue ) ]


searchValuesOfPrepositionsInTautology (Not wff) 
  = searchValuesOfPrepositionsInContradiction wff


searchValuesOfPrepositionsInTautology (And left rigth)
  = searchValuesOfPrepositionsInTautology left
  ++ searchValuesOfPrepositionsInTautology rigth


searchValuesOfPrepositionsInTautology (Or left rigth) = 
  if leftInterpretation == falsePrepositionValue
  then searchValuesOfPrepositionsInTautology rigth
  else 
    if rigthInterpretation == falsePrepositionValue 
    then searchValuesOfPrepositionsInTautology left
    else []
  where
    (leftInterpretation, rigthInterpretation) = 
      ( interpretation left
      , interpretation rigth
      )


searchValuesOfPrepositionsInTautology (Iff left rigth) =
  if leftInterpretation == truePrepositionValue
  then searchValuesOfPrepositionsInTautology rigth
  else 
    if leftInterpretation == falsePrepositionValue
    then searchValuesOfPrepositionsInContradiction rigth
    else
      if rigthInterpretation == truePrepositionValue
      then searchValuesOfPrepositionsInTautology left
      else 
        if rigthInterpretation == falsePrepositionValue
        then searchValuesOfPrepositionsInContradiction left
        else []
  where
    (leftInterpretation, rigthInterpretation) = 
      ( interpretation left
      , interpretation rigth
      )


searchValuesOfPrepositionsInTautology (Imply left rigth) =
  if leftInterpretation == truePrepositionValue
  then searchValuesOfPrepositionsInTautology rigth
  else 
    if rigthInterpretation == falsePrepositionValue
    then searchValuesOfPrepositionsInContradiction left
    else []
  where
    (leftInterpretation, rigthInterpretation) = 
      ( interpretation left
      , interpretation rigth
      )


searchValuesOfPrepositionsInContradiction
  :: WFF Preposition -> [PairOfPrepositionAndYourValue]
searchValuesOfPrepositionsInContradiction (Atomic preposition) =
  case preposition of
    PrepositionWithKnownValue _ _ -> []
    PrepositionWithUnknownValue char -> 
      [ (preposition, falsePrepositionValue) ]


searchValuesOfPrepositionsInContradiction (Not wff)
  = searchValuesOfPrepositionsInTautology wff


searchValuesOfPrepositionsInContradiction (And left rigth) =
  if leftInterpretation == truePrepositionValue
  then searchValuesOfPrepositionsInContradiction rigth
  else 
    if rigthInterpretation == truePrepositionValue
    then searchValuesOfPrepositionsInContradiction left
    else []
  where
    (leftInterpretation, rigthInterpretation) = 
      ( interpretation left
      , interpretation rigth
      )


searchValuesOfPrepositionsInContradiction (Or left rigth) 
  = (searchValuesOfPrepositionsInContradiction left) 
  ++ (searchValuesOfPrepositionsInContradiction rigth)


searchValuesOfPrepositionsInContradiction (Iff left rigth) =
  if leftInterpretation == truePrepositionValue
  then searchValuesOfPrepositionsInContradiction rigth
  else 
    if leftInterpretation == falsePrepositionValue
    then searchValuesOfPrepositionsInTautology rigth
    else
      if rigthInterpretation == truePrepositionValue
      then searchValuesOfPrepositionsInContradiction left
      else 
        if rigthInterpretation == falsePrepositionValue
        then searchValuesOfPrepositionsInTautology left
        else []
  where
    (leftInterpretation, rigthInterpretation) = 
      ( interpretation left
      , interpretation rigth
      )


searchValuesOfPrepositionsInContradiction (Imply left rigth)
  = (searchValuesOfPrepositionsInTautology left)
  ++ (searchValuesOfPrepositionsInContradiction rigth)


