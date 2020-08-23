module ReductioAdAbsurdum.ReductioAdAbsurdum
  ( isTautology
  , laws
  , viewReductioAdAbsurdumProcess
  )
  where


import DataWFF.PropositionalCalculus
import Data.Maybe (isJust)
import ReductioAdAbsurdum.ViewLaws(laws)
import ReductioAdAbsurdum.ViewReductioAdAbsurdum 
  ( viewReductioAdAbsurdumProcess
  )
import ReductioAdAbsurdum.SearchInfoAboutWFF
  ( PairOfPrepositionAndYourValue 
  , getInfoAssumingItsNotTautology 
  , applyingDiscoveredValuesInWFF 
  , searchForAbsurdInList
  , hasOnlyKnownPrepositionInWFF 
  , hasOnlyKnownPrepositionInList
  )


isTautology ::  WFF Preposition -> WFFProperties
isTautology wff
  | isJust maybeAbsurd = Tautology
  | otherwise = Unknown
  where maybeAbsurd = searchForAbsurdInWFF wff


searchForAbsurdInWFF :: WFF Preposition -> Maybe Preposition
searchForAbsurdInWFF wff  
  | hasAbsurd = absurdFound
  | breakCondition = Nothing
  | otherwise = searchForAbsurdInWFF newWff
  where
    newWff = 
      applyingDiscoveredValuesInWFF wff 
      listOfPairPrepositionValueFound
    listOfPairPrepositionValueFound = 
      getInfoAssumingItsNotTautology wff
    absurdFound = 
      searchForAbsurdInList listOfPairPrepositionValueFound
    hasAbsurd = isJust absurdFound
    breakCondition 
      =  (null listOfPairPrepositionValueFound)
      || (hasOnlyKnownPrepositionInWFF wff)
      || ( hasOnlyKnownPrepositionInList 
           $ map fst listOfPairPrepositionValueFound
         )


