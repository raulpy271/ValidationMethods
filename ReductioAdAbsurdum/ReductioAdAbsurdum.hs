module ReductioAdAbsurdum.ReductioAdAbsurdum
  ( isTautology
  , viewReductioAdAbsurdumProcess
  )
  where


import DataWFF.PropositionalCalculus
import Data.Maybe (isJust)
import ReductioAdAbsurdum.ViewReductioAdAbsurdum 
  ( viewReductioAdAbsurdumProcess
  )
import ReductioAdAbsurdum.SearchInfoAboutWFF
  ( PairOfPrepositionAndYourValue 
  , getInfoAssumingItsNotTautology 
  , applyingDiscoveredValuesInWFF 
  , searchForAbsurdInList
  , hasOnlyKnownPrepositionInWFF 
  )


isTautology ::  WFF Preposition -> WFFProperties
isTautology wff
  | isJust maybeAbsurd = Tautology
  | otherwise = Unknown
  where maybeAbsurd = searchForAbsurdInWFF wff


searchForAbsurdInWFF :: WFF Preposition -> Maybe Preposition
searchForAbsurdInWFF wff  
  | hasAbsurd = absurdFound
  | (null listOfPairPrepositionValueFound)
    || (hasOnlyKnownPrepositionInWFF wff) = Nothing
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


