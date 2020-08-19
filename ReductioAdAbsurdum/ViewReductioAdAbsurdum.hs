module ReductioAdAbsurdum.ViewReductioAdAbsurdum
  ( viewReductioAdAbsurdumProcess
  ) 
  where


import DataWFF.PropositionalCalculus
import Data.Maybe
  ( isJust
  , fromJust
  )
import ReductioAdAbsurdum.ViewSearchPrepositionsValue 
  ( viewWFFAndYoursValuesFound
  )
import ReductioAdAbsurdum.SearchInfoAboutWFF 
  ( applyingDiscoveredValuesInWFF
  , searchForAbsurdInList
  , getInfoAssumingItsNotTautology
  , hasOnlyKnownPrepositionInWFF 
  , hasOnlyKnownPrepositionInList
  )


viewReductioAdAbsurdumProcess :: WFF Preposition -> String
viewReductioAdAbsurdumProcess wff = 
  viewReductioAdAbsurdumHelper wff initialMenssage
  where
    initialMenssage = "\n"
      ++ "Assuming that the formula is a contradiction we have:"
    viewReductioAdAbsurdumHelper
      :: WFF Preposition
      -> String
      -> String
    viewReductioAdAbsurdumHelper wff menssage
      | hasAbsurd = menssage ++ absurdFoundMenssage
      | breakCondition = menssage ++ notMenssage
      | otherwise = 
        viewReductioAdAbsurdumHelper newWff (menssage ++ otherwiseMenssage)
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


        wffAndValuesFound :: String
        wffAndValuesFound = viewWFFAndYoursValuesFound wff
        notMenssage = wffAndValuesFound 
          ++ "No contradictions were found.\nTherefore, it is not possible to say anything about the validity of the formula."
          ++ "\n"
        absurdFoundMenssage = wffAndValuesFound
          ++ "Two values were found for the preposition '" 
          ++ (show (fromJust absurdFound)) ++ "'.\n"
          ++ "Therefore, the initial formula is a tautology.\n"
        otherwiseMenssage = wffAndValuesFound
          ++ "Applying the result in the formula we have:"
