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
  )


viewReductioAdAbsurdumProcess :: WFF Preposition -> String
viewReductioAdAbsurdumProcess wff = 
  viewReductioAdAbsurdumHelper wff initialMenssage
  where
    initialMenssage = "\n"
      ++ "Assuming that the formula is a contradiction we get:"
    viewReductioAdAbsurdumHelper
      :: WFF Preposition
      -> String
      -> String
    viewReductioAdAbsurdumHelper wff menssage
      | null listOfPairPrepositionValueFound = menssage ++ notMenssage
      | hasAbsurd = menssage ++ absurdFoundMenssage
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
