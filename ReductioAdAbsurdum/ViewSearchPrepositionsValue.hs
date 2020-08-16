module ReductioAdAbsurdum.ViewSearchPrepositionsValue
  ( getInfoAssumingItsNotTautology
  , viewSearchValuesOfPrepositionsInContradiction
  , viewSearchValuesOfPrepositionsInTautology
  , viewWFFAndYoursValuesFound 
  )
  where

import DataWFF.PropositionalCalculus
import DataWFF.Interpretation (interpretation)
import Tools.DataWFFManipulation
  ( truePrepositionValue
  , falsePrepositionValue
  )


viewWFFAndYoursValuesFound :: WFF Preposition -> String
viewWFFAndYoursValuesFound wff = "\n" 
  ++ (show wff) ++ "\n" 
  ++ (getInfoAssumingItsNotTautology wff)
  ++ "\n"


getInfoAssumingItsNotTautology :: WFF Preposition -> String
getInfoAssumingItsNotTautology wff =
  viewSearchValuesOfPrepositionsInContradiction wff


viewSearchValuesOfPrepositionsInTautology 
  :: WFF Preposition -> String
viewSearchValuesOfPrepositionsInTautology (Atomic preposition) = "T"


viewSearchValuesOfPrepositionsInTautology (Not wff) 
  = " T  " ++ (viewSearchValuesOfPrepositionsInContradiction wff) ++ " "


viewSearchValuesOfPrepositionsInTautology (And left rigth)
  = " " ++ (viewSearchValuesOfPrepositionsInTautology left)
  ++ "  T  "
  ++ (viewSearchValuesOfPrepositionsInTautology rigth) ++ " "


viewSearchValuesOfPrepositionsInTautology (Or left rigth) = 
  if leftInterpretation == falsePrepositionValue
  then 
    " " ++ (addEmptyEspaces left)
    ++ "  T  "
    ++ (viewSearchValuesOfPrepositionsInTautology rigth) ++ " "
  else 
    if rigthInterpretation == falsePrepositionValue 
    then 
      " " ++ (viewSearchValuesOfPrepositionsInTautology left)
      ++ "  T  "
      ++ (addEmptyEspaces rigth) ++ " "
    else
      " " ++ (addEmptyEspaces left)
      ++ "  T  "
      ++ (addEmptyEspaces rigth) ++ " "
  where
    (leftInterpretation, rigthInterpretation) = 
      ( interpretation left
      , interpretation rigth
      )


viewSearchValuesOfPrepositionsInTautology (Iff left rigth) =
  if leftInterpretation == truePrepositionValue
  then 
    " " ++ (addEmptyEspaces left)
    ++ "  T  "
    ++ (viewSearchValuesOfPrepositionsInTautology rigth) ++ " "
  else 
    if leftInterpretation == falsePrepositionValue
    then 
      " " ++ (addEmptyEspaces left)
      ++ "  T  "
      ++ (viewSearchValuesOfPrepositionsInContradiction rigth) ++ " "
    else
      if rigthInterpretation == truePrepositionValue
      then 
        " " ++ (viewSearchValuesOfPrepositionsInTautology left)
        ++ "  T  "
        ++ (addEmptyEspaces rigth) ++ " "
      else 
        if rigthInterpretation == falsePrepositionValue
        then
          " " ++ (viewSearchValuesOfPrepositionsInContradiction left)
          ++ "  T  "
          ++ (addEmptyEspaces rigth) ++ " "
        else
          " " ++ (addEmptyEspaces left)
          ++ "  T  "
          ++ (addEmptyEspaces rigth) ++ " "
  where
    (leftInterpretation, rigthInterpretation) = 
      ( interpretation left
      , interpretation rigth
      )


viewSearchValuesOfPrepositionsInTautology (Imply left rigth) =
  if leftInterpretation == truePrepositionValue
  then 
    " " ++ (addEmptyEspaces left)
    ++ "  T  "
    ++ (viewSearchValuesOfPrepositionsInTautology rigth) ++ " "
  else 
    if rigthInterpretation == falsePrepositionValue
    then 
      " " ++ (viewSearchValuesOfPrepositionsInContradiction left)
      ++ "  T  "
      ++ (addEmptyEspaces rigth) ++ " "
    else
      " " ++ (addEmptyEspaces left)
      ++ "  T  "
      ++ (addEmptyEspaces rigth) ++ " "
  where
    (leftInterpretation, rigthInterpretation) = 
      ( interpretation left
      , interpretation rigth
      )


viewSearchValuesOfPrepositionsInContradiction
  :: WFF Preposition -> String
viewSearchValuesOfPrepositionsInContradiction (Atomic preposition) = "F"


viewSearchValuesOfPrepositionsInContradiction (Not wff)
  = " F  " ++ (viewSearchValuesOfPrepositionsInTautology wff) ++ " "


viewSearchValuesOfPrepositionsInContradiction (And left rigth) =
  if leftInterpretation == truePrepositionValue
  then 
    " " ++ (addEmptyEspaces left)
    ++ "  F  "
    ++ (viewSearchValuesOfPrepositionsInContradiction rigth) ++ " "
  else 
    if rigthInterpretation == truePrepositionValue
    then 
      " " ++ (viewSearchValuesOfPrepositionsInContradiction left)
      ++ "  F  "
      ++ (addEmptyEspaces rigth) ++ " "
    else
      " " ++ (addEmptyEspaces left)
      ++ "  F  "
      ++ (addEmptyEspaces rigth) ++ " "
  where
    (leftInterpretation, rigthInterpretation) = 
      ( interpretation left
      , interpretation rigth
      )


viewSearchValuesOfPrepositionsInContradiction (Or left rigth) 
  = " " ++ (viewSearchValuesOfPrepositionsInContradiction left) 
  ++ "  F  "
  ++ (viewSearchValuesOfPrepositionsInContradiction rigth) ++ " "


viewSearchValuesOfPrepositionsInContradiction (Iff left rigth) =
  if leftInterpretation == truePrepositionValue
  then 
    " " ++ (addEmptyEspaces left)
    ++ "  F  "
    ++ (viewSearchValuesOfPrepositionsInContradiction rigth) ++ " "
  else 
    if leftInterpretation == falsePrepositionValue
    then 
      " " ++ (addEmptyEspaces left)
      ++ "  F  "
      ++ (viewSearchValuesOfPrepositionsInTautology rigth) ++ " "
    else
      if rigthInterpretation == truePrepositionValue
      then
        " " ++ (viewSearchValuesOfPrepositionsInContradiction left)
        ++ "  F  "
        ++ (addEmptyEspaces rigth) ++ " "
      else 
        if rigthInterpretation == falsePrepositionValue
        then 
          " " ++ (viewSearchValuesOfPrepositionsInTautology left)
          ++ "  F  "
          ++ (addEmptyEspaces rigth) ++ " "
        else
          " " ++ (addEmptyEspaces left)
          ++ "  F  "
          ++ (addEmptyEspaces rigth) ++ " "
  where
    (leftInterpretation, rigthInterpretation) = 
      ( interpretation left
      , interpretation rigth
      )


viewSearchValuesOfPrepositionsInContradiction (Imply left rigth)
  = " " ++ (viewSearchValuesOfPrepositionsInTautology left)
  ++ "  F  "
  ++ (viewSearchValuesOfPrepositionsInContradiction rigth) ++ " "


addEmptyEspaces :: WFF Preposition -> String
addEmptyEspaces (Atomic _ ) = " "
addEmptyEspaces (Not wff) = "    " 
  ++ (addEmptyEspaces wff) ++ " "
addEmptyEspaces (And left rigth)   = " " 
  ++ (addEmptyEspaces left) ++ "     " 
  ++ (addEmptyEspaces rigth) ++ " "
addEmptyEspaces (Or left rigth)    = " " 
  ++ (addEmptyEspaces left) ++ "     " 
  ++ (addEmptyEspaces rigth) ++ " "
addEmptyEspaces (Iff left rigth)   = " " 
  ++ (addEmptyEspaces left) ++ "     " 
  ++ (addEmptyEspaces rigth) ++ " "
addEmptyEspaces (Imply left rigth) = " " 
  ++ (addEmptyEspaces left) ++ "     " 
  ++ (addEmptyEspaces rigth) ++ " "
