module SemanticTree.SemanticTree
  ( isTautology
  )
  where


import DataWFF.PropositionalCalculus
import DataWFF.Interpretation (interpretation)
import Tools.DataWFFManipulation 
  ( getUnkownPrepositionInWFF 
  , replacePrepositionInFormula
  )
import SemanticTree.ViewSemanticTree 
  ( viewSemanticTreeProcess
  , truePrepositionValue
  , falsePrepositionValue
  )


isTautology :: WFF Preposition -> WFFProperties
isTautology wff 
  | not (elem truePrepositionValue allInterpretation) = Contradiction
  | not (elem falsePrepositionValue allInterpretation) = Tautology
  | otherwise = Satisfiable 
  where allInterpretation = allInterpretationOfWFF wff


allInterpretationOfWFF :: WFF Preposition -> [Preposition]
allInterpretationOfWFF wff = 
  allInterpretationHelper wff (getUnkownPrepositionInWFF wff) 

  where
    allInterpretationHelper 
      :: WFF Preposition -> [Preposition] -> [Preposition] 
    allInterpretationHelper wff [] = [interpretation wff]


    allInterpretationHelper wff (firstPreposition:otherPreposition)
      | wffInterpretation == truePrepositionValue = [truePrepositionValue]
      | wffInterpretation == falsePrepositionValue = [falsePrepositionValue]
      | otherwise =
          (allInterpretationHelper wffWithOne otherPreposition) ++ 
          (allInterpretationHelper wffWithZero otherPreposition)

      where
        wffInterpretation = interpretation wff
        (wffWithOne, wffWithZero) = 
          ( replacePrepositionInFormula wff
            firstPreposition truePrepositionValue 
          , replacePrepositionInFormula wff
            firstPreposition falsePrepositionValue 
          )


