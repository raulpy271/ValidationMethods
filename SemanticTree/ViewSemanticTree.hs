module SemanticTree.ViewSemanticTree
  ( viewSemanticTreeProcess
  , truePrepositionValue
  , falsePrepositionValue
  )
  where


import DataWFF.PropositionalCalculus
import DataWFF.Interpretation (interpretation)
import Tools.DataWFFManipulation 
  ( getUnkownPrepositionInWFF 
  , replacePrepositionInFormula
  )


type Depth = Int
type WFFInTree = String


(truePrepositionValue, falsePrepositionValue) = 
  ( PrepositionWithKnownValue '1' True
  , PrepositionWithKnownValue '0' False
  )


viewSemanticTreeProcess :: WFF Preposition -> WFFInTree
viewSemanticTreeProcess wff = 
  viewSemanticTreeHelper wff 
  (getUnkownPrepositionInWFF wff) 1 (show wff) 

  where
    getTabs :: Depth -> String 
    getTabs depth = concat $ replicate depth tab
      where tab = "    "
    

    viewSemanticTreeHelper 
      :: WFF Preposition -> [Preposition] -> Depth -> WFFInTree 
      -> WFFInTree 
    viewSemanticTreeHelper wff [] depth tree = tree ++
      ", interpretation: " ++ (show wffInterpretation) ++ "\n" 
      where
        wffInterpretation = interpretation wff


    viewSemanticTreeHelper wff (firstPreposition:otherPreposition) depth tree
      | (wffInterpretation == truePrepositionValue) 
        || (wffInterpretation == falsePrepositionValue) = tree ++ 
          ", interpretation: " ++ (show wffInterpretation) ++ "\n" 
      | otherwise = 
          tree ++ "\n" ++
          ( viewSemanticTreeHelper wffWithOne otherPreposition  
            (depth + 1) (  (getTabs depth) ++ (show wffWithOne) )
          ) ++ 
          ( viewSemanticTreeHelper wffWithZero otherPreposition 
            (depth + 1) (  (getTabs depth) ++ (show wffWithZero) )
          )

      where
        wffInterpretation = interpretation wff
        (wffWithOne, wffWithZero) = 
          ( replacePrepositionInFormula wff
            firstPreposition truePrepositionValue 
          , replacePrepositionInFormula wff
            firstPreposition falsePrepositionValue 
          )


