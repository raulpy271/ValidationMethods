
module DataWFF.PropositionalCalculus
  ( PropositionalCalculus(..)
  , FormulaInString(..)
  , Preposition(..)
  , WFF(..)
  , WFFProperties(..)
  , readWFF
  )
  where

import DataWFF.Instances


class PropositionalCalculus a where
  notInterpretation   :: a -> Preposition 
  andInterpretation   :: a -> a -> Preposition
  orInterpretation    :: a -> a -> Preposition
  iffInterpretation   :: a -> a -> Preposition
  implyInterpretation :: a -> a -> Preposition


instance PropositionalCalculus Preposition where


  notInterpretation wff = case wff of
    PrepositionWithUnknownValue char -> 
      PrepositionWithUnknownValue char
    PrepositionWithKnownValue char value -> 
      PrepositionWithKnownValue char (not value)
    

  andInterpretation 
    (PrepositionWithKnownValue _ True)
    (PrepositionWithKnownValue _ True)
      = PrepositionWithKnownValue '1' True 
  andInterpretation 
    (PrepositionWithKnownValue _ False) _
      = PrepositionWithKnownValue '0' False
  andInterpretation 
    _ (PrepositionWithKnownValue _ False)
      = PrepositionWithKnownValue '0' False
  andInterpretation _ _ = PrepositionWithUnknownValue 'x'


  orInterpretation 
    (PrepositionWithKnownValue _ True) _
      = PrepositionWithKnownValue '1' True
  orInterpretation 
    _ (PrepositionWithKnownValue _ True)
      = PrepositionWithKnownValue '1' True
  orInterpretation 
    (PrepositionWithKnownValue _ False)
    (PrepositionWithKnownValue _ False)
      = PrepositionWithKnownValue '0' False
  orInterpretation _ _ = PrepositionWithUnknownValue 'x'


  iffInterpretation 
    (PrepositionWithKnownValue _ valueLeft)
    (PrepositionWithKnownValue _ valueRigth)
      = PrepositionWithKnownValue 'x' (valueLeft == valueRigth)
  iffInterpretation _ _ = PrepositionWithUnknownValue 'x'


  implyInterpretation 
    _ (PrepositionWithKnownValue _ True)
      = PrepositionWithKnownValue '1' True
  implyInterpretation 
    (PrepositionWithKnownValue _ False) _
      = PrepositionWithKnownValue '1' True
  implyInterpretation 
    (PrepositionWithKnownValue _ valueLeft)
    (PrepositionWithKnownValue _ valueRigth)
      = PrepositionWithKnownValue 'x' (valueLeft <= valueRigth)
  implyInterpretation _ _ = PrepositionWithUnknownValue 'x'
 

