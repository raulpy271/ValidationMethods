module DataWFF.Interpretation
  ( interpretation
  
  )
  where


import DataWFF.PropositionalCalculus


interpretation :: WFF Preposition -> Preposition 
interpretation wff = case wff of
  Atomic preposition -> preposition

  Not preposition -> 
    notInterpretation 
      (interpretation preposition)

  And prepositionLeft prepositionRigth -> 
    andInterpretation 
      (interpretation prepositionLeft) 
      (interpretation prepositionRigth)

  Or prepositionLeft prepositionRigth -> 
    orInterpretation 
      (interpretation prepositionLeft) 
      (interpretation prepositionRigth)

  Iff prepositionLeft prepositionRigth -> 
    iffInterpretation 
      (interpretation prepositionLeft) 
      (interpretation prepositionRigth)

  Imply prepositionLeft prepositionRigth -> 
    implyInterpretation 
      (interpretation prepositionLeft) 
      (interpretation prepositionRigth)


