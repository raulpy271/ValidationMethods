module ReductioAdAbsurdum.ViewLaws 
  ( laws
  ) 
  where


import DataWFF.PropositionalCalculus
import ReductioAdAbsurdum.ViewSearchPrepositionsValue 
  ( viewSearchValuesOfPrepositionsInContradiction 
  , viewSearchValuesOfPrepositionsInTautology 
  )


laws = "\n"
  ++ (text !! 0)
  ++ "\n"
  ++ (
       viewListOfExampleWhenOperationIsTrue
       listOfWFFWexamplesWhenOperationIsTrue
     ) 
  ++ "\n"
  ++ "\n" ++ (text !! 1)
  ++ "\n"
  ++ (
       viewListOfExampleWhenOperationIsFalse
       listOfWFFWexamplesWhenOperationIsFalse
     )
  ++ "\n"
  where
    text =
      [ "the following rules are used to value operands when operations are true:"
      , "the following rules are used to value operands when operations are false:"
      ]


listOfWFFWexamplesWhenOperationIsTrue,
  listOfWFFWexamplesWhenOperationIsFalse
  :: [WFF Preposition]
listOfWFFWexamplesWhenOperationIsTrue = 
  map readWFF
  [ "a ^ b"
  , "0 V b"
  , "a V 0"
  , "1 = b"
  , "0 = b"
  , "a = 1"
  , "a = 0"
  , "1 > b"
  , "a > 0"
  ]


listOfWFFWexamplesWhenOperationIsFalse = 
  map readWFF
  [ "1 ^ b"
  , "a ^ 1"
  , "a V b"
  , "1 = b"
  , "0 = b"
  , "a = 1"
  , "a = 0"
  , "a > b"
  ]




viewListOfExampleWhenOperationIsTrue,
    viewListOfExampleWhenOperationIsFalse
  :: [WFF Preposition]
  -> String
viewListOfExampleWhenOperationIsTrue list = 
  foldl ( foldHelper viewWFFAndYoursValuesFoundWhenOperationIsTrue) "" list


viewListOfExampleWhenOperationIsFalse list = 
  foldl ( foldHelper viewWFFAndYoursValuesFoundWhenOperationIsFalse) "" list


viewWFFAndYoursValuesFoundWhenOperationIsFalse, 
  viewWFFAndYoursValuesFoundWhenOperationIsTrue 
  :: WFF Preposition -> String
viewWFFAndYoursValuesFoundWhenOperationIsTrue wff =
  viewWFFAndYourValuesHelper viewSearchValuesOfPrepositionsInTautology wff 


viewWFFAndYoursValuesFoundWhenOperationIsFalse wff =
  viewWFFAndYourValuesHelper viewSearchValuesOfPrepositionsInContradiction wff 


viewWFFAndYourValuesHelper 
  :: (WFF Preposition -> String)
  -> WFF Preposition 
  -> String
viewWFFAndYourValuesHelper f wff = "\n" ++ (show wff) ++ "\n" ++ (f wff)


foldHelper
  :: (WFF Preposition -> String)
  -> String
  -> WFF Preposition 
  -> String
foldHelper f str wff = str ++ "\n" ++ (f wff)


