module DataWFF.Datatypes
  ( FormulaInString(..)
  , Preposition(..)
  , WFF(..)
  , WFFProperties(..)
  )
  where


type FormulaInString = String
type LetterThatRepresentsAPreposition = Char
type PrepositionValue = Bool

data Preposition
  = PrepositionWithUnknownValue 
    LetterThatRepresentsAPreposition 
  | PrepositionWithKnownValue 
    LetterThatRepresentsAPreposition PrepositionValue


data WFF preposition
  = Atomic preposition
  | Not    (WFF preposition)
  | And    (WFF preposition) (WFF preposition)
  | Or     (WFF preposition) (WFF preposition)
  | Iff    (WFF preposition) (WFF preposition)
  | Imply  (WFF preposition) (WFF preposition)
  deriving (Eq)


data WFFProperties 
  = Tautology 
  | Satisfiable 
  | Contradiction 
  | Unknown
  deriving (Eq, Show)

