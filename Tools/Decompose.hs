module Tools.Decompose 
  ( decomposeFormulaInString
  ) 
  where


import Text.Regex.Posix
import Tools.StringsManipulation
  ( sublist
  , getIndexOfFirstOperation
  , getIndexOfLastOperation
  , getIndexOfOperands
  )



decomposeFormulaInString :: String -> [String]

--e. g.:"(p > 1) = (a = b)" -> ["p > 1", " = ", "a = b"  ]

decomposeFormulaInString string = result (getIndexOfOperands string) string
  where
    indexFirstOperation = getIndexOfFirstOperation string
    indexLastOperation = getIndexOfLastOperation string
    result list string
      -- caso a formula seja desta forma : (p > q) = (p > q)
      | length list == 4 = 
          [ sublist ((list!!0)+1) (list!!1) string
          , sublist ((list!!1)+1) (list!!2) string
          , sublist ((list!!2)+1) (list!!3) string
          ] 


      | length list == 2 =
        -- caso a formula seja assim: ~ ( p > q)
        if (string =~ "\\w|\\(" :: String) == "(" 
          && ((reverse string) =~ "\\w|\\)" :: String) == ")"
          then [ sublist ((list!!0)+1) (list!!1) string, " ", " " ]
          else
            if (string =~ "\\w|\\(" :: String) == "("
            -- caso a formula seja assim : (p = q) > s
            then 
              [ sublist ((list!!0)+1) (list!!1) string
              , sublist indexLastOperation (indexLastOperation+1)  string
              , sublist (indexLastOperation+1) (length string) string 
              ]
          -- caso a formula seja assim: p > (q = s)
            else 
              [ sublist 0 indexFirstOperation string
              , sublist indexFirstOperation (indexFirstOperation+2) string
              , sublist ((list!!0)+1) (list!!1) string 
              ]


      -- caso a formula seja assim: p > q
      | (null list) && indexFirstOperation /= (-1) =
        [ sublist 0 indexFirstOperation string
        , [string !! indexFirstOperation ]
        , sublist (indexFirstOperation + 1) (length string) string 
        ]


      | otherwise = [string, " ", " "]  

