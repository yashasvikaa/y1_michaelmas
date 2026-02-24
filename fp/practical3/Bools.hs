module Bools where

import Prelude hiding (True , False)
import Combinators

-- Exercise 3
true , false :: SKExpr
true = K -- selects the first argument 
-- false = K I where I = S K K
false = K :@ (S :@ K :@ K) -- selects the second argument

data BoolExpr = True | False | Or BoolExpr BoolExpr | And BoolExpr BoolExpr | Neg BoolExpr
  deriving (Eq, Show)

-- Example expression for quick testing 
boolexpr = True `And` (True `And` Neg (False `Or` True))

-- Exercise 4
-- Key Idea: Every Boolean expression is translated 
-- into an SK expression that represents a function 
-- expecting two arguments: valueIfTrue and valueIfFalse.
-- So, And a b = a b false means if a is true, return b, else return false.
-- Similarly, Or a b = a true b 
-- Neg b = b false true

compileBool :: BoolExpr -> SKExpr
compileBool True = true 
compileBool False = false

compileBool (Neg b) = compileBool b :@ false :@ true 
compileBool (And a b) = compileBool a :@ compileBool b :@ false 
compileBool (Or a b) = compileBool a :@ true :@ compileBool b

-- Evaluating a BoolExpr by compiling it to SK and fully reducing.
-- After evaluation, the result should be either 'true' or 'false'.
evalBool :: BoolExpr -> IO ()
evalBool bexpr = case last (eval (compileBool bexpr)) of
  b | b == true -> print (show bexpr ++ " is True")
  b | b == false -> print (show bexpr ++ " is False")
  _ -> print "Didn't reduce to a Bool"