module Combinators where

-- SK combinator expressions.
-- Application is written with :@ and is left-associative 
-- by the data structure. 
data SKExpr = S | K | SKExpr :@ SKExpr
  deriving (Eq, Show)

-- Example combinator expression for quick testing
combexpr = S :@ (K :@ S) :@ S :@ K

-- The Reducible class: values that can take one reduction 
-- step (or not).
class Reducible a where
  -- Reduce returns Nothing when the value is in normal form,
  -- or Just a' for the one-step leftmost reduction result.
  reduce                 :: a -> Maybe a

-- Exercise 1
-- Stategy: 
-- 1. Try reducing the left subexpression first.
-- 2. If the left side is in normal form, check if this whole
--    expression matches one of the SK reduction rules: 
--    (K x) r -> x
--    (S x y) r -> x r (y r) 
-- 3. If neither applies, try reducing the right subexpression.
instance Reducible SKExpr where
  reduce (l :@ r) = case reduce l of
    -- If left can reduce, do that
    Just l'       -> Just (l' :@ r)
    Nothing       -> case l of
      -- K reduction
      K :@ x       -> Just x
      -- S reduction 
      S :@ x :@ y  -> Just (x :@ r :@ (y :@ r))
      -- Otherwise, try reducing right
      _            -> case reduce r of
        Just r'     -> Just (l :@ r')
        Nothing     -> Nothing

  reduce _ = Nothing


-- Exercise 2
-- Repeatedly reduce until reaching normal form,
-- Returns full sequence of intermediate steps.
eval :: Reducible a => a -> [a]
eval x = x : case reduce x of
  Just x' -> eval x'
  Nothing -> []


