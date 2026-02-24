module Sudoku3 where
import Sudoku (cp)

type Solver = Grid -> [Grid]
type Grid = Matrix Digit 

type Matrix a = [Row a]
type Row a = [Cell a]
type Cell a = a 

digits :: [ Digit ]
digits = ['1'..'9']

solve :: Solver 
solve = filter legal . expand . freedoms

legal :: Grid -> Bool

type Freedoms = [Digit]

freedoms :: Grid -> Matrix Freedoms
freedoms = map (map choice)
        where choice d | blank d = digits 
                       | otherwise = [d]

expand :: Matrix Freedoms -> [Grid]
expand = cp . map cp

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [ x:ys | x <- xs, ys <- cp xss]

legal :: Grid -> Bool 
legal g = all nodups (rows)