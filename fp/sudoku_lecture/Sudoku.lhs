> module Sudoku 
> where 

> type Solver = Grid -> [Grid]

Here, we choose to represent a matrix as a list of rows, and a row as a list of 
cells.

> type Grid = Matrix Digit 
> type Digit = Char 

> type Matrix a = [ Row a ]
> type Row a = [ Cell a ]
> type Cell a = a 

> digits :: [ Digit ]
> digits = ['1'..'9']

> blank :: Digit -> Bool
> blank = (== '.')

> solve :: Solver
> solve = filter legal . expand . freedoms

> freedoms :: Grid -> Matrix Freedoms 
> expand :: Matrix Freedoms -> [ Grid ]
> legal :: Grid -> Bool

> type Freedoms = [ Digit ] 
> freedoms = map ( map choice )
>            where choice d | blank d = digits
>                           | otherwise = [d]

> expand = cp . map cp 

> cp :: [[a]] -> [[a]]
> cp [] = [[]]
> cp (xs : xss) = [ x:ys | x <- xs, ys <- cp xss]

cp [ [1,2], [3], [4,5,6] ] gives [[1,3,4], [1,3,5], [1,3,6], [2,3,4], [2,3,5], [2,3,6]]
cp [ "12", "3", "456" ] gives ["134", "135", "136", "234", "235", "236"]

cp [xs= [ [x] | x<- xs]]
cp (xs:[]) = [x:ys | x<-xs, ys <- cp []]

m1 = [["1", "23"], ["45", "6"]]
map cp m1 gives [["12","13"],["46","56"]]
(cp. map cp) m1 gives [["12","46"],["12","56"],["13","46"],["13","56"]]

> legal g = all nodups (rows g) &&
>           all nodups (cols g) &&
>           all nodups (boxs g)

> nodups :: Eq a => [a] -> Bool
> nodups [] = True
> nodups (x:xs) = x `notElem` xs && nodups xs 

> rows :: Matrix a  -> [Row a]
> rows = id

> cols :: Matrix a -> [ Row a ]
> cols [xs] = [ [x] | x <- xs]
> cols (xs : xss) = zipWith (:) xs (cols xss)

> by :: Int -> [a] -> [[a]]
> by n [] = []
> by n xs = take n xs : by n (drop n xs)

> boxs :: Matrix a -> [Row a]
> boxs = map concat . concat . map cols . by 3 . map (by 3)

