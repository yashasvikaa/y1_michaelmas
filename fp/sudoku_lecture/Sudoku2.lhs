> module Sudoku2 
> where 
> import Sudoku hiding (solve)
  
> size :: Matrix Freedoms -> Integer 
> size = product . map (toInteger . length) . concat

> pruneRow :: Row Freedoms -> Row Freedoms
> pruneRow row = map (remove (ones row)) row 

> ones :: [[a]] -> [a]
> ones = map head . filter singleton 

OR 
ones row = [ d | [d] <- row ]

> remove :: Freedoms -> Freedoms -> Freedoms 
> remove xs [d]  = [d]
> remove xs ds = filter (`notElem` xs) ds

> prune :: Matrix Freedoms -> Matrix Freedoms
> prune = pruneBy boxs . pruneBy cols . pruneBy rows 
>         where pruneBy f = f . map pruneRow . f 

> singleton :: [a] -> Bool 
> singleton [_] = True 
> singleton _ = False 

> prunesolve :: Solver 
> prunesolve = filter legal . expand . prune . freedoms 

LEMMA 
filter nodups . cp = filter nodups. cp . pruneRow 

> bowlsolver :: Solver 
> bowlsolver = filter legal . expand . repeatedly prune . freedoms

> repeatedly :: Eq a => (a -> a) -> a -> a
> repeatedly f x | f x == x = x
>                | otherwise = repeatedly f (f x)

repeatedly f = fst . head . dropWhile (uncurry (/=)) . pairs . iterate f
               where pairs xs = xs `zip` tail xs

> 