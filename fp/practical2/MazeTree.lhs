> module MazeTree (
>   Tree, 
>   fromListToTree,
>   fromSorted,
>   treeToList,
>   treeContains,
>   MazeT,
>   sizeOfT,
>   collect,
>   makeMazeT,
>   hasWallT,
>   mazeTToWalls
> ) 
> where

> import Geography
> import Data.List (sort)
  
> data Tree a = Empty 
>             | Node (Tree a) a (Tree a)
>   deriving (Show, Eq)

> fromListToTree :: [Place] -> Tree Place
> fromListToTree xs = fromSorted (sort xs)

> fromSorted :: [Place] -> Tree Place
> fromSorted [] = Empty
> fromSorted ys =
>  let mid = length ys `div` 2
>      (left, m:right) = splitAt mid ys
>  in Node (fromSorted left) m (fromSorted right)

> treeToList :: Tree a -> [a]
> treeToList Empty = []
> treeToList (Node left x right) = treeToList left ++ [x] ++ treeToList right 

> treeContains :: Ord a => Tree a -> a -> Bool
> treeContains Empty _ = False  
> treeContains (Node left x right) y
>   | y == x    = True
>   | y < x     = treeContains left y
>   | otherwise = treeContains right y

> data MazeT = MazeT Size (Tree Place) (Tree Place) (Tree Place) (Tree Place)
>   deriving (Show, Eq)

> sizeOfT :: MazeT -> Size
> sizeOfT (MazeT size _ _ _ _) = size  

> collect :: Direction -> [Wall] -> [Place]
> collect d walls = [ pos | (pos, dir) <- walls, dir == d]

> makeMazeT :: Size -> [Wall] -> MazeT
> makeMazeT size walls =
>   let ns = fromListToTree (collect N walls)
>       ss = fromListToTree (collect S walls)
>       es = fromListToTree (collect E walls)
>       ws = fromListToTree (collect W walls)
>   in MazeT size ns ss es ws

> hasWallT :: MazeT -> Place -> Direction -> Bool
> hasWallT (MazeT _ ns ss es ws) pos d = case d of
>   N -> treeContains ns pos
>   S -> treeContains ss pos
>   E -> treeContains es pos
>   W -> treeContains ws pos    

> mazeTToWalls :: MazeT -> [Wall]
> mazeTToWalls (MazeT _ ns ss es ws) =
>   [ (p, N) | p <- treeToList ns ] ++
>   [ (p, S) | p <- treeToList ss ] ++
>   [ (p, E) | p <- treeToList es ] ++
>   [ (p, W) | p <- treeToList ws ]
