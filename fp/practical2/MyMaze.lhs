> module MyMaze (
>   Maze, 
>   makeMaze, 
>   addWall, 
>   makeBoundary,
>   reflect,
>   hasWall,  
>   sizeOf,
>   fromEnumDir,
>   randomMaze    
> )
> where

> import Geography 
  
Exercise 5: 
Since searching for walls in a list takes a very long time and is inefficient, 
a better representation of a maze would be to store the walls in four separate 
lists, one for each direction. 

> data Maze = Maze Size [Place] [Place] [Place] [Place]

Here, the first list of places are those that have a wall to the North, the second 
list those that have a wall to the South, the third those that have a wall to the East, 
and the fourth those that have a wall to the West.  

> makeMaze :: Size -> [Wall] -> Maze
> makeMaze (x,y) walls = 
>   let boundariesN = [ (i,y-1) | i <- [0..x-1] ]
>       boundariesS = [ (i,0)   | i <- [0..x-1] ]
>       boundariesE = [ (x-1,j) | j <- [0..y-1] ]
>       boundariesW = [ (0,j)   | j <- [0..y-1] ]
>       (ns, ss, es, ws) = foldr addWall ([],[],[],[]) (walls ++ map reflect walls ++ map (makeBoundary N) boundariesN  ++ map (makeBoundary S) boundariesS ++ map (makeBoundary E) boundariesE ++ map (makeBoundary W) boundariesW)
>   in Maze (x,y) ns ss es ws

Here, makeMaze first computes the boundaries of the maze, then uses foldr to 
process each wall (including the reflected walls and the boundary walls) to
produce the four lists of walls. 

> addWall :: Wall -> ([Place], [Place], [Place], [Place]) -> ([Place], [Place], [Place], [Place])
> addWall (pos, d) (ns, ss, es, ws) = case d of
>   N -> (pos:ns, ss, es, ws)
>   S -> (ns, pos:ss, es, ws)
>   E -> (ns, ss, pos:es, ws)
>   W -> (ns, ss, es, pos:ws)

Here, addWall takes a wall and the current four lists of walls, and adds the wall
to the appropriate list. 

> makeBoundary :: Direction -> Place -> Wall
> makeBoundary d pos = (pos, d)

Here, makeBoundary takes a place and a direction and produces the corresponding wall.

> reflect :: Wall -> Wall
> reflect ((i,j), d) = (move d (i,j), opposite d)

Here, reflect "reflects" a wall; i.e. gives the representation as
seen from the other side; for example, reflect ((3,4), N) = ((3,5),S)

> hasWall :: Maze -> Place -> Direction -> Bool
> hasWall (Maze _ ns ss es ws) pos d = case d of
>   N -> pos `elem` ns
>   S -> pos `elem` ss
>   E -> pos `elem` es
>   W -> pos `elem` ws   

Here, hasWall tests whether there is a wall in the given direction from the given place.

> sizeOf :: Maze -> Size
> sizeOf (Maze size _ _ _ _) = size

Here, sizeOf returns the size of the maze.

The difference in the time and memory space needed to solve the large maze for this 
representation is much less than that neeeded for the previous representation.
Here is a comparison of the two functions to solve the maze:

fastSolveMaze largeMaze (0,0) (22,21)
[N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,W,W,S,W,N,N,N,W,W,W,N,N,N,E,S,S,E,N,N,N,W,W,N,N,N,N,N,E,S,S,S,S,E,E,E,E,E,E,E,S,W,W,W,W,W,S,E,E,E,E,E,E,N,N,N,W,W,W,W,N,E,E,N,W,W,N,E,E,N,W,W,W,N,N,N,E,S,S,E,N,N,E,E,E]
(0.03 secs, 4,907,992 bytes)

fastSolveMaze largeMaze (0,0) (22,21)
[N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,W,W,S,W,N,N,N,W,W,W,N,N,N,E,S,S,E,N,N,N,W,W,N,N,N,N,N,E,S,S,S,S,E,E,E,E,E,E,E,S,W,W,W,W,W,S,E,E,E,E,E,E,N,N,N,W,W,W,W,N,E,E,N,W,W,N,E,E,N,W,W,W,N,N,N,E,S,S,E,N,N,E,E,E]
(0.06 secs, 4,586,504 bytes)

Exercise 6:

> fromEnumDir :: Direction -> Int
> fromEnumDir N = 0
> fromEnumDir E = 1
> fromEnumDir S = 2
> fromEnumDir W = 3

> randomMaze :: Size -> [Wall]
> randomMaze (w,h) = 
>    [ ((x,y),d) | x <- [0..w-1], y <- [0..h-1], d <- [N,E]
>    , (x + y + fromEnumDir d) `mod` 3 == 0]

Here are a few test mazes generated using randomMaze:

ghci> drawMaze (makeMaze (5,10) (randomMaze (5,10)))
+---+---+---+---+---+
|           |       |
+   +---+   +   +---+
|   |           |   |
+   +   +---+   +   +
|       |           |
+---+   +   +---+   +
|           |       |
+   +---+   +   +---+
|   |           |   |
+   +   +---+   +   +
|       |           |
+---+   +   +---+   +
|           |       |
+   +---+   +   +---+
|   |           |   |
+   +   +---+   +   +
|       |           |
+---+   +   +---+   +
|           |       |
+---+---+---+---+---+

ghci> drawMaze (makeMaze (6,6) (randomMaze (6,6)))
+---+---+---+---+---+---+
|   |           |       |
+   +   +---+   +   +---+
|       |           |   |
+---+   +   +---+   +   +
|           |           |
+   +---+   +   +---+   +
|   |           |       |
+   +   +---+   +   +---+
|       |           |   |
+---+   +   +---+   +   +
|           |           |
+---+---+---+---+---+---+