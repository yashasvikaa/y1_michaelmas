> import Sudoku 

> example = ["2....1.38",
>            "........5",
>            ".7...6...",
>            ".......13",
>            ".981..257",
>            "31....8..",
>            "9..8...2.",
>            ".5..69784",
>            "4..25...."]

> sudoku :: Solver -> FilePath -> IO()
> sudoku solver file = do 
>                        xs <- readFile file 
>                        putStr (xs ++ "\n" ++ interpret (solve(lines xs)))
>    where interpret [ ] = "No solution"
>          interpret [b] = unlines b 
>          interpret _ = "More than one solution"