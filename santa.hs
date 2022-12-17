import System.IO
import Data.Functor ((<&>))

compr :: [String] -> b0
compr = error "X"

preproc :: b0 -> b1
preproc = error "X"

kernell :: b0 -> b1
kernell = error "X"

solve :: b0 -> c
solve = error "X"

part1 = solve . kernell . preproc . compr

test1 = wrap "demo" part1
result1 = wrap "input" part1

wrap :: String -> ([String]->a) -> IO a
wrap name f = (openFile name ReadMode >>= hGetContents) <&> (f . lines)
