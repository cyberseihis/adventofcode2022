module Main where
import System.IO
import Data.List (break,sort)

splitUpon :: (Eq a) => a -> [a] -> [[a]]
splitUpon _ [] = []
splitUpon x (_:ys) =
    let f = break (==x)
        (a,b) = f ys
    in a:splitUpon x b

preproc :: String -> [[Int]]
preproc = map (map read) . splitUpon "" . lines

kernell :: [[Int]] -> [Int]
kernell = map sum

bestElf :: [Int] -> Int
bestElf = maximum

threeBestElves :: [Int] -> Int
threeBestElves = sum.take 3.reverse.sort

part1 :: String -> Int
part1 = bestElf . kernell .preproc

part2 :: String -> Int
part2 = threeBestElves . kernell .preproc

main :: IO ()
main = do
    fhandle <- openFile "input" ReadMode
    fcont <- hGetContents fhandle
    print . part1 $ fcont
    print . part2 $ fcont
    return ()
