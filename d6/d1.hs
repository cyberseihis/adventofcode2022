module Main where
import System.IO
import Data.List (break,sort, nub)

quartets :: Int -> [a] -> [[a]]
quartets _ [] = []
quartets n x@(_:xs) =
    take n x:quartets n xs

isUnique :: Eq a => [a] -> Bool
isUnique x = x == nub x

firstPack :: Eq a => [a] -> Int
firstPack =
    (+4) . length . takeWhile (not.isUnique) . quartets 4

firstMes :: Eq a => [a] -> Int
firstMes =
    (+14) . length . takeWhile (not.isUnique) . quartets 14

main :: IO ()
main = do
    fhandle <- openFile "input" ReadMode
    fcont <- hGetContents fhandle
    print . firstMes $ fcont
    return ()
