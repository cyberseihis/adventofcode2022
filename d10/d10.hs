module Main where
import System.IO
import Data.List (break,sort)

compr :: String -> Int
compr x
    | null y = 0
    | otherwise = read . head $ y
    where y = tail . words $ x

part1 :: String -> Int
part1 x =
    let y = signalstr .fixcycles. map compr . lines $ x
        ind = [20,60..220]
    in sum [y!!(i-1) |i<-ind]

fixcycles :: [Int] -> [Int]
fixcycles = concatMap (\x-> if x==0 then [0] else [0,x])

signalstr :: [Int] -> [Int]
signalstr x = zipWith (*) y [1..(length x + 1)]
    where y = scanl (+) 1 x

xReg :: [Int] -> [Int]
xReg = scanl (+) 1

inRang :: Int -> Int -> Bool
inRang x y = (<2) . abs $ x-y

takeRows :: [Int] -> [[Int]]
takeRows [] = []
takeRows x = take 40 x:takeRows (drop 40 x)

drawRow :: [Int] -> [Char]
drawRow = map drawTile . zipWith inRang [0..39]

drawTile :: Bool -> Char
drawTile False = '.'
drawTile True = '#'

part2 :: String -> [String]
part2 = map drawRow . takeRows . xReg .fixcycles. map compr . lines

main :: IO ()
main = do
    fhandle <- openFile "input" ReadMode
    fcont <- hGetContents fhandle
    print . part1 $ fcont
    mapM_ putStrLn $ part2 fcont
