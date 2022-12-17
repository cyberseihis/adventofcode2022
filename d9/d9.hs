module Main where
import System.IO
import Data.List (break,sort, nub)
import Data.Set (fromList)

cardinal :: Char -> (Int,Int)
cardinal x =
    case x of
    'U' -> (0,1)
    'D' -> (0,-1)
    'L' -> (-1,0)
    'R' -> (1,0)
    _ -> (0,0)

preproc :: String -> [(Int,Int)]
preproc x =
    let ((fac:_):ammo:_) = words x
        car = cardinal fac
        am = read ammo
    in replicate am car

comprehend :: String -> [(Int,Int)]
comprehend = concatMap preproc . lines

hPath :: [(Int,Int)] -> [(Int,Int)]
hPath = uncurry zip . f (scanl1 (+)) . unzip
    where
    f h (a,b) = (h a,h b)

move :: (Int,Int) -> (Int,Int) -> (Int,Int)
move (x,y) (j,k)
    | dx < -1 && dy < -1 = (j-1,k-1)
    | dx < -1 && dy > 1 = (j-1,k+1)
    | dx > 1 && dy < -1 = (j+1,k-1)
    | dx > 1 && dy > 1 = (j+1,k+1)
    | dx < -1 = (j-1,k)
    | dx > 1 = (j+1,k)
    | dy < -1 = (j,k-1)
    | dy > 1 = (j,k+1)
    | otherwise = (x,y)
    where
        dx = x - j
        dy = y - k

tPath :: [(Int,Int)] -> [(Int,Int)]
tPath = scanl move (0,0)

manySquares :: Ord a => [a] -> Int
manySquares = length . nub

part1 = manySquares . tPath . hPath . comprehend 

part2 = manySquares . foldl1 (.) (replicate 9 tPath) . hPath . comprehend 

main :: IO ()
main = do
    fhandle <- openFile "input" ReadMode
    fcont <- hGetContents fhandle
    print . part2 $ fcont
    return ()
