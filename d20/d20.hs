import System.IO
import Data.Functor ((<&>))
import Santa
import GHC.Base (assert)
import Control.Arrow (Arrow(first))

compr :: [String] -> [(Int,Int)]
compr = (`zip` [0..]) . map read

move :: [(Int,Int)] -> Int -> [(Int,Int)]
move u i =
    let (bef,pr@(val,_):aft) = break ((==i).snd) u
        x = val `mod` l
        ful = aft++bef
        l = length ful
        point = if x>=0 then x else l-x
        (above,bellow) = splitAt point ful
    in concat [above,[pr],bellow]

kernell :: [(Int,Int)] -> [(Int,Int)]
kernell x =
    let iz = [0..length x - 1]
    in foldl move x iz

solve :: [(Int,Int)] -> Int
solve x =
    let u = cycle . map fst $ x
        gu = dropWhile (/=0) u
        y1 = gu!!1000
        y2 = gu!!2000
        y3 = gu!!3000
    in y1+y2+y3

part1 = solve . kernell . compr

key = 811589153

proc2 :: [(Int,Int)] -> [(Int,Int)]
proc2 = map $ first (*key)

ker2 = foldl1 (.) $ replicate 10 kernell

part2 = solve . ker2 . proc2 . compr

test1 = wrD part1
result1 = wrI part1

test2 = wrD part2
result2 = wrI part2

main = print =<< result2
