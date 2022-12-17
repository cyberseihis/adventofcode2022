{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.IO
import Data.List (break,sort, elemIndex)
import qualified Data.Text as T
import Control.Arrow (arr, Arrow ((***)))

data V = Vi Int | Va [V] deriving (Show,Read)

instance Eq V where
    (==) (Vi x) (Vi y) = x == y
    (==) vx@(Va x) vy@(Vi y) = vx == Va [vy]
    (==) vx@(Vi x) vy@(Va y) = Va [vx] == vy
    (==) (Va y) (Va x) = y == x

instance Ord V where
    Vi x <= Vi y = x<=y
    vx@(Va _) <= vy@(Vi _) = vx <= Va [vy]
    vx@(Vi _) <= vy@(Va _) = Va [vx] <= vy
    Va [] <= Va (_:_) = True
    Va (_:_) <= Va [] = False
    Va (x:xs) <= Va (y:ys)
        | x==y = Va xs <= Va ys
        | otherwise = x<=y

rephrase :: T.Text -> T.Text
rephrase =
    T.append "Va ".
    T.replace "." ",".
    T.replace "(" "[".
    T.replace "[" "[Vi ".
    T.replace "," ",Vi ".
    T.replace "[]" "(]".
    rebase .
    T.replace ",[" ".Va ["

rebase :: T.Text -> T.Text
rebase x
    | x==nu = x
    | otherwise = rebase nu
    where nu = T.replace "[[" "(Va [" . T.replace "[(" "(Va (" $ x

compr :: String -> [(T.Text,T.Text)]
compr = pairUp . map T.pack . filter (not.null) . lines where
    pairUp [] = []
    pairUp (x:y:xs) = (x,y):pairUp xs

preproc :: [(T.Text,T.Text)] -> [(V,V)]
preproc = map (arr rV *** arr rV)

rV = read . T.unpack . rephrase

kernell :: [(V,V)] -> [Bool]
kernell = map $ uncurry (<=)

solve1 :: [Bool] -> Int
solve1 = sum . map fst . filter snd . zip [1..]

part1 = solve1 . kernell . preproc . compr

dp1 = rV "[[2]]"
dp2 = rV "[[6]]"

fixProc :: [(V,V)] -> [V]
fixProc = (dp1:) . (dp2:) . uncurry (++) . unzip

kernell2 :: [V] -> [V]
kernell2 = sort

solve2 :: [V] -> Int
solve2 x = 
    let Just i1 = elemIndex dp1 x 
        Just i2 = elemIndex dp2 x 
    in (i1+1) * (i2+1)

part2 = solve2 . kernell2 . fixProc . preproc . compr

test1 = openFile "demo" ReadMode >>= hGetContents >>=
    return . kernell . preproc . compr

result1 = openFile "input" ReadMode >>= hGetContents >>=
    return . part1

test2 = openFile "demo" ReadMode >>= hGetContents >>=
    return . part2

result2 = openFile "input" ReadMode >>= hGetContents >>=
    return . part2

main :: IO ()
main = do
    fhandle <- openFile "input" ReadMode
    fcont <- hGetContents fhandle
    -- print . part1 $ fcont
    return ()
