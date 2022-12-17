module Main where
import System.IO
import Data.List (break,sort, transpose)

isVisible :: (Ord a) => [[a]] -> Int -> Int -> Bool
isVisible x i j =
    let column = x !! j
        row = transpose x !! i
        val = column !! i
        (rowl,_:rowr) = splitAt j row
        (coll,_:colr) = splitAt i column
        f = (<val) . maximum
    in any f [rowl,rowr,coll,colr]

countVisible x =
    (+ perim) . length . filter id $
    [isVisible x i j | i <- [1..len-2], j <- [1..wid-2]]
    where len = length x
          wid = length . head $ x
          perim = 2*len + 2*wid - 4

scenes :: (Ord a) => [[a]] -> Int -> Int -> Int
scenes x i j =
    let column = x !! j
        row = transpose x !! i
        val = column !! i
        (rowl,_:rowr) = splitAt j row
        (coll,_:colr) = splitAt i column
        rowll = reverse rowl
        colll = reverse coll
        oneMore h = if null h then 0 else 1
        f = (\(x,y)->length x + oneMore y).span (<val)
    in product . map f $ [rowll,rowr,colll,colr]

bestScene x =
    maximum $
    [scenes x i j | i <- [1..len-2], j <- [1..wid-2]]
    where len = length x
          wid = length . head $ x

main :: IO ()
main = do
    fhandle <- openFile "input" ReadMode
    fcont <- hGetContents fhandle
    let inp = lines fcont
    print $ bestScene inp
    return ()
