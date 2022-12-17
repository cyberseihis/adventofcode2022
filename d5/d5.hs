module Main where
import System.IO
-- import Data.Map as M (fromList, lookup)
-- import Data.Maybe
import Data.Text (replace,pack,unpack)

fillWithCrates = unpack . replace (pack "    ") (pack " [-]") . pack
clearCrates = map (filter (/="[-]"))

rot8 :: [[a]] -> [[a]]
rot8 [] = []
rot8 x
    | null . head $ x = []
    | otherwise = map head x:rot8 (map tail x)

switch :: [a] -> Int -> a -> [a]
switch x i z =
    take i x ++ z : drop (i+1) x

moveTop :: [[a]] -> Int -> Int -> [[a]]
moveTop x i y =
    let frm = x !! i
        to = x !! y
        nfrm = tail frm
        nto = head frm:to
    in switch (switch x y nto) i nfrm

moveMore :: [[a]] -> Int -> Int -> Int -> [[a]]
moveMore x n i y =
    let frm = x !! i
        to = x !! y
        nfrm = drop n frm
        nto = take n frm++to
    in switch (switch x y nto) i nfrm

readStak :: [String] -> [[Char]]
readStak = map (map (!!1)) . clearCrates . rot8 . map (words.fillWithCrates)

readMoves :: [String] -> [(Int,Int)]
readMoves [] = []
readMoves (x:xs) =
    let ws = words x
        n = read (ws!!1)
        i = read (ws!!3) -1
        y = read (ws!!5) - 1 in
    replicate n (i,y) ++ readMoves xs

nuMoves :: [String] -> [(Int,Int,Int)]
nuMoves [] = []
nuMoves (x:xs) =
    let ws = words x
        n = read (ws!!1)
        i = read (ws!!3) -1
        y = read (ws!!5) - 1 in
    (n,i,y) : nuMoves xs

nextStak :: [[Char]] -> [(Int,Int)] -> [[Char]]
nextStak stak [] = stak
nextStak stak ((i,y):mvs) =
    nextStak (moveTop stak i y) mvs

nuStak :: [[Char]] -> [(Int,Int,Int)] -> [[Char]]
nuStak stak [] = stak
nuStak stak ((n,i,y):mvs) =
    nuStak (moveMore stak n i y) mvs

main :: IO ()
main = do
    fhandle <- openFile "input" ReadMode
    fcont <- hGetContents fhandle
    let (stak,_:moves) = break null . lines $ fcont
        stakk = readStak . init $ stak
        movvs = readMoves moves
        newmovs = nuMoves moves
    -- print stakk
    -- print $ moveTop stakk 0 2
    -- print movvs
    print . map head $ nextStak stakk movvs
    print . map head $ nuStak stakk newmovs
    return ()
