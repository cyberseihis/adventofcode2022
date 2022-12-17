{-# LANGUAGE OverloadedLists #-}
import System.IO
import Data.List (break,sort, elemIndex)
import Control.Arrow (Arrow((***), first, (&&&)))
import Data.Sequence (Seq ((:|>)), adjust', empty, index, (!?))
import qualified Data.Sequence as Seq
import Data.IntSet (IntSet, singleton, insert, lookupGT)
import qualified Data.IntSet as IntSet
import Data.Functor ((<&>))

compr :: [String] -> [[(Int,Int)]]
compr = map poins where
    poins = map (read . (++")") . ('(':)) . filter (/="->") . words

lows = (minimum *** minimum) . unzip . concat
highs = (maximum *** maximum) . unzip . concat

regX :: [[(Int,Int)]] -> [[(Int,Int)]]
regX xy =
    let (mx,my) = lows xy
        f = first (\x->x-mx)
    in map (map f) xy

unroll :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
unroll (x,y) (x2,y2) =
    let sx = min x x2
        bx = max x x2
        sy = min y y2
        by = max y y2
    in [(w,z)|w<-[sx..bx],z<-[sy..by]]

unscroll :: [(Int,Int)] -> [(Int,Int)]
unscroll [] = []
unscroll [x] = [x]
unscroll (x:y:xs) = unroll x y++unscroll (y:xs)

putIn :: Seq IntSet -> (Int,Int) -> Seq IntSet
putIn ch (x,y) = adjust' (insert y) x ch

putUp :: [(Int,Int)] -> Seq IntSet
putUp x =
    let i = maximum $ map fst x
    in foldl putIn (Seq.replicate (i+1) IntSet.empty) x

procc :: [[(Int,Int)]] -> Seq IntSet
procc = putUp . concatMap unscroll . regX

addFloor :: [[(Int,Int)]] -> [[(Int,Int)]]
addFloor x =
    let (_,by) = highs x
        j = by + 2
        flor = [(500-j,j),(500+j,j)]
    in flor:x

fallOnce :: Seq IntSet -> (Int,Int) -> Seq IntSet
fallOnce ch (x,y) = case boty of
    Just z -> if z<y then Seq.empty else
        if z==y then checkLeft else fallOnce ch (x,z)
    Nothing -> Seq.empty
    where chx = ch`index`x
          boty = bottom chx (y-1)
          ble = do
            col <- ch!?(x-1)
            bottom col y
          bri = do
            col <- ch!?(x+1)
            bottom col y
          checkLeft = case ble of
            Just z -> if z<=y then checkRight else fallOnce ch (x-1,z)
            Nothing -> Seq.empty
          checkRight = case bri of
            Just z -> if z<=y then adjust' (insert y) x ch else fallOnce ch (x+1,z)
            Nothing -> Seq.empty

bottom :: IntSet -> Int -> Maybe Int
bottom col y = pred <$> lookupGT y col

part1 x =
    let plots = compr x
        (mx,my) = lows plots
        sours = 500 - mx
        flon = flip fallOnce (sours,0)
    in pred.length . takeWhile (not.null) . iterate flon . procc $ plots

getOrigin = (500-).fst.lows

refactor1 =
    pred.length . takeWhile (not.null) . uncurry iterate . first (flip fallOnce.(,0))  . (getOrigin &&& procc) . compr

part2 x =
    let plots = addFloor $ compr x
        (mx,my) = lows plots
        sours = 500 - mx
        flon = flip fallOnce (sours,0)
    in pred.length . takeWhile (not.null) . iterate flon . procc $ plots

test1 = wrap "demo" part1
result1 = wrap "input" refactor1
test2 = wrap "demo" part2
result2 = wrap "input" part2

wrap :: String -> ([String]->a) -> IO a
wrap name f = (openFile name ReadMode >>= hGetContents) <&> (f . lines)
