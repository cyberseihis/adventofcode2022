{-# LANGUAGE RecordWildCards #-}
import System.IO
import Data.Functor ((<&>))
import Santa
import qualified Data.Sequence as Seq
import Data.Array.Unboxed (UArray, array, listArray, (!), elems)
import qualified Data.Array.Unboxed as UArray
import Data.Sequence (Seq, (><))
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as Imap
import Control.Arrow (Arrow((&&&)))
import Debug.Trace (traceShowId)
import Data.Foldable (Foldable(toList))
import Data.List (elemIndex)
import Data.Maybe (fromJust)

-- Idea: Array to index order to sequnce location,
-- array to match order with value, so lut
-- sequence of orders that moves with the problem

type Lut = UArray Int Int
type Gps = IntMap Int
type Ring = Seq Int
data Now = Now
    { gps :: Gps
    , ring :: Ring
    } deriving (Show, Eq)

-- Get values in selection order
compr :: [String] -> Lut
compr x =
    let n = length x in
    listArray (0,n-1) . map read $ x

-- Starting locations of each
start :: Int -> Gps
start n = Imap.fromList $ zip [0..n-1] [0..n-1]

ezSeq :: Int -> Ring
ezSeq n = Seq.fromList [0..n-1]

moment :: Int -> Now
moment i = Now {gps=start i, ring=ezSeq i}

click :: Lut -> Now -> Int -> Now
click lut Now {..} n =
    let x = fx $ lut UArray.! n
        fx x = if x<0 then (x`mod`l)-1 else x`mod`l
        i = gps Imap.! n
        l = succ . snd . UArray.bounds $ lut
        thumb = teleport ring i ((i+x) `mod` l)
        belt = Seq.take x . Seq.drop (i+1) $ ring><ring
        sattelite = foldl (triangulate l) gps belt
        antena = Imap.adjust ((`mod`l).(+x)) n sattelite
    in Now {gps=antena, ring=thumb}

teleport :: Ring -> Int -> Int -> Ring
teleport ring from to
    | from < tor = Seq.deleteAt from . Seq.insertAt tor x$ ring
    | from > tor = Seq.deleteAt (from+1) . Seq.insertAt tor x$ ring
    | otherwise = ring
    where x = ring `Seq.index` from
          tor = to + 1

triangulate :: Int -> Gps -> Int -> Gps
triangulate n gps i = Imap.adjust f i gps
    where
    f :: Int -> Int
    f = (`mod`n) . pred

prett :: Lut -> Now -> [Int]
prett lut Now {ring} = map (lut UArray.!) . toList $ ring

part1 s =
    let l = length s
        no = moment l
        lut = compr s
        clock = click lut
    in foldl clock no [0..l-1]

bet s =
    let l = length s
        no = moment l
        lut = compr s
        clock = click lut
    in solve lut $ foldl clock no [0..l-1]

solve :: Lut -> Now -> Int
solve lut Now {..} =
    let humble = fromJust . elemIndex 0 . elems $ lut
        found = ((gps Imap.! humble) + 1)  `mod` l
        l= Imap.size gps
        targs = map ((`mod`l).(+found)) [1000,2000,3000]
        launch = map (Seq.index ring) targs
        vals = map (lut UArray.!) launch
    in sum vals

glance = wrD compr

test1 = wrD bet
result1 = wrI bet

main = print =<< result1
