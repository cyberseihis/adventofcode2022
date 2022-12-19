{-# LANGUAGE RecordWildCards #-}
import System.IO
import Data.Functor ((<&>))
import Santa
import Data.Char (isDigit)
import Control.Arrow (Arrow(second))
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShowId)
import Control.Parallel.Strategies (rdeepseq,parMap)
import Control.DeepSeq (force)

compr :: [String] -> [Cost]
compr = map $ bom . map read . filter (all isDigit) . words

bom :: [Int] -> Cost
bom [oo,co,bo,bc,go,gb] =
    [
    [oo,0,0,0],
    [co,0,0,0],
    [bo,bc,0,0],
    [go,0,gb,0]
    ]

mob :: Cost -> [Int]
mob [
    [oo,0,0,0],
    [co,0,0,0],
    [bo,bc,0,0],
    [go,0,gb,0]
    ] =
    [oo,co,bo,bc,go,gb]

type Raw = [Int]
type Slaves = [Int]
type Clk = Int
type Cost = [Raw]
data Now = Now
    { slaves :: Slaves
    , raw :: Raw
    , clk :: Clk
    } deriving (Show, Eq)

begin :: Now
begin = Now {slaves=[1,0,0,0], raw=[0,0,0,0], clk=0}


idMat =
    [ [1,0,0,0]
    , [0,1,0,0]
    , [0,0,1,0]
    , [0,0,0,1] ]

-- How many more materials are needed
charge :: Raw -> Raw -> Raw
charge = zipWith f where
    f x y = if x==0 then 0 else max 0 $ x-y

divUp :: Int -> Int -> Int
divUp x y
    | x == 0 && y==0 = 0
    | y == 0 = 1_000_000
    | otherwise = uncurry (+).second (min 1)$divMod x y

addsub :: Int -> Int -> Int -> Int
addsub x y z = x + y - z

-- State after buying the next robot, or nothing if you cant
buy :: Cost -> Now -> Int -> Maybe Now
buy cost Now {..} i =
    let hatch = zipWith (+) slaves $ idMat!!i
        bill = (cost !! i)
        togo = charge bill raw
        est = maximum $ zipWith divUp togo slaves
        tik = clk + est + 1
        prod = map (*(est+1)) slaves
        balan = zipWith3 addsub raw prod bill
    in (if tik >= depth then Nothing else Just $ Now {slaves=hatch, raw=balan, clk=tik})

-- Returns ammount of geodes at the end if nothing else gets bought
finalise :: Now -> Int
finalise Now {..} =
    let unused = depth - clk
        gRobots = slaves!!3
        prod = unused * gRobots
        g = raw!!3
    in g + prod

-- What the future will be like for every purchase
next :: Cost -> Now -> [Now]
next cost now =
    let candi = candidates (mob cost) now
    in mapMaybe (buy cost now) candi

oldidates :: [Int] -> Now -> [Int]
oldidates[oo,co,bo,bc,go,gb] Now {slaves=[ro,rc,rb,rg]} =
    let po = ro < 4 && rg ==0 && rb == 0
        pc = rc < bc && rg == 0
        pb = rc > 0 && rb < gb && (divUp gb rb > divUp go ro)
        pg = rb > 0
        uhu = [po,pc,pb,pg]
    in map fst . filter snd . zip [0..3] $ uhu
    

candidates :: [Int] -> Now -> [Int]
candidates[oo,co,bo,bc,go,gb] Now {slaves=[ro,rc,rb,rg]} =
    let po = ro < 4
        pc = rc < bc
        pb = rc > 0 && rb < gb
        pg = rb > 0
        uhu = [po,pc,pb,pg]
    in map fst . filter snd . zip [0..3] $ uhu
    

-- Best geodes from given state
kernell :: Cost -> Now -> Int
kernell cost now
    | null ahead = finalise now
    | clk now < 6 = maximum . parMap rdeepseq (kernell cost) $ ahead
    | otherwise = maximum . map (kernell cost) $ ahead
    where ahead = next cost now

depth = 32

part1 x = (`kernell` begin) $ force (head.compr$x)
part2 x = parMap rdeepseq  (`kernell` begin) $ force (take 3 .compr $ x)

test1 = wrD part1
result1 = wrI part1
test2 = wrD part2
result2 = wrI part2


try1 = [0,2,3,1,0,0,2,0,1,5,0,0,0,3,14,5,1,1,1,5,4,15,0,2,0,1,9,8,0,5]
blok = sum . zipWith (*) [1..] $ [9,12]
answer1 = sum . zipWith (*) [1..] $ try1

main = result2 >>= print
