import System.IO
import Data.Functor ((<&>))
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Arrow (Arrow(second, first, (***)))
import Data.Maybe (fromMaybe, fromJust)
import Data.List (intersperse, find, nub, findIndex)

data Mov = Le | Ri | Do deriving (Eq,Show,Enum)

compr :: [String] -> [Mov]
compr = map (\x->if x=='<' then Le else Ri) . head

ddr :: [Mov] -> [Mov]
ddr = intersperse Do . cycle

newtype Shape = Sh (Set (Int,Int)) deriving (Show)
unSh (Sh x) = x

blueprints = [
    [(0,0),(0,1),(0,2),(0,3)],
    [(1,0),(0,1),(1,1),(2,1),(1,2)],
    [(0,0),(0,1),(0,2),(1,2),(2,2)],
    [(0,0), (1,0), (2,0), (3,0)],
    [(0,0),(1,0),(0,1),(1,1)]
    ]

priors = map (Sh. Set.fromList) blueprints

premover :: Shape -> Mov -> Shape
premover (Sh s) m = Sh $ Set.map (poi m) s

poi :: Mov -> (Int,Int) -> (Int,Int)
poi m = case m of
    Le -> second pred
    Ri -> second succ
    Do -> first pred

bounder :: Shape -> Bool
bounder (Sh x) = any oob x
    where oob (y,x) = x<0 || x>6 || y<0

overlap :: Shape -> Shape -> Bool
overlap (Sh x) (Sh y) = not $ Set.disjoint x y

everest :: Shape -> Int
everest x =
    let j = Set.lookupMax . Set.map fst . unSh $ x
        jj = succ <$> j
    in fromMaybe 0 jj

transla :: Shape -> (Int,Int) -> Shape
transla (Sh x) (i,y) = Sh . Set.map ((+i)***(+y)) $ x

spawn :: Shape -> Int -> Shape
spawn earth pat =
    let u = everest earth + 3
        egg = priors!!pat
    in transla egg (u,2)

frame :: (Shape,Shape,Int) -> Mov -> (Shape,Shape,Int)
frame (earth,rock,nex) m
    | m==Do && okk = (mars,spawn mars (nex`mod`5),nex+1)
    | otherwise = (earth,pebble,nex)
    where
        nu = premover rock m
        ov = overlap earth nu
        okk = ov || bounder nu
        pebble = if okk then rock else nu
        mars = Sh . uncurry Set.union . (unSh***unSh) $ (earth,pebble)

preproc = ddr

trench = Sh Set.empty

kernell :: [Mov] -> [(Shape,Shape,Int)]
kernell = scanl frame (trench,spawn trench 0,1)

solve :: Int -> [(Shape,Shape,Int)] -> Int
solve i = (\(Just (x,_,_))->everest x) . find (\(_,_,x)->x==i)

part1 = solve 164 . kernell . preproc . compr

heightAt :: Int -> [(Shape,Shape,Int)] -> Int
heightAt i = (\(x,_,_)->everest x) . (!!i)

thought :: [(Shape,Shape,Int)] -> [Int]
thought = map fst . nub . map (\(x,_,p)->(everest x,p))

delta :: Num a => [a] -> [a]
delta x = zipWith (-) (tail x) x

rountabout :: [(Shape,Shape,Int)] -> Int -> Int
rountabout x y = fromJust $ findIndex (\(w,_,_)->everest w==y) x

solve2 bg hk =
    let lg = length bg
        lk = length hk
        sg = sum bg
        sk = sum hk
        after = 1000000000000-lg
        bulk = (after`div`lk) * sk
        rst = sum . take (after`mod`lk) $ hk
    in sg+bulk+rst

part2 = kernell . preproc . compr

test1 = wrap "demo" part1
result1 = wrap "input" part1

test2 = wrap "demo" part2
result2 = wrap "input" part2

wrap :: String -> ([String]->a) -> IO a
wrap name f = (openFile name ReadMode >>= hGetContents) <&> (f . lines)
