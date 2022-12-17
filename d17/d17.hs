import System.IO
import Data.Functor ((<&>))
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Arrow (Arrow(second, first, (***)))
import Data.Maybe (fromMaybe)
import Data.List (intersperse)

data Mov = Le | Ri | Do deriving (Eq,Show,Enum)

compr :: [String] -> [Mov]
compr = map (\x->if x=='<' then Le else Ri) . head

ddr :: [Mov] -> [Mov]
ddr = intersperse Do . cycle

newtype Shape = Sh (Set (Int,Int))
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
bounder (Sh x) = not . any oob $ x
    where oob (y,x) = x<0 || x>6 || y<0

overlap :: Shape -> Shape -> Bool
overlap (Sh x) (Sh y) = Set.disjoint x y

everest :: Shape -> Int
everest x =
    let j = Set.lookupMax . Set.map fst . unSh $ x
    in fromMaybe 0 j

transla :: Shape -> (Int,Int) -> Shape
transla (Sh x) (i,y) = Sh . Set.map ((+i)***(+y)) $ x

spawn :: Shape -> Int -> Shape
spawn earth pat =
    let u = everest earth + 3
        egg = priors!!pat
    in transla egg (u,2)

frame :: (Shape,Shape,Int) -> Mov -> (Shape,Shape,Int)
frame (earth,rock,nex) m
    | m==Do && ov = (mars,spawn mars (nex`mod`7),nex+1)
    | otherwise = (earth,pebble,nex)
    where
        nu = premover rock m
        ov = overlap earth nu
        pebble = if ov || bounder nu then rock else nu
        mars = Sh . uncurry Set.union . (unSh***unSh) $ (earth,pebble)

preproc :: b0 -> b1
preproc = error "X"

kernell :: b0 -> b1
kernell = error "X"

solve :: b0 -> c
solve = error "X"

part1 = solve . kernell . preproc . compr

test1 = wrap "demo" part1
result1 = wrap "input" part1

wrap :: String -> ([String]->a) -> IO a
wrap name f = (openFile name ReadMode >>= hGetContents) <&> (f . lines)
