import System.IO
import Data.Functor ((<&>))
import Data.List (intersect, (\\))
import Control.Arrow (Arrow(second, (&&&)))
import Data.Graph (flattenSCC, stronglyConnComp)

type C3 = (Int,Int,Int)

compr :: [String] -> [C3]
compr = map $ read . ('(':) . (++")")

-- Exposed sides = 6*|cubes| - 2*|edges|
-- or - sum(degrees)

potential :: C3 -> [C3]
potential (x,y,z) =
    [
    (x+1,y,z),(x,y+1,z),(x,y,z+1),
    (x-1,y,z),(x,y-1,z),(x,y,z-1)
    ]

everyone :: [C3] -> [C3]
everyone = concatMap potential

ejgy :: [C3] -> [C3]
ejgy x = everyone x `intersect` x

exposed :: [C3] -> Int
exposed x = (6 * ncub) - nedg
    where ncub = length x
          nedg = length . ejgy $ x

libr :: [C3] -> [C3]
libr x = filter (not.(`elem` x)) . everyone $ x

-- Cubes who have a cube above and a cube bellow
sandwich :: [C3] -> [C3]
sandwich u = filter (traped u) u

sr1 :: [C3] -> C3 -> [C3]
sr1 u (x,y,z) = filter (\(w,e,r)->y==e&&z==r) u

sr2 :: [C3] -> C3 -> [C3]
sr2 u (x,y,z) = filter (\(w,e,r)->x==w&&z==r) u

sr3 :: [C3] -> C3 -> [C3]
sr3 u (x,y,z) = filter (\(w,e,r)->y==e&&x==w) u

traped :: [C3] -> C3 -> Bool
traped u c@(x,y,z) =
    let g1 = sr1 u c
        g2 = sr2 u c
        g3 = sr3 u c
        o1 = any (\(n,_,_)->n>x) g1 && any (\(n,_,_)->n<x) g1
        o2 = any (\(_,n,_)->n>y) g2 && any (\(_,n,_)->n<y) g2
        o3 = any (\(_,_,n)->n>z) g3 && any (\(_,_,n)->n<z) g3
    in o1 && o2 && o3

refine :: [C3] -> [C3]
refine u =
    let d = sandwich u
        weird x = elem x u && notElem x d
        ok = not.any weird.potential
    in filter ok d

solve2 :: [C3] -> Int
solve2 x = length x - (length . refine $ x)

-- Common neighbors of two cubes
commons :: C3 -> C3 -> [C3]
commons x y = potential x `intersect` potential y

-- Are two cubes connected?
inone :: [C3] -> C3 -> C3 -> Bool
inone stones q@(x,y,z) v@(a,b,c) =
    let j = map abs [x-a,y-b,z-c]
        sj = sum j
        l1max = ((<=1).maximum$j)&&sj<3&&sj>0
        wifi = not.all (`elem`stones)$commons q v
    in l1max && (sj==1||wifi)

-- All connections of a cube
overcube :: [C3] -> [(Int,C3)] -> C3 -> [(Int,C3)]
overcube stones u c = filter (inone stones c.snd) u

-- Turns exposed faces to graph connections
toNodes :: [C3] -> [C3]  -> [(C3,Int,[Int])]
toNodes stones u =
    let mc = zip [0..] u
    in [(c,i,map fst$overcube stones mc c)|(i,c)<-mc]

toIdk :: [C3] -> [C3] -> [Int]
toIdk stones = map (length.flattenSCC). stronglyConnComp . toNodes stones

part1 = length . libr . compr
part2 = uncurry toIdk . (id&&&libr) . compr

test1 = wrD part1
result1 = wrI part1

wrD = wrap "demo"
wrI = wrap "input"

test2 = wrD part2
result2 = wrI part2

wrap :: String -> ([String]->a) -> IO a
wrap name f = (openFile name ReadMode >>= hGetContents) <&> (f . lines)

main = result2 >>= print
