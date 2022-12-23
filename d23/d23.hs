import System.IO
import Data.Functor ((<&>))
import Santa
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (tails, find, intersect)
import Debug.Trace (traceShow)

data Elf = Elf Int Int deriving (Eq,Show)

instance Num Elf where
    Elf x y + Elf w e = Elf (x+w) (y+e)
    Elf x y * Elf w e = Elf (x*w) (y*e)
    Elf x y - Elf w e = Elf (x-w) (y-e)
    abs (Elf x y) = Elf (abs x) (abs y)
    signum (Elf x y) = Elf (signum x) (signum y)
    fromInteger x = Elf (fromInteger x) (fromInteger x)

compr :: [String] -> [Elf]
compr = concat . zipWith f [1..] . map rowToElf
    where f i = map (Elf i)
          rowToElf = mapMaybe isElf . zip [1..]
          isElf (i,x) = if x =='.' then Nothing else Just i

type Coord = Elf

inflate :: Int -> [Int]
inflate 0 = [-1,0,1]
inflate x = [x]

expand :: Coord -> [Coord]
expand (Elf x y) = [Elf i j | i <- inflate x, j <- inflate y, i /= 0 || j /= 0]

data Face = I | N | S | W | E deriving (Eq,Show)

vidot :: Face -> Coord
vidot I = Elf 0 0
vidot N = Elf (-1) 0
vidot S = Elf 1 0
vidot W = Elf 0 (-1)
vidot E = Elf 0 1

fov :: Face -> [Coord]
fov = expand . vidot

move :: Elf -> Face -> Elf
move elf face = elf + vidot face

cards = map (take 4) . tails . cycle $ [N,S,W,E]

spot :: [Elf] -> Elf -> Face -> Bool
spot elves elf = any ((`elem`elves) . (+elf)) . fov

next :: [Elf] -> [Face] -> Elf -> Elf
next elves faces myself =
    let near = hood elves myself
        lonely = not $ spot near myself I
        nface = fromMaybe I $ find (not.spot near myself) faces
    in move myself $ if lonely then I else nface

isUniq :: Eq a => [a] -> a -> Bool
isUniq xs x = (==1) . length . filter (==x) $ xs

hood :: [Elf] -> Elf -> [Elf]
hood elves elf = elves `intersect` ([elf+x|x<-fov I])

wantToGo :: [Elf] -> [Face] -> [Elf]
wantToGo elves faces = map (next elves faces) elves

click :: [Elf] -> [Face] -> [Elf]
click elves faces =
    let wanna = wantToGo elves faces
        ok = map (isUniq wanna) wanna
        ite x y z = if x then y else z
    in zipWith3 ite ok wanna elves

deck = take 10 cards

bigSquare :: [Elf] -> Int
bigSquare elves =
    let j = map (\(Elf x y)->x) elves
        i = map (\(Elf x y)->y) elves
        mx = 1 + maximum j - minimum j
        my = 1 + maximum i - minimum i
    in mx * my

solve elves = bigSquare elves - length elves

part1 lins =
    let begin = compr lins
    in solve $ foldl click begin deck

click2 :: ([Elf],Int) -> [[Face]] -> Int
click2 _ [] = 0
click2 (elf,i) (fac:fs) =
    let nx = traceShow i $ click elf fac
    in if elf==nx then i else click2 (nx,succ i) fs

solve2 = snd

part2 lins =
    let begin = compr lins
    in click2 (begin,1) cards

test1 = wrD part1
result1 = wrI part1

test2 = wrD part2
result2 = wrI part2

main = print =<< result2
