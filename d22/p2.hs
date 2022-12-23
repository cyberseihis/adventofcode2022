import System.IO
import Data.Functor ((<&>))
import Santa
import qualified Data.Array as Ar
import Data.Array (Array, listArray, indices, bounds, (!))
import Data.List (groupBy, findIndex, findIndices)
import Data.Char (isDigit)
import Control.Arrow (Arrow((***)))
import Data.Maybe (fromMaybe, fromJust)
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import Debug.Trace

type Row = Array Int Bool
type Board = [Row]

data Turn = L | R | I deriving(Eq,Show,Read)
type Leg = (Int,Turn)
type Path = [Leg]

compr :: [String] -> (Board,Path)
compr lins =
    let (bord,[_,paf]) = break null lins
    in (mkBoard bord,mkPath paf)

pairUp :: [String] -> [(String,String)]
pairUp [] = []
pairUp [x] = [(x,"I")]
pairUp (x:y:xs) = (x,y) : pairUp xs

mkPath :: String -> Path
mkPath x =
    let seperate = groupBy (\a b->isDigit a == isDigit b) x
        pars = pairUp seperate
        legs = map (read***read) pars
    in legs

mkBoard :: [String] -> Board
mkBoard = map mkRow

mkRow :: String -> Row
mkRow x =
    let (pad,ruins) = break (/=' ') x
        lth = length pad
        nth = length x
        bou@(bl,br) = (lth,nth-1)
        pavement = pave ruins
    in listArray bou pavement

pave :: [Char] -> [Bool]
pave = map (=='.')

-- Helpers

around :: Int -> (Int,Int) -> Int
around x (l,h)
    | x>h = l
    | x<l = h
    | otherwise = x

data Coord = Coord Int Int deriving (Eq,Show,Ord)

fixHor :: Now -> Now
fixHor now@(Now coord _) =
    let oh = verse!?coord
        ho = move <$> oh
    in fromMaybe now ho

fixVert :: Now -> Now
fixVert now@(Now coord _) =
    let oh = horse!?coord
        ho = move <$> oh
    in fromMaybe now ho

peak :: Board -> Int -> Int
peak board y = fromJust $ findIndex ((y`elem`).indices) board

trench :: Board -> Int -> Int
trench board y = last . findIndices ((y`elem`).indices) $ board

data Face = E | S | W | N deriving (Eq,Show,Enum)

fromTurn :: Turn -> Int
fromTurn I = 0
fromTurn R = 1
fromTurn L = -1

rotate :: Face -> Turn -> Face
rotate face turn =
    let x = fromEnum face
        y = fromTurn turn
    in toEnum $ (x+y) `mod` 4

data Now = Now Coord Face deriving (Eq,Show)

click :: Board -> Now -> Leg -> Now
click _ (Now coord face) (0,turn) = Now coord $ rotate face turn
click board now@(Now coord face) (n,turn) =
    let canned = move now
        pos
            | elem face [N,S] = fixVert canned
            | otherwise = fixHor canned
        Now poser _ = pos
        clear = canGo board poser
        nn = if clear then n-1 else 0
        nupos = traceShowId $ if clear then pos else now
    in click board nupos (nn,turn)

canGo :: Board -> Coord -> Bool
canGo board (Coord x y) =
    let row = board!!x
    in row!y

move :: Now -> Now
move (Now (Coord x y) face) =
    let 
    nuco =
        case face of
        N -> Coord (x-1) y
        S -> Coord (x+1) y
        W -> Coord x (y-1)
        E -> Coord x (y+1)
    in Now nuco face

oldfixHor :: Board -> Coord -> Coord
oldfixHor board (Coord x y) =
    let row = board!!x
        boun = bounds row
        nuy = around y boun
    in Coord x nuy

begining :: Board -> Now
begining board =
    let j = oldfixHor board $ Coord 0 maxBound
    in Now j E

follow :: Board -> Path -> Now
follow board path =
    let beg = begining board
        clock = click board
    in foldl clock beg path

password :: Now -> Int
password (Now (Coord x y) face) =
    let nx = x+1
        ny = y+1
        fa = fromEnum face
    in 1000*nx + 4*ny + fa

hardcode :: Now -> Now
hardcode (Now (Coord x y) face)
    | x<0 && y>=100 = Now (Coord (200+x) (y-100)) N

type Border = [Now]
type Tyle = Map Coord Now

mkBorder :: Coord -> Coord -> Face -> Border
mkBorder (Coord x y) (Coord a b) face =
    let
    nucoord
        | x==a && y>b = [Coord x i| i<-reverse [b..y]]
        | x==a && y<b = [Coord x i| i<-[y..b]]
        | x>a && y==b = [Coord i y| i<-reverse [a..x]]
        | x<a && y==b = [Coord i y| i<-[x..a]]
    in map (`Now` face) nucoord

mkTyle :: Border -> Border -> Tyle
mkTyle from to =
    let cfrom = map (\(Now x _)->x) from
    in Map.fromList $ zip cfrom to

domain :: [Tyle] -> Tyle
domain = foldl1 Map.union

-- Verticals
up3 = mkBorder (Coord (-1) 100) (Coord (-1) 149) S
do3 = mkBorder (Coord 50 100) (Coord 50 149) N
up1 = mkBorder (Coord (-1) 50) (Coord (-1) 99) S
do6 = mkBorder (Coord 150 50) (Coord 150 99) N
up5 = mkBorder (Coord 99 0) (Coord 99 49) S
do2 = mkBorder (Coord 200 0) (Coord 200 49) N

-- Horizontals
le1 = mkBorder (Coord 0 49) (Coord 49 49) E
ri3 = mkBorder (Coord 0 150) (Coord 49 150) W
le4 = mkBorder (Coord 50 49) (Coord 99 49) E
ri4 = mkBorder (Coord 50 100) (Coord 99 100) W
le5 = mkBorder (Coord 149 (-1)) (Coord 100 (-1)) E
ri6 = mkBorder (Coord 149 100) (Coord 100 100) W
le2 = mkBorder (Coord 150 (-1)) (Coord 199 (-1)) E
ri2 = mkBorder (Coord 150 50) (Coord 199 50) W

hors = [(up3,do2),(do3,ri4),(up1,le2),(do6,ri2),(up5,le4),(do2,up3)]
vers = [(le1,le5),(ri3,ri6),(le4,up5),(ri4,do3),(le5,le1),(ri6,ri3),(le2,up1),(ri2,do6)]

horse = domain $ map (uncurry mkTyle) hors
verse = domain $ map (uncurry mkTyle) vers

part1 = password . uncurry follow . compr

test1 = wrD part1
result1 = wrI part1

main = print =<< result1
