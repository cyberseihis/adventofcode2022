import System.IO
import Data.Functor ((<&>))
import Santa
import qualified Data.Array as Ar
import Data.Array (Array, listArray, indices, bounds, (!))
import Data.List (groupBy, findIndex, findIndices)
import Data.Char (isDigit)
import Control.Arrow (Arrow((***)))
import Data.Maybe (fromMaybe, fromJust)

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

data Coord = Coord Int Int deriving (Eq,Show)

fixHor :: Board -> Coord -> Coord
fixHor board (Coord x y) =
    let row = board!!x
        boun = bounds row
        nuy = around y boun
    in Coord x nuy

fixVert :: Board -> Coord -> Coord
fixVert board (Coord x y) =
    let row = board!!x
        boun = (peak board y,trench board y)
        nux = around x boun
    in Coord nux y

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
    let cand = move now
        pos
            | elem face [N,S] = fixVert board cand
            | otherwise = fixHor board cand
        clear = canGo board pos
        nn = if clear then n-1 else 0
        nupos = if clear then pos else coord
    in click board (Now nupos face) (nn,turn)

canGo :: Board -> Coord -> Bool
canGo board (Coord x y) =
    let row = board!!x
    in row!y

move :: Now -> Coord
move (Now (Coord x y) face) = 
    case face of
    N -> Coord (x-1) y
    S -> Coord (x+1) y
    W -> Coord x (y-1)
    E -> Coord x (y+1)

begining :: Board -> Now
begining board =
    let j = fixHor board $ Coord 0 maxBound
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

type Border = [Coord]
data Tyle = Tyle Border Border deriving (Eq,Show)

mkBorder :: Coord -> Coord -> Border
mkBorder (Coord x y) (Coord a b)
    | x==a && y>b = [Coord x i| i<-reverse[b..y]]
    | x==a && y<b = [Coord x i| i<-[b..y]]
    | x>a && y==b = [Coord i y| i<-reverse[x..a]]
    | x<a && y==b = [Coord i y| i<-[x..a]]

part1 = password . uncurry follow . compr

test1 = wrD part1
result1 = wrI part1
