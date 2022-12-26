import Control.Arrow (Arrow ((&&&), (***), first))
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Functor ((<&>))
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (findIndices, transpose, findIndex)
import Data.Set (Set)
import qualified Data.Set as Set
import Santa
import System.IO
import Data.Maybe (fromJust)

-- Idea: keep set of empty positions for each direction in each column/row
-- compare if current height + #round % length is in set of that column etc
-- keep set? or list you nub with every position that can be reached every step
-- 2 arrays of sets, one for rows one for columns

type Free = IntSet -- Empty spaces for a line and direction

type Bydir = (Free, Free) -- Empty spaces for each direction of a line
-- First is up/left second down/right

type Rows = Array Int Bydir

type Cols = Array Int Bydir

type Grid = (Rows, Cols) -- All empty spaces

type Pos = (Int, Int) -- Position on map

type Spread = Set Pos -- All positions that could be reached this timestep

compr :: [String] -> Grid
compr inf =
  let x = skin inf
      y = transpose x
      h = length x
      w = length y
      rows = Array.listArray (0, h - 1) $ map readRow x
      cols = Array.listArray (0, w - 1) $ map readCol y
   in (rows, cols)

notMe x = IntSet.fromAscList . findIndices (/= x)

readRow :: Strip -> Bydir
readRow = notMe '<' &&& notMe '>'

readCol :: Slope -> Bydir
readCol = notMe '^' &&& notMe 'v'

type Strip = String -- Horizontal line

type Slope = String -- Vertical line

-- Remove walls
skin :: [String] -> [Strip]
skin = map (init . tail) . init . tail

type Clk = Int

-- Every point a position could possibly move to in a round
near :: Pos -> Spread
near (x, y) = Set.fromList . map ((+ x) *** (+ y)) $ [(1, 0), (-1, 0), (0, 0), (0, 1), (0, -1)]

-- Locations to consider for the next round
candidates :: Spread -> Spread
candidates = Set.unions . Set.map near

inBounds :: Grid -> Spread -> Spread
inBounds (rows, cols) =
  let h = length rows
      w = length cols
      inb (x, y) = x < h && y < w && x >= 0 && y >= 0
   in Set.filter inb

okCol :: Grid -> Clk -> Pos -> Bool
okCol (rows, cols) clk (x, y) =
  let ux = (x + clk) `mod` h
      dx = (x - clk) `mod` h
      h = length rows
      (colUp, colDown) = cols Array.! y
      okUp = IntSet.member ux colUp
      okDown = IntSet.member dx colDown
   in okUp && okDown

okRow :: Grid -> Clk -> Pos -> Bool
okRow (rows, cols) clk (x, y) =
  let ly = (y + clk) `mod` w
      ry = (y - clk) `mod` w
      w = length cols
      (rowLeft, rowRight) = rows Array.! x
      okLeft = IntSet.member ly rowLeft
      okRight = IntSet.member ry rowRight
   in okLeft && okRight

okGrid :: Grid -> Clk -> Spread -> Spread
okGrid grid clk = Set.filter okk . inBounds grid
    where okk x = okr x && okc x
          okr = okRow grid clk
          okc = okCol grid clk

-- Given previous positions and current time return current positions
kernell :: Grid -> Clk -> Spread -> Spread
kernell grid clk = okGrid grid clk . candidates

begining :: Spread
begining = Set.singleton (-1,0)

-- The bottom right corner
mazeExit :: Grid -> Pos
mazeExit = (pred***pred) . (length***length)

mazeGoal :: Grid -> Spread
mazeGoal = Set.singleton . first succ . mazeExit

solve :: Pos -> [Spread] -> Int
solve maex = (+2) . fromJust . findIndex (Set.member maex)

runner :: Grid -> Clk -> Spread -> [Spread]
runner grid clk x =
    let nx = kernell grid clk x
        rx = Set.union nx begining
    in rx:runner grid (clk+1) rx

runBack :: Grid -> Clk -> Spread -> [Spread]
runBack grid clk x =
    let nx = kernell grid clk x
        rx = Set.union nx $ mazeGoal grid
    in rx:runBack grid (clk+1) rx

solveBack :: [Spread] -> Int
solveBack = (+2) . fromJust . findIndex (Set.member (0,0))

track :: Grid -> Clk -> Int
track grid clk =
    let maex = mazeExit grid
    in solve maex $ runner grid clk begining

backtrack :: Grid -> Clk -> Int
backtrack grid clk =
    solveBack $ runBack grid clk (mazeGoal grid)

zigzag :: Grid -> Int
zigzag grid = 
    let
        un = track grid 1
        deux = backtrack grid . succ $ un
        trois = track grid . succ $ un + deux
    in un + deux + trois

part1 lins =
    let grid = compr lins
    in track grid 1

part2 = zigzag . compr

test1 = wrD part1
result1 = wrI part1

test2 = wrD part2
result2 = wrI part2
