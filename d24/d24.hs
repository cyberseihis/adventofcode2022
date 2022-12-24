import Control.Arrow (Arrow ((&&&)))
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Functor ((<&>))
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (findIndices, transpose)
import Data.Set (Set)
import Santa
import System.IO

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

near :: Pos -> Spread
near (x,y) = Set.fromList . map ((+x)***(+y)) $ [(1,0),(-1,0),(0,0),(0,1),(0,-1)

-- Given previous positions and current time return current positions
kernell :: Grid -> Spread -> Clk -> Spread
kernell (rows,cols) spread clk = error " "
    

part1 = compr

test1 = wrD part1

result1 = wrI part1
