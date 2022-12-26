import System.IO
import Data.Functor ((<&>))
import Santa
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Tuple (swap)

-- From -2 to 2
type Qit = Int
-- LSB first
type Snafu = [Qit]

type Dig = Char
type Fuel = String

diglist = zip "=-012" [-2..2]
fromDig :: Map Dig Qit
fromDig = Map.fromList diglist
toDig = Map.fromList $ map swap diglist

compr :: [String] -> [Snafu]
compr = map $ reverse . map (fromDig!)

snafuToInt :: Snafu -> Int
snafuToInt [] = 0
snafuToInt (x:xs) = x + (5 * snafuToInt xs)

intToSnafu :: Int -> Snafu
intToSnafu 0 = []
intToSnafu n
    | x>2 = (x-5):intToSnafu (y+1)
    | otherwise = x:intToSnafu y
    where x = n `mod` 5
          y = n `div` 5

writeFuel :: Snafu -> Fuel
writeFuel = reverse . map (toDig!)

part1 = writeFuel . intToSnafu . sum . map snafuToInt . compr

test1 = wrD part1
result1 = wrI part1
