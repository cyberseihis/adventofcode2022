import System.IO
import Data.Functor ((<&>))
import Santa
import Data.Maybe (mapMaybe)

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
expand (Elf x y) = [Elf i j | x /= 0 || y /= 0, i <- inflate x, j <- inflate y]

part1 = error "a" . compr

test1 = wrD part1
result1 = wrI part1
