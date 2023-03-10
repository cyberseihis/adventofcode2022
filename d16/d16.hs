import System.IO
import Data.Functor ((<&>))
import Data.Graph ()
import Data.IntMap ()
import Data.Char (isUpper, isDigit)
import Control.Arrow ((&&&), second)
import Data.Map.Strict (Map, (!))
import Data.Tuple (swap)
import qualified Data.Map.Strict as Map
import Data.IntMap.Strict (IntMap,(!))
import qualified Data.IntMap.Strict as IMap
import Data.Foldable (find)
import Control.Monad (guard)
import Data.List (nub)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Debug.Trace (traceShowId, trace)

glance :: String -> [String]
glance = tail . words . filter vax
    where vax x = isUpper x || isDigit x || x==' '

aOnTop :: [[String]] -> [[String]]
aOnTop = uncurry (++) . swap . break ((=="AA").head)

nominal :: [String] -> Map String Int
nominal = Map.fromList . flip zip [0..]

deName :: Map String Int -> [String] -> [Int]
deName x = map (x Map.!)

propa :: [[String]] -> [(Int,[Int])]
propa x =
    let (nams,flo,tu) = unzip3 . map (\(x:y:xs)->(x,y,xs)) . aOnTop $ x
        nDic = nominal nams
        detu = map (deName nDic) tu
        fli = map read flo
    in zip fli detu

compr :: [String] -> [(Int,[Int])]
compr = propa . map glance

weightIn :: [Int] -> [(Int,Int)]
weightIn = zip (repeat 1)

type Idk = (Int,[(Int,Int)])

newtype Grop = Grop (IntMap Idk) deriving (Eq,Show)
unGrop (Grop x) = x

dignify :: [(Int,[Int])] -> Grop
dignify = Grop . IMap.fromList . zip [0..] . map (second weightIn)

isRoad :: (Int,[a]) -> Bool
isRoad (x,ys) = x==0 && length ys == 2

isDeadend :: Grop -> Int -> Idk -> Bool
isDeadend (Grop dic) ke r@(x,ys) = isRoad r &&
    let [(_,u),(_,i)] = ys
        mpi = dic IMap.!i
        mpu = dic IMap.!u
        nir = not.isRoad
        chans1 = nir mpu && nir mpi
        chans2 = nir mpu && ke>i
        chans3 = ke>u && nir mpi
    in chans1 || chans2 || chans3

noRoads :: Grop -> Grop
noRoads gx@(Grop x) =
    let isdnt = isDeadend gx
        (rods,parks) = IMap.partitionWithKey isdnt x
    in (if IMap.null rods then gx else noRoads $ IMap.foldlWithKey bridge gx rods)

mergeRoads :: Grop -> Grop
mergeRoads gx@(Grop x) =
    let xs = IMap.filter (any ((`IMap.member`x) . snd) . snd) x
        Grop paved = IMap.foldlWithKey bridge gx xs
    in Grop $ foldl (flip IMap.delete) paved (IMap.keys xs)

bridge :: Grop -> Int -> Idk -> Grop
bridge (Grop x) ke (d,a) =
    let [(n,i),(m,y)] = take 2 a
        nucnt = m+n
        adj1 = IMap.adjust (pave y nucnt) i
        adj2 = IMap.adjust (pave i nucnt) y
        rem = IMap.delete ke
    in Grop . clenup ke . rem . adj2 . adj1 $ x

clenup :: Int -> IntMap Idk -> IntMap Idk
clenup ke = IMap.map $ second $ filter $ (/=ke).snd

pave :: Int -> Int -> Idk -> Idk
pave nu cnt = second ((cnt,nu):)

preproc :: [(Int,[Int])] -> Grop
preproc = noRoads . dignify

-- Second preprocessing

hammer :: Eq a => (a -> a) -> a -> a
hammer f x
  | x' == x = x'
  | otherwise = hammer f x'
  where x' = f x

toVal :: Grop -> Int -> Int
toVal (Grop dc) = fst . (dc IMap.!)

data MN = MN Int Int deriving (Eq,Show,Ord)
mn x y = if x<y then MN x y else MN y x

type Ruler = Map MN Int

suffl (a,x) = [(mn a c,b)|(b,c)<-x]

bizzaro :: [(MN,Int)] -> [(MN,Int)]
bizzaro = Map.toList . Map.fromListWith min

moreEdge1 :: [(MN,Int)] -> [(MN,Int)]
moreEdge1 par = bizzaro $
    par ++[(mn x w,c+d)|(MN x z,c)<-par,(MN w y,d)<-par,z==y, x/=w]


moreEdge2 :: [(MN,Int)] -> [(MN,Int)]
moreEdge2 par = bizzaro $
    par ++[(mn x y,c+d)|(MN x z,c)<-par,(MN w y,d)<-par,z==w, x/=y]

moreEdge3 :: [(MN,Int)] -> [(MN,Int)]
moreEdge3 par = bizzaro $
    par ++[(mn z w,c+d)|(MN x z,c)<-par,(MN w y,d)<-par,x==y, z/=w]

moreEdge4 :: [(MN,Int)] -> [(MN,Int)]
moreEdge4 par = bizzaro $
    par ++[(mn z y,c+d)|(MN x z,c)<-par,(MN w y,d)<-par,x==w, z/=y]

everyEdge :: [(MN,Int)] -> [(MN,Int)]
everyEdge =  hammer moreEdge4. hammer moreEdge3. hammer moreEdge2. hammer moreEdge1

toValhalla :: Grop -> [Idk]
toValhalla gx@(Grop x) =
    let tog = toVal gx
        lit = IMap.elems x
        f = map.second$map.second$tog
    in f lit

toValves :: [Idk] -> IntSet
toValves = IntSet.fromList . map fst

toSpace :: [Idk] -> Ruler
toSpace =
    Map.map succ . Map.fromListWith min . everyEdge . concatMap suffl

-- Computations

data Steam = Steam Int Int IntSet deriving (Eq,Show)

ahoy :: Ruler -> Steam -> Int
ahoy rul (Steam sec loc places)
    | null branches = boon
    | otherwise = boon + nextBest
    where branches = map toGo . IntSet.toList $ canReach
          nextBest = maximum . map (ahoy rul) $ branches
          boon = sec*loc
          canReach = IntSet.filter ((<=sec).cost) places
          cost x = rul Data.Map.Strict.! mn loc x
          toGo :: Int -> Steam
          toGo x = Steam (sec-cost x) x (IntSet.delete x canReach)

kernell :: b0 -> b1
kernell = error "X"

solve :: [Idk] -> Int
solve = uncurry ahoy . second (Steam 30 0.IntSet.delete 0) . (toSpace &&& toValves)

part1 :: [String] -> Int
part1 = solve . toValhalla . preproc . compr

type At = Int
type Clk = Int
type Chart = IntSet
data Eleph = Eleph
    { clk :: Clk
    , at :: At
    , chart :: Chart
    } deriving (Show, Eq)

ahoy2 :: Ruler -> Chart -> Steam -> Int
ahoy2 rul remain (Steam sec loc places)
    | null branches = boon + paralBest
    | otherwise = boon + theBest
    where branches = map toGo . IntSet.toList $ canReach
          theBest = max nextBest paralBest
          nextBest = maximum . map (uncurry$ahoy2 rul) $ branches
          paralBest = ahoy rul (Steam 26 0 remain)
          boon = sec*loc
          canReach = IntSet.filter ((<=sec).cost) places
          cost x = rul Data.Map.Strict.! mn loc x
          toGo :: Int -> (Chart,Steam)
          toGo x = 
            let jump = Steam (sec-cost x) x (IntSet.delete x canReach)
                hop = IntSet.delete x remain
            in (hop , jump)

solve2 :: [Idk] -> Int
solve2 x =
    let spa = toSpace x
        vlv = toValves x
        stem = Steam 26 0 crt
        crt = IntSet.delete 0 vlv
    in ahoy2 spa crt stem

part2 :: [String] -> Int
part2 = solve2 . toValhalla . preproc . compr

test1 = wrD part1
result1 = wrIn part1

test2 = wrD part2
result2 = wrIn part2

main = print =<< result2

wrD = wrap "demo"
wrIn = wrap "input"
wrap :: String -> ([String]->a) -> IO a
wrap name f = (openFile name ReadMode >>= hGetContents) <&> (f . lines)
