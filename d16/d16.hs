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

newtype Grop = Grop (IntMap (Int,[(Int,Int)])) deriving (Eq,Show)
unGrop (Grop x) = x

dignify :: [(Int,[Int])] -> Grop
dignify = Grop . IMap.fromList . zip [0..] . map (second weightIn)

isRoad :: (Int,[a]) -> Bool
isRoad (x,ys) = x==0 && length ys == 2

isDeadend :: Grop -> Int -> (Int,[(Int,Int)]) -> Bool
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

bridge :: Grop -> Int -> (Int, [(Int,Int)]) -> Grop
bridge (Grop x) ke (d,a) =
    let [(n,i),(m,y)] = take 2 a
        nucnt = m+n
        adj1 = IMap.adjust (pave y nucnt) i
        adj2 = IMap.adjust (pave i nucnt) y
        rem = IMap.delete ke
    in Grop . clenup ke . rem . adj2 . adj1 $ x

clenup :: Int -> IntMap (Int, [(Int, Int)]) -> IntMap (Int, [(Int, Int)])
clenup ke = IMap.map $ second $ filter $ (/=ke).snd

pave :: Int -> Int -> (Int, [(Int, Int)]) -> (Int, [(Int, Int)])
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

newtype Ruler = Ruler (Map MN Int) deriving (Eq,Show)
unRuler (Ruler x) = x

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

toValhalla :: Grop -> [(Int,[(Int,Int)])]
toValhalla gx@(Grop x) =
    let tog = toVal gx
        lit = IMap.elems x
        f = map.second$map.second$tog
    in f lit

toValves :: [(Int,[(Int,Int)])] -> IntSet
toValves = IntSet.fromList . map fst

toSpace :: [(Int,[(Int,Int)])] -> Ruler
toSpace =
    Ruler . Map.map succ . Map.fromListWith min . everyEdge . concatMap suffl

-- Computations

kernell :: b0 -> b1
kernell = error "X"

solve :: b0 -> c
solve = error "X"

part1 = solve . kernell . preproc . compr

test1 = wrDemo $ toSpace . toValhalla . preproc . compr
result1 = wrIn $ toSpace . toValhalla .preproc . compr

wrDemo = wrap "demo"
wrIn = wrap "input"
wrap :: String -> ([String]->a) -> IO a
wrap name f = (openFile name ReadMode >>= hGetContents) <&> (f . lines)
