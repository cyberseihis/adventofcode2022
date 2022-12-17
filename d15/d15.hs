import System.IO
import Data.Functor ((<&>))
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe, fromJust)
import Control.Arrow ((***), Arrow ((&&&), second))
import Control.Applicative (Applicative(liftA2))
import Control.Monad (liftM2)
import Data.Sequence
    ( Seq((:|>)), adjust', findIndexL, sortOn, fromList, empty, index )
import Data.List (find, foldl', nub)
import Data.Foldable (toList)

grabInts :: String -> [Int]
grabInts =
    let h = ['0'..'9']++[' ','-']
    in mapMaybe readMaybe . words . filter (`elem` h)

compr :: [String] -> [((Int,Int),(Int,Int))]
compr = map (h.take 4.grabInts)
    where h [x,y,z,w] = ((x,y),(z,w))

coverd :: Int -> (Int,Int) -> Int -> Maybe (Int,Int)
coverd j (x,y) r
    | dr<0 = Nothing
    | otherwise = Just (x-dr,x+dr)
    where dr = r - abs(j-y)

manhatan :: (Int,Int) -> (Int,Int) -> Int
manhatan = (uncurry (+).) . uncurry (***) . (h***h)
    where h = (abs.).(-)

mergeRanges :: Seq (Int,Int) -> (Int,Int) -> Seq (Int,Int)
mergeRanges xs u@(i,y) =
    let g = findIndexL ((>=i-1).snd) xs
        brog (x,c) (t,y) = (max 0 *** min 4000000) (min x t,max c y)
    in case g of
        Just x -> adjust' (brog u) x xs
        Nothing -> xs:|>u

fixRanges :: Seq (Int,Int) -> Seq (Int,Int)
fixRanges = fst . fromJust . find ((==0).snd) . uncurry zip . second (\x->zipWith (-) x $ tail x) . (id&&&map length) . iterate (foldl' mergeRanges empty) . sortOn fst

atHeight :: Int -> [((Int,Int),(Int,Int))] -> Seq (Int,Int)
atHeight y = fromList . mapMaybe (uncurry (coverd y) . (fst &&& uncurry manhatan))

beaconsAtY :: Int -> [((Int,Int),(Int,Int))] -> Int
beaconsAtY y = length . filter (==y) . map snd . nub . map snd

kernell :: Int -> [((Int,Int),(Int,Int))] -> Seq (Int,Int)
kernell y = fixRanges . atHeight y

nCoverage :: Seq (Int,Int) -> Int
nCoverage = sum . map ((+1).abs.uncurry (-)) . toList

solve :: Int -> [((Int,Int),(Int,Int))] -> Int
solve y = uncurry (-) . ((nCoverage . kernell y) &&& beaconsAtY y)

magicRow :: [((Int,Int),(Int,Int))] -> Maybe Int
magicRow x = find ((>1).length.(`kernell`x)) [0..4000000]

solve2 :: [((Int,Int),(Int,Int))] -> Int
solve2 x =
    let y = fromJust $ magicRow x
        i = (+1) . snd . (`index`0) . kernell y $ x
    in i*4000000+y

part1 =  solve 2000000 . compr
part2 = solve2 . compr

test1 = wrap "demo" part1
result1 = wrap "input" part1

test2 = wrap "demo" part2
result2 = wrap "input" part2

wrap :: String -> ([String]->a) -> IO a
wrap name f = (openFile name ReadMode >>= hGetContents) <&> (f . lines)

main = result2 >>= print
