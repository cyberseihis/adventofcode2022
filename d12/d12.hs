module Main where
import System.IO
import Data.List (break,sort, find, nub)
import Data.Graph (graphFromEdges, Graph, Vertex, path)
import Data.Char (ord)
import Data.Array ((!))
-- import Data.IntSet (IntSet, insert)
import Data.Sequence (Seq ((:<|)), (><), fromList)
import Data.Set (Set, insert)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Maybe (fromJust)

compr :: String -> [[Int]]
compr = map (map ord) . lines

reaches :: [[Int]] -> (Int , Int) -> [(Int,Int)]
reaches tav w =
    let f (x,y) = tav !! x !! y
        g (w,e) (r,t) = (w+r,e+t)
        heigh = length tav
        widt = length $ head tav
        val y
            | y == ord 'S' = ord 'a'
            | y == ord 'E' = ord 'z'
            | otherwise = y
        h = [(0,1),(0,-1),(1,0),(-1,0)]
        ok (x,y) = x>=0 && y>=0 && x<heigh && y < widt
    in filter ((<=val (f w)+1).val.f) . filter ok . map (g w) $ h

chartToGraph :: [[Int]] -> (Vertex->[Vertex],Int,Int)
chartToGraph tav =
    let heigh = length tav
        widt = length $ head tav
        f (x,y) = tav !! x !! y
        Just srs = find ((==ord 'S').f) coords
        Just dst = find ((==ord 'E').f) coords
        coords = [(i,y)|i<-[0..heigh-1],y<-[0..widt-1]]
        (g,nfv,vfk)=graphFromEdges [(x,x,reaches tav x)|x<-coords]
        thrd (_,_,n) = n
        (Just vsrs,Just vdst) = (vfk srs, vfk dst)
    in ((g!), vsrs, vdst)

reaches2 :: [[Int]] -> (Int , Int) -> [(Int,Int)]
reaches2 tav w =
    let f (x,y) = tav !! x !! y
        g (w,e) (r,t) = (w+r,e+t)
        heigh = length tav
        widt = length $ head tav
        cond y= (f w /= ord 'a')||(f y /= ord 'a')
        val y
            | y == ord 'S' = ord 'a'
            | y == ord 'E' = ord 'z'
            | otherwise = y
        h = [(0,1),(0,-1),(1,0),(-1,0)]
        ok (x,y) = x>=0 && y>=0 && x<heigh && y < widt
    in filter ((<=val (f w)+1).val.f) . filter cond . filter ok . map (g w) $ h

chartToGraph2 :: [[Int]] -> (Vertex->[Vertex],[Int],Int)
chartToGraph2 tav =
    let heigh = length tav
        widt = length $ head tav
        f (x,y) = tav !! x !! y
        Just dst = find ((==ord 'E').f) coords
        coords = [(i,y)|i<-[0..heigh-1],y<-[0..widt-1]]
        (g,nfv,vfk)=graphFromEdges [(x,x,reaches2 tav x)|x<-coords]
        thrd (_,_,n) = n
        lows = filter ((==ord 'a').f) coords
        (vsri,Just vdst) = (map (fromJust.vfk) lows, vfk dst)
        vsrs = filter (\x->path g x vdst) vsri
    in ((g!), vsrs, vdst)

rount :: (Int->[Int]) -> ([(Int,Int)] , [(Int,Int)]) -> ([(Int,Int)] , [(Int,Int)])
rount _ (_,[]) = ([],[])
rount f (seen, x@(q,i):qs) =
    let nu = zip (f q) $ repeat (i+1)
        flop = map fst (seen++qs)
        nux = filter (not.(`elem`flop).fst) nu
    in (x:seen, qs++nux)

solv1 :: (Vertex->[Vertex],Int,Int) -> Int
solv1 (f,s,d) =
    let se = []
        sq = [(s,0)]
        bff = tail $ iterate (rount f) (se,sq)
        chk x = (not.null.fst$x) && ((==d).fst.head.fst) x
        fin = find chk bff
        dst (Just x)= snd.head.fst $ x
        dst Nothing = maxBound
    in dst fin

solv2 chart =
    let (f,lows,d) = chartToGraph2 chart
        sv1 x = solv1 (f,x,d)
    in minimum . map sv1 $ lows

part1 = openFile "input" ReadMode >>= hGetContents >>= return .
    solv1 . chartToGraph . compr

part2 = openFile "input" ReadMode >>= hGetContents >>= return .
    solv2 . compr

main :: IO ()
main = do
    fhandle <- openFile "input" ReadMode
    fcont <- hGetContents fhandle
    -- print . part1 $ fcont
    return ()
