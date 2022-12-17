module Main where
import System.IO

linetoPairs :: String -> ((Int, Int),(Int, Int))
linetoPairs l =
    let (x,_:y) = break (==',') l
        (x1,_:x2) = break (=='-') x
        (y1,_:y2) = break (=='-') y
    in ((read x1, read x2),(read y1, read y2))

pairsContain :: (Int, Int) -> (Int, Int) -> Bool
pairsContain (x1,x2) (y1,y2) =
    (x1 <= y1) && (y2 <= x2)

eitherContains :: ((Int, Int) , (Int, Int)) -> Bool
eitherContains (x, y) =
    pairsContain x y || pairsContain y x

containsCount :: [String] -> Int
containsCount = length . filter eitherContains . map linetoPairs

pairOverlaps :: (Int, Int) -> (Int, Int) -> Bool
pairOverlaps (x1,x2) (y1,y2) =
    (x1 <= y1) && (y1 <= x2)

eitherOverlaps :: ((Int, Int) , (Int, Int)) -> Bool
eitherOverlaps (x, y) =
    pairOverlaps x y || pairOverlaps y x

overlapCount :: [String] -> Int
overlapCount = length . filter eitherOverlaps . map linetoPairs

main :: IO ()
main = do
    fhandle <- openFile "inputd4" ReadMode
    fcont <- hGetContents fhandle
    print (overlapCount . lines $ fcont)
    return ()
