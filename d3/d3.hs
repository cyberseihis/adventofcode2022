module Main where
import System.IO
import Data.Char (ord, isUpper)

splitLine :: String -> (String, String)
splitLine x =
    let n = length x `div` 2
    in splitAt n x

commonLetter :: (String, String) -> String
commonLetter (x,y) =
    filter (`elem` y) x

alphaord :: Char -> Int
alphaord x =
    let up = if isUpper x then 38 else 96
    in ord x - up

totalOverlap :: [String] -> Int
totalOverlap = sum . map (alphaord . head . commonLetter . splitLine)

threeLines :: [String] -> [(String, String, String)]
threeLines [] = []
threeLines (x:y:z:xs) = 
    (x,y,z):threeLines xs

commonThree :: (String, String,String) -> String
commonThree (x,y,z) =
    filter (`elem` z) . filter (`elem` y) $ x

threelap :: [String] -> Int
threelap = sum . map (alphaord . head . commonThree) . threeLines 

main :: IO ()
main = do
    fhandle <- openFile "input" ReadMode
    fcont <- hGetContents fhandle
    print $ totalOverlap . lines $ fcont
    print $ threelap . lines $ fcont
    return ()
