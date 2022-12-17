module Main where
import System.IO
import Data.List (break,sort)

data Fs = Dir [Fs] | Leaf Int deriving (Show)

data Bash = Cd | CdUp | Blob Int deriving (Eq,Show)

textToBash :: String -> [Bash]
textToBash = map match.filter (not.hasBadWord).map words.lines
    where
    match x
        | ".." `elem` x = CdUp
        | "cd" `elem` x = Cd
        | otherwise = Blob (read (head x)::Int)
    hasBadWord x = elem "ls" x || elem "dir" x

checkReader :: String -> [String]
checkReader = map match.filter (not.hasBadWord).map words.lines
    where
    match x
        | ".." `elem` x = "CdUp"
        | "cd" `elem` x = "Cd"
        | otherwise = head x
    hasBadWord x = elem "ls" x || elem "dir" x

totalSize :: Fs -> Int
totalSize (Leaf x) = x
totalSize (Dir x) = sum . map totalSize $ x

treeOfBash :: [Bash] -> Fs
treeOfBash x = head (filesOfBash x)

filesOfBash :: [Bash] -> [Fs]
filesOfBash [] = []
filesOfBash (Blob x:xs) = Leaf x:filesOfBash xs
filesOfBash (Cd:xs) =
    let domain = untilLeave xs
        branch = filesOfBash domain
        rest = drop (length domain) xs
    in Dir branch:filesOfBash rest
filesOfBash (CdUp:xs) = filesOfBash xs

untilLeave :: [Bash] -> [Bash]
untilLeave =
    reverse.snd . foldl leaveFind (1,[])

leaveFind :: (Int,[Bash]) -> Bash -> (Int,[Bash])
leaveFind a@(0,CdUp:_) _ = a
leaveFind (i,x) y = (f y i,y:x)
    where f CdUp = \t->t-1
          f Cd = (+ 1)
          f _ = id

smallFolders :: Fs -> (Int,Int)
smallFolders (Leaf x) = (x,0)
smallFolders (Dir x) =
    let children = map smallFolders x
        tapply f (x,y) = (f x,f y)
        (mySize,cnt) = tapply sum.unzip $ children
        mySmall = if mySize<=100000 then mySize else 0
    in (mySize,cnt+mySmall)

justEnough :: Fs -> Int -> (Int,Int)
justEnough (Leaf x) t = (x,maxBound)
justEnough (Dir x) tresh =
    let children = map (`justEnough` tresh) x
        (chSizes,chBest) = unzip children
        mySize = sum chSizes
        prevBest = minimum chBest
        myBest = if mySize>=tresh && mySize <= prevBest then mySize else prevBest
    in (mySize,myBest)



main :: IO ()
main = do
    fhandle <- openFile "input" ReadMode
    fcont <- hGetContents fhandle
    let root = treeOfBash .textToBash $ fcont
        totalUsed = fst . smallFolders $ root
        threshold =  totalUsed - 40000000
    print $ justEnough root threshold
    return ()
