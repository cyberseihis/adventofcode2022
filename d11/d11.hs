module Main where
import System.IO
import Data.List (break,sort, transpose)
import Data.Sequence (update, Seq, (!?), fromList)
import Data.Foldable (toList)

data Monkey = Monkey {
    items :: [Int],
    op :: Int -> Int,
    dib :: Int -> Bool,
    addr0 :: Int,
    addr1 :: Int,
    cnt :: Int
}

instance Show Monkey where
    show = show . items

splitUpon :: (Eq a) => a -> [a] -> [[a]]
splitUpon _ [] = []
splitUpon x (_:ys) =
    let f = break (==x)
        (a,b) = f ys
    in a:splitUpon x b

compr :: String -> Seq Monkey
compr = fromList . map makeMonkey . splitUpon "" .("":). lines

makeMonkey :: [String] -> Monkey
makeMonkey (_:ar:oop:xs) =
    let [divi,m0,m1] = [read.last.words$x|x<-xs]
        par = read.(++"]").('[':).unwords.drop 2.words$ar
        oops = readOp . drop 4 . words $ oop
    in Monkey {items=par, op=oops, dib=(==0).(`mod`divi), addr0=m0, addr1=m1,cnt=0}

readOp :: [String] -> Int -> Int
readOp [op,i]
    | i == "old" = (^2)
    | op == "+" = (+read i)
    | op == "*" = (*read i)

targets :: Monkey -> [Int]
targets Monkey {items=[]}=[]
targets man@(Monkey {items=(item:xs), op=op, dib=dib, addr0=m0, addr1=m1}) =
    let x = dib . op $ item
    in (if x then m0 else m1):targets man{items=xs}

throw ::  Int -> Int -> Seq Monkey ->Seq Monkey
throw x y zoo =
    let Just origin = zoo !? x
        Just dest = zoo !? y
        Monkey {items=(item:rest)} = origin
        nuOrig = origin{items=rest,cnt=cnt origin+1}
        xItem = (`mod`9699690) . op origin $ item
        nuDest = dest{items=items dest++[xItem]}
    in update x nuOrig . update y nuDest $ zoo

turn :: Int -> Seq Monkey -> Seq Monkey
turn i zoo =
    let Just man = zoo !? i
        trgts = reverse . targets $ man
        f = foldl (.) id . map (throw i) $ trgts
    in f zoo

rount :: Seq Monkey -> Seq Monkey
rount x =
    let n = length x
        f = foldl (.) id . map turn . reverse $ [0..n-1]
    in f x

solv = product.take 2.reverse . sort . map cnt .toList.(!!20) . iterate rount
solv2 = product.take 2.reverse . sort . map cnt .toList.(!!10000) . iterate rount
part1 = solv . compr
part2 = solv2 . compr

fetchInput = openFile "input" ReadMode >>= hGetContents
fetchDemo = openFile "demo" ReadMode >>= hGetContents

main :: IO ()
main = do
    fcont <- fetchInput
    fdem <- fetchDemo
    print . part2 $ fdem
    print . part2 $ fcont
    return ()
