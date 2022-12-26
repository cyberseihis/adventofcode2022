module Santa (
    wrap, wrD, wrI
) where

import System.IO
import Data.Functor ((<&>))

wrap :: String -> ([String]->a) -> IO a
wrap name f = (openFile name ReadMode >>= hGetContents) <&> (f . lines)

wrD = wrap "demo"
wrI = wrap "input"
