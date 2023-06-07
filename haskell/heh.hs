import GHC.IO.IOMode
import System.IO

fx :: FilePath -> IO ()
fx f = do
    h <- openFile f ReadMode
    o <- openFile (f ++ ".out") WriteMode
    c <- hGetContents h
    hPutStr o $ unlines $ numlines 1 $ lines c
    hClose o
    hClose h

wrap n (a:b:ls)
    | (length a + length b) <= 120 = wrap n ((a++b):ls)
    | True = (show n ++ ":" ++ a) : wrap (n+1) (b:ls)
wrap n [l] = [show n ++ ":" ++ l]
wrap _ [] = []


numlines :: Int -> [String] -> [String]
numlines _ [] = []
numlines n (a:as) = (show n ++ ": " ++ a) : numlines (n+1) as

suf [] = [[]]
suf l@(_:xs) = l : suf xs

pre [] = [[]]
pre l2 = 
    let
        prefix = init l2 
    in l2 : pre prefix


init' [] = []
init' [a] = []
init' (x:xs) = x : init' xs

substrs [] = []
substrs [a] = a 
substrs (x:xs) = 
    let
        charLen = 1
    in charLen


data PD a = PD Int [a] deriving (Show,Eq)

empty :: PD a
empty = PD 0 []

top (PD _ (x:_)) = x

push (PD s l) v = PD (s+1) (v:l)

pop (PD s (x:xs)) = (x,PD (s-1) xs)

len (PD s _) = s

pushStr l [] = l
pushStr l [a] = push l a
pushStr l (x:xs) = result
    where
        new_L = push l x
        result = pushStr new_L xs

