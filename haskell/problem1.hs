import GHC.CmmToAsm.AArch64.Instr (x0)
import GHC.CmmToAsm.PPC.Regs (tmpReg)
import GHC (ABExport(XABExport))
import GHC.Stg.Lift.Monad (FloatLang)
import GHC.Driver.CmdLine (Flag(Flag))
import GHC.Integer.GMP.Internals (bigNatToInteger)
import GHC.IO.IOMode (IOMode(ReadMode))
import System.IO (openFile, hClose, hGetContents)

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast [_] = undefined
myButLast [x, y] = x
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt [] _ = undefined
elementAt _ 0 = undefined
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) =
    let
        semi = myReverse xs
        result = semi ++ [x]
    in
        result

myReverse' :: [a] -> [a]
myReverse' [] = []
myReverse' [x] = [x]
myReverse' (x:xs) =  myReverse' xs ++ [x]

myReverse'' :: [a] -> [a]
myReverse'' list =
    let
        reversee [] reversed = reversed
        reversee (x:xs) reversed = reversee xs (x:reversed)
    in reversee list []

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []  = False
isPalindrome [b] = True
isPalindrome (x:xs) =
    let
        last_el = last xs
        remaining = init xs
        result = ((x == last_el) && isPalindrome remaining)
    in result

data FlattenData a = Elem a | List [FlattenData a] deriving (Show, Eq)

flatten:: FlattenData a -> [a]
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List [])     = []

compress:: String -> String
compress [] = ""
compress [x] = x : ""
compress (x:ys@(y:_)) =
    let
        result = if x == y then compress ys else x : compress ys
    in result

-- data Encoding a = Item (Int a Char b) | 


pack:: (Eq a) => [a] -> [[a]]
pack [x] = [[x]]
pack (x:xs)
    | x == head heaad = (x:heaad):tail
    | otherwise = [x]:all
    where all@(heaad:tail) = pack xs

bubblesort:: (Ord a) => [a] -> [a]
bubblesort [] = []
bubblesort [x] = [x]
bubblesort l1 =
    let
        current_res = mySort l1
        next_res = mySort current_res
        result = if current_res == next_res then next_res else bubblesort next_res
    in result


quicksort:: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = smaller ++ [x] ++ bigger
    where
        smaller = quicksort [y | y <- xs, y <= x]
        bigger = quicksort [z | z <- xs, z > x]

mySort:: (Ord a) => [a] -> [a]
mySort [] = []
mySort [a] = [a]
mySort (x:y:xs)
    | x > y = y: mySort (x:xs)
    | otherwise = x : mySort (y:xs)

mid:: (Ord a) => [a] -> a
mid [] = undefined
mid [x] = x
mid l1 = middle $ quicksort l1

middle:: (Ord a) => [a] -> a
middle [] = undefined
middle [x] = x
middle l1@(x:xs) =
    let
        new_list = init $ tail l1
        result = if new_list == [] then x else middle new_list
    in result

trim:: String -> String
trim [] = ""
trim l1 =
    let
        trim_start = trimHelper l1
        reversed = myReverse trim_start
        trim_end = myReverse $ trimHelper reversed
        len = lengthString trim_end
    in show len ++ ":" ++ trim_end

trimHelper:: String -> String
trimHelper [] = ""
trimHelper [x] = if x == ' ' then "" else x : ""
trimHelper l1@(x:xs)
    | x == ' ' = trimHelper xs
    | otherwise = l1

lengthString:: String -> Int
lengthString "" = 0
lengthString [a] = 1
lengthString (x:xs) = lengthString xs + 1

processLine:: String -> String
processLine line = show $ trim line

stringSize:: String -> Int
stringSize [] = 0
stringSize [x] = 1
stringSize (x:xs) = stringSize xs + 1

countEmptyLines:: [String] -> Int
countEmptyLines [] = 0
countEmptyLines [x] = if stringSize x == 0 then 1 else 0
countEmptyLines (x:xs) =
    let
        size = if stringSize x == 0 then 1 else 0
    in countEmptyLines xs + size

lenls:: FilePath -> IO()
lenls path = do
    handle <- openFile path ReadMode
    content <- hGetContents handle
    let linesOfFile = lines content
    let emptyLines = countEmptyLines linesOfFile
    let l = map processLine linesOfFile
    putStr ( unlines l ++ "/" ++ show emptyLines)
    hClose handle

data ZA a = ZA Int [a] deriving (Show, Eq)

empty:: ZA a
empty = ZA 0 []

top :: ZA a -> a
top (ZA _ []) = undefined
top (ZA _ (x:xs)) = x

push :: ZA a -> a -> ZA a
push (ZA a list) b =
    let
        new_count = a + 1
    in ZA new_count (b:list)

pop :: ZA a -> ZA a
pop (ZA a (x:xs)) =
    let
        new_count = a - 1
    in ZA new_count xs

pushStr:: ZA a -> [a] -> ZA a
pushStr a [] = a
pushStr z1 (x:xs) =
    let
        (ZA a b) = pushStr z1 xs
        new_count = a + 1
    in ZA new_count ([x] ++ b)

popStr:: Eq a => ZA a -> [a] -> ZA a
popStr a [] = a
popStr z1@(ZA a []) (x:xs) = z1
popStr z1@(ZA a (x1:xs1)) (x2:xs2) =
    let
        result = if x1 == x2 then popStr (pop z1) xs2 else z1
    in result

len :: ZA a -> Int
len (ZA a _) = a

data Rule a = R a [a] [a] Int Int deriving (Show,Eq)

readRules fname = do
    h <- openFile fname ReadMode
    c <- hGetContents h
    let lns = lines c
    let rules = loadLines lns
    (\r -> (return $! myLast r) >> (hClose h >> return r)) $! rules

loadLines [] = []
loadLines (l:ls) = R c popS puS ((read fromS)::Int) ((read l4)::Int) : loadLines ls
    where
        (c:':':l1) = l
        (popS,(_:l2)) = span (/=':') l1
        (puS,(_:l3))  = span (/=':') l2
        (fromS,(_:l4)) = span (/=':') l3