import System.Environment ( getArgs )


quick :: Ord a => [a] -> [a]
quick [] = []
quick [x] = [x]
quick (x:xs) = smaller ++ [x] ++ larger
   where
       smaller = quick [y | y <- xs , y <= x]
       larger = quick [y | y <- xs, y > x]
 
myInit [] = undefined	
myInit [x] = []
myInit (x:xs) = x:myInit xs  
 
myTail [] = undefined
myTail [x] = []
myTail (x:xs) = xs
 
mid :: Ord a => [a] -> a
mid [] = undefined
mid [x] = x
mid [x,y] = x
mid xs = mid (myInit $ myTail $ quick xs)

-- main function, orchestrates the program
main :: IO()
main = do
    -- arguments parsing
    args <- getArgs
    let a = [3,43,4,8,9,10,5,98,1,2,16]  
    print (mid a)