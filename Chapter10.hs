module Chapter10 where
import Data.List
import qualified GHC.Tuple as Range

{-
            Reverse Polish notation calculator
-}


{-
Allows you to calculate the Reverse Polish Notation of a set of values, so it
    calculates it in the form of a list (or stack)
-}
solveRPN :: (Num a, Read a, Floating a) => String -> a
solveRPN = head . foldl foldingFunction [] . words
    where
        foldingFunction (x:y:ys) "*" = (x * y):ys  
        foldingFunction (x:y:ys) "+" = (x + y):ys  
        foldingFunction (x:y:ys) "-" = (y - x):ys  
        foldingFunction (x:y:ys) "/" = (y / x):ys  
        foldingFunction (x:y:ys) "^" = (y ** x):ys  
        foldingFunction (x:xs) "ln" = log x:xs  
        foldingFunction xs "sum" = [sum xs]  
        foldingFunction xs numberString = read numberString:xs

{-
ghci> solveRPN "2.7 ln"  
0.9932518  
ghci> solveRPN "10 10 10 10 sum 4 /"  
10.0  
ghci> solveRPN "10 10 10 10 10 sum 4 /"  
12.5  
ghci> solveRPN "10 2 ^"  
100.0

ghci> solveRPN "43.2425 0.5 ^"
6.575903
-}


--Defines the Nodes and Road Weightings
data Node = Node Road (Maybe Road)  
data Road = Road Int Node

-- Section is the distance as an interval. RoadSystem just builds the Road
data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)  
type RoadSystem = [Section]

-- Label type is just an enumeration of either A, B or C. We'll also make a type synonym: Path.
data Label = A | B | C deriving (Show)  
type Path = [(Label, Int)] 

-- Represents the road system for Heathrow and London
heathrowToLondon :: RoadSystem  
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]


{-
First, this function calculates the optimal price on road A based on the best
    so far on A and we do the same for B.
We do sum $ map snd pathA, so if pathA is something like [(A,100),(C,20)],
    priceA becomes 120.
forwardPriceToA is the price that we would pay if we went to the next crossroads
    on A if we went there directly from the previous crossroads on A. It equals the best price to our previous A, plus the length of the A part of the current section
-}
roadStep :: (Path, Path) -> Section -> (Path, Path)  
roadStep (pathA, pathB) (Section a b c) =   
    let priceA = sum $ map snd pathA  
        priceB = sum $ map snd pathB  
        forwardPriceToA = priceA + a  
        crossPriceToA = priceB + b + c  
        forwardPriceToB = priceB + b  
        crossPriceToB = priceA + a + c  
        newPathToA = if forwardPriceToA <= crossPriceToA  
                        then (A,a):pathA  
                        else (C,c):(B,b):pathB  
        newPathToB = if forwardPriceToB <= crossPriceToB  
                        then (B,b):pathB  
                        else (C,c):(A,a):pathA  
    in  (newPathToA, newPathToB)

{-
What the function returns when you pass empty parameters

ghci> roadStep ([], []) (head heathrowToLondon)  
([(C,30),(B,10)],[(B,10)])
-}

optimalPath :: RoadSystem -> Path  
optimalPath roadSystem = 
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem  
    in  if sum (map snd bestAPath) <= sum (map snd bestBPath)  
            then reverse bestAPath  
            else reverse bestBPath

{-
ghci> optimalPath heathrowToLondon  
[(B,10),(C,30),(A,5),(C,20),(B,2),(B,8),(C,0)]
-}

{-
Takes a list of ints, and groups them in sets of 3
-}
groupsOf :: Int -> [a] -> [[a]]  
groupsOf 0 _ = undefined  
groupsOf _ [] = []  
groupsOf n xs = take n xs : groupsOf n (drop n xs)

{-
usage: cat (file).txt | runhaskell heathrow.hs  
- Takes groupped list and returns the optimal path and its price for a given
    Range.
- the Lambah function creates sections of length 3 and appends it to a list
- The set comprised of Lambah function sections is the roadSystem map
-}
main :: IO ()
main = do  
    contents <- getContents  
    let threes = groupsOf 3 (map read $ lines contents)  
        roadSystem = map (\[a,b,c] -> Section a b c) threes  
        path = optimalPath roadSystem  
        pathString = concat $ map (show . fst) path  
        pathPrice = sum $ map snd path  
    putStrLn $ "The best path to take is: " ++ pathString  
    putStrLn $ "The price is: " ++ show pathPrice 

{- (File).txt contents
50  
10  
30  
5  
90  
20  
40  
2  
25  
10  
8  
0  
-}

{-
ghci> roadStep ([], []) (head heathrowToLondon)  
([(C,30),(B,10)],[(B,10)])

$ cat paths.txt | runhaskell heathrow.hs  
The best path to take is: BCACBBC  
The price is: 75  
-}