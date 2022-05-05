module Chapter6 where
import Data.List
import System.IO
import Data.List

{-
usgae: compareWithHundred x - Compares x with 100 by using curried functions
-}
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare x 100

{-
usage: divideByTen x - Divides x by 10 using infix functions
-}
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

{-
usage: divideHudno - Returns 10 as 100/10 = 10
-}
divideHundo = divideByTen 100

{-
usage: isUpperAlphanum x - x represents a 'Char' and will return whether
    or not x is an Uppercase letter.
-}
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

{-
Example that functions can take functions as parameters and return functions
-}
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

{-
usage: zipWith' (operator) [] [] - Applied the operator and performs
    the operation over the 2 lists (so allows you to subtract, add, etc)
    meanwhile you want to apply this over 2 lists of equal length, etc
    to be able to calculate things such as vectors, etc.
-}
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
{-
Examples:
ghci> zipWith' (+) [4,2,5,6] [2,6,2,3]  
[6,8,7,9]  
ghci> zipWith' max [6,3,2,1] [7,3,1,5]  
[7,3,2,5]  
ghci> zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]  
["foo fighters","bar hoppers","baz aldrin"]  
ghci> zipWith' (*) (replicate 5 2) [1..]  
[2,4,6,8,10]  
ghci> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]  
[[3,4,6],[9,20,30],[10,12,12]] 
-}

{-
usage: flip' x - Flip takes a function and returns a function similar to the original
    but the first 2 arguments are flipped.
-}
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x
-- Other method of being written
flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y
-- Flip function but made using recursion
flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f = \x y -> f y x

{-
usage: map' _ [] - Takes a function and applied the function to every element in the list
    _ represents anything can be put there (*, +, - , etc)
-}
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs
{-
Examples:
ghci> map (+3) [1,5,3,1,6]  
[4,8,6,4,9]  
ghci> map (++ "!") ["BIFF", "BANG", "POW"]  
["BIFF!","BANG!","POW!"]  
ghci> map (replicate 3) [3..6]  
[[3,3,3],[4,4,4],[5,5,5],[6,6,6]]  
ghci> map (map (^2)) [[1,2],[3,4,5,6],[7,8]]  
[[1,4],[9,16,25,36],[49,64]]  
ghci> map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]  
[1,3,6,2,2]
-}

{-
usage: filter' () [] - Applies the opperator on the list
-}
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs
{-
Examples:
ghci> filter (>3) [1,5,3,2,1,6,4,3,2,1]  
[5,6,4]  
ghci> filter (==3) [1,2,3,4,5]  
[3]  
ghci> filter even [1..10]  
[2,4,6,8,10]  
ghci> let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]  
[[1,2,3],[3,4,5],[2,2]]  
ghci> filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"  
"uagameasadifeent"  
ghci> filter (`elem` ['A'..'Z']) "i lauGh At You BecAuse u r aLL the Same"  
"GAYBALLS"  
-}

{-
usage: quicksort [] - performs quicksort on the list
-}
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted

{-
usage: largestDivisible - returns the largest number divisible by 3829 
    thats smaller than 100k
-}
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

{-
usage: chain x - Creates a list that starts with n and ends with 1, if n is
    even, it divides n by 2, otherwise it multiplies it by 3 and adds 1 to n
usage: numLongChains - Returns the number of lists for the chains between 1
    and 100 returns the quantity that are longer than 15
-}
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n =  n:chain (n `div` 2)
    | odd n  =  n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15
-- Use a Lamdba (function)
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

{-
Lambda function example for the lists (applying the function as an operator)
ghci> zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]  
[153.0,61.5,31.0,15.75,6.6] 

ghci> map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]  
[3,8,9,8,7
-}

{-
usage: addThree(') x y z - Adds x, y and z together
-}
addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

addThree' :: (Num a) => a -> a -> a -> a
addThree' = \x -> \y -> \z -> x + y + z

{-
usage: sum(','') [] - Performs a sum calculation over a list using the foldl function
    and a Lambda function
-}
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

sum''' :: (Num a) => [a] -> a
sum''' xs = foldl (+) 0 xs

{-
usage: elem' x [] - Returns whether x is in the list[]. it uses foldl to loop
    through all the values in the list, and it used the lambda expression
    to pass the rest of the list through (using acc)
-}
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys


{-
Examples of foldl1, foldr1, foldl and foldr
-}
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

{-
ghci> scanl (+) 0 [3,5,2,1]  
[0,3,8,10,11]  
ghci> scanr (+) 0 [3,5,2,1]  
[11,8,3,1,0]  
ghci> scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]  
[3,4,5,5,7,9,9,9]  
ghci> scanl (flip (:)) [] [3,2,1]  
[[],[3],[2,3],[1,2,3]]  
-}

{-
usage: sqrtSums - Returns how many elements does it take for the sum of the
    roots of all natural numbers to exceed 1000
-}
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1



{- Saves us time as $ has the lower precedence, so we don't have to use ()
    as they have high precedence-}
--($) :: (a -> b) -> a -> b
--f $ x = f x


{-
Example:
ghci> map ($ 3) [(4+), (10*), (^2), sqrt]  
[7.0,30.0,9.0,1.7320508075688772] 
-}



-- Function composition, so joining to functions to make a new one i.e f(g(x))
--(.) :: (b -> c) -> (a -> b) -> a -> c
--(f . g)x = f(g x)



{-
first finds the absolute values and then negates it
ghci> map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]  
[-5,-3,-6,-7,-3,-2,-19,-24]

writen with function composition
ghci> map (negate . abs) [5,-3,-6,7,-3,2,-19,24]  
[-5,-3,-6,-7,-3,-2,-19,-24]

f (g (z x)) is EQUIVALENT to (f . g . z) x
ghci> map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]  
[-14,-15,-27] 
ghci> map (negate . sum . tail) [[1..5],[3..6],[1..7]]  
[-14,-15,-27]  
-}


fn' :: (RealFrac a, Integral b, Floating a) => a -> b
fn' x = ceiling (negate (tan (cos (max 50 x))))
fn'' :: Double -> Integer
fn'' = ceiling . negate . tan . cos . max 50

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSum' :: Integer  
oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSum'' :: Integer  
oddSquareSum'' =   
    let oddSquares = filter odd $ map (^2) [1..]  
        belowLimit = takeWhile (<10000) oddSquares  
    in  sum belowLimit