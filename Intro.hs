import Data.List
import System.IO

{-
usage: boomBangs [x..Y] - Divides list by 2 as it only takes odds,
        but for a list will return boom when less than 10 and bang when over 10
-}
boomBangs :: Integral a => [a] -> [[Char]]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x] 

{-
usage: length' x - Returns the length of a list, string, etc
-}
length' :: Num a => [t] -> a
length' xs = sum [1 | _ <- xs]

length'' :: (Num b) => [a] -> b  
length'' [] = 0  
length'' (_:xs) = 1 + length'' xs  

{-
Removes non uppercase letters from 
-}
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   


{-
let rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2] 
returns all right angles triangles which have a hypotenuse (c) under 10
-}


{-
usage: addThree x y z - Adds x, y and z and returns the result
Allows you to add 3 Integers together and returns the result
-}
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

{-
usage: circumference r - calculates the circumference of a circle the given radius
-}
circumference :: Float -> Float  
circumference r = 2 * pi * r 


{-
usage: sayMe(') x - If x is less than (LT) 4 then, print the the formated string.
Otherwise prints out of bounds/not within 3
-}
sayMe :: (Integral a) => a -> String 
sayMe x
  | x == 1 = "One!"
  | x == 2 = "Two!"
  | x == 3 = "Three!"
  | otherwise = "Not within 3"

sayMe' :: (Integral a) => a -> String 
sayMe' x =
  case x of
          1 -> "One!"
          2 -> "Two!"
          3 -> "Three!"
          x -> "Out of bounds"

sayMe'' :: (Integral a) => a -> String  
sayMe'' 1 = "One!"  
sayMe'' 2 = "Two!"  
sayMe'' 3 = "Three!"  
sayMe'' 4 = "Four!"  
sayMe'' 5 = "Five!"  
sayMe'' x = "Not between 1 and 5"  

{-
usage: factorial(') x - Returns the factorial of x (product adds all values in the list)
and [1..n] represents a list containing all values between 1 and n.
-} 
factorial :: Integer -> Integer  
factorial n = product [1..n]

factorial' :: (Integral a) => a -> a 
factorial' 0 = 1  
factorial' n = n * factorial' (n - 1)

{-
usage: addVectors (x1,y1) (x2,y2) - Function that takes two vectors in a 2D space that
are in the form of pairs, and adds them together. (Pattern matching)
-}
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

{-
usage: (first/second/third) (a, b, c) - Returns the value in the corresponding position
based on what was passed as the values.
-}
first :: (a, b, c) -> a  
first (x, _, _) = x  
  
second :: (a, b, c) -> b  
second (_, y, _) = y  
  
third :: (a, b, c) -> c  
third (_, _, z) = z

{-
let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
[a+b | (a,b) <- xs]
returns [4,7,6,8,11,4]  
Takes a list consisting of tuples and adds the key and value together
-}

{-Sums all values in a list and returns the result-}
sum' :: (Num a) => [a] -> a  
sum' [] = 0  
sum' (x:xs) = x + sum' xs


{-
usage: capital x - returns the first letter of a string

Note: Using {}@(x:xs), this enables you to call the head by using x, call the rest of the
list by using xs and then calling the entire list using {}

Note: You can't use ++ in pattern matches
-}
capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

{-
usage: bmiTell x - Tells you whether your bmi is appropriate or if its not.
usage: bmiTell' x y - Tells you whether your weight and height are normal, underweight or
        overweight
-}
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
  | bmi <= 18.5 = "You're underweight, you emo, you!"  
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
  | otherwise   = "You're a whale, congratulations!"

bmiTell' :: (RealFloat a) => a -> a -> String  
bmiTell' weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!"

bmiTell'' :: (RealFloat a) => a -> a -> String  
bmiTell'' weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2

bmiTell''' :: (RealFloat a) => a -> a -> String  
bmiTell''' weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0

{-
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
bmiTell''' is the most optimal out of the 4 solutions although can be modified to:
```Haskell
...  
where bmi = weight / height ^ 2  
      (skinny, normal, fat) = (18.5, 25.0, 30.0)
```
Replacing this with the equivalent in bmiTell''' is the most optimal solution for this
        question.
-}

{-
usage: max x y - Returns x or y in correspondance to which one is larger
-}
max' :: (Ord a) => a -> a -> a  
max' a b   
    | a > b     = a  
    | otherwise = b

max'' :: (Ord a) => a -> a -> a  
max'' a b | a > b = a | otherwise = b

{-
usage: a `mycompare` b - Returns GT, EQ, LT in correspondance to whether
        x is bigger, equal or smaller than y
-}
myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT

{-
usage: initials f l - f and l are respectful to firstname and lastname, this code snippet
        returns the first letter of someone's firstname and lastname
-}
initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname

{-
usage: calcBmis - Takes a list of weight-height pairs and returns a list of BMIs
usage: calcBmis (x, y) - Tells you what your BMI by inputting your weight and height
        respectfully into x and y
-}
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

{-
Only adds to to the list if the output of the bmi calculation is over 25, so it filters out
        all other BMI's
-}
calcBmis'' :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis'' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0] 

{-
usage: cylinder x y - Returns the surface area of a cylinder using the
        `let <definitions> in <expression>`
-}
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea 

{-
Example of how case expression works:
case expression of pattern -> result  
                   pattern -> result  
                   pattern -> result  
                   ...  
-}

{-
Example: writting head using patterns and writting it in a case expression
usage: head' [arr] - returns the first value of a list, or error message when list is empty
usage: head'' [arr] - returns the first value of a list, or error message when list is empty
-}
head' :: [a] -> a  
head' [] = error "No head for empty lists!"  
head' (x:_) = x  

head'' :: [a] -> a  
head'' xs = case xs of [] -> error "No head for empty lists!"
                       (x:_) -> x

{-
Tells you whether a list is emty, has 1 item or has more than 1
-}
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."

{-samething but instead of using case, it uses where-}
describeList' :: [a] -> String  
describeList' xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."

{-
usage: maximum' [] - Returns the biggest value in the inputted list
-}
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs
{-
Same format only just adding another ('), only thing is the code is more clean
-}
maximum'' :: (Ord a) => [a] -> a  
maximum'' [] = error "maximum of empty list"  
maximum'' [x] = x  
maximum'' (x:xs) = max x (maximum' xs)

{-
usgae: repliacate' n x - Generates a list consisting of x repeated n-1 amounts of times
-}
replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x


{-
usage: take' x [] - Takes the first x elements from the list and returns them as a separate list
-}
take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs

{-
usage: reverse' [] - Reverses the inputted list
-}
reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]

{-
usage: repeat' x - Creates an infinite list that containts x
-}
repeat' :: a -> [a]  
repeat' x = x:repeat' x  

{-
usage: zip' [] [] - Combines 2 values of 2 lists and returns it as a tuple within a list
usage: zip'' [] [] - Combines 4 values of 2 lists and returns it as a tuple within a list
-}
zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

zip'' :: [a] -> [b] -> [(a,b,b,a)]  
zip'' _ [] = []  
zip'' [] _ = []  
zip'' (x:z:xs) (y:j:ys) = (x,y,j,z):zip'' xs ys 

{-
usage: elem' x [] - Returns a boolean value depending if x is in the list
-}
elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs

{-
usage: quicksort [] - Performs quicksort on the list inputted and sorts it from smallest
        to largest
-}
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  