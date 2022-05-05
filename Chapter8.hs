module Chapter8 where
import Data.List
import System.IO
import Data.List
import Data.Map
import Data.Set
import qualified Data.Map as Map

{-
Algebraic data type intro:

Using the `data` keyword at the begining you are able to define your own data type.
    The parts after the '=' are value contructors.
-}
-- How standard library would sort of define Bool
-- data Bool = False | True

{-
- How the standard library would define Int
data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647
-}

{-
Example of how we can create and define our own type having `deriving (Show)` automatically makes it part of
    the Show typeclass.

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

surface :: Shape -> Float  
surface (Circle _ _ r) = pi * r ^ 2  
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
-}

-- The following 2 are examples of defining a type or a typeclass
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
{-
ghci> surface (Rectangle (Point 0 0) (Point 100 100))  
10000.0  
ghci> surface (Circle (Point 0 0) 24)  
1809.5574 
-}

{-
usage: nudge (Circle (Point 34 34) 10) 5 10  
expected: Circle (Point 39.0 44.0) 10.0

Takes an object and "nudges" (a.k.a. Transposes) it to the new position

ghci> nudge (baseRect 40 100) 60 23  
Rectangle (Point 60.0 23.0) (Point 100.0 123.0)
-}
nudge :: Shape -> Float -> Float -> Shape  
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r  
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape  
baseCircle r = Circle (Point 0 0) r  
  
baseRect :: Float -> Float -> Shape  
baseRect width height = Rectangle (Point 0 0) (Point width height)


{-
           Record Syntax
-}

data Person = Person String String Int Float String String deriving (Show)
{-
ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"  
ghci> guy  
Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate" 
-}

firstName :: Person -> String  
firstName (Person firstname _ _ _ _ _) = firstname  
  
lastName :: Person -> String  
lastName (Person _ lastname _ _ _ _) = lastname  
  
age :: Person -> Int  
age (Person _ _ age _ _ _) = age  
  
height :: Person -> Float  
height (Person _ _ _ height _ _) = height  
  
phoneNumber :: Person -> String  
phoneNumber (Person _ _ _ _ number _) = number  
  
flavor :: Person -> String  
flavor (Person _ _ _ _ _ flavor) = flavor

{-
ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"  
ghci> firstName guy  
"Buddy"  
ghci> height guy  
184.2  
ghci> flavor guy  
"Chocolate"
-}


{-
More efficient way of writting the datatype for Person
-}
data Person' = Person' { firstName' :: String  
                     , lastName' :: String  
                     , age' :: Int  
                     , height' :: Float  
                     , phoneNumber' :: String  
                     , flavor' :: String  
                     } deriving (Show)

data Car = Car String String Int deriving (Show)
{-
ghci> Car "Ford" "Mustang" 1967  
Car "Ford" "Mustang" 1967
-}

-- Optimized:
data Car' = Car' {company :: String, model :: String, year :: Int} deriving (Show)
{-
ghci> Car {company="Ford", model="Mustang", year=1967}  
Car {company = "Ford", model = "Mustang", year = 1967}
-}

{-
           Type Parameters
-}

-- Will always hold nothing, or Maybe something as no one type can have only the type parameter Maybe
-- How standard library defines Maybe:
--data Maybe a = Nothing | Just a

{-
ghci> Just "Haha"  
Just "Haha"  
ghci> Just 84  
Just 84  
ghci> :t Just "Haha"  
Just "Haha" :: Maybe [Char]  
ghci> :t Just 84  
Just 84 :: (Num t) => Maybe t  
ghci> :t Nothing  
Nothing :: Maybe a  
ghci> Just 10 :: Maybe Double  
Just 10.0 
-}

data Car'' = Car'' { company' :: String  
               , model' :: String  
               , year' :: Int  
               } deriving (Show)

tellCar :: Car'' -> String  
tellCar (Car'' {company' = c, model' = m, year' = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
{-
ghci> let stang = Car {company="Ford", model="Mustang", year=1967}  
ghci> tellCar stang  
"This Ford Mustang was made in 1967"
-}

data Car''' a b c = Car''' { company'' :: a  
                     , model'' :: b  
                     , year'' :: c   
                     } deriving (Show)

tellCar' :: (Show a) => Car''' String String a -> String  
tellCar' (Car''' {company'' = c, model'' = m, year'' = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

{-
ghci> tellCar (Car "Ford" "Mustang" 1967)  
"This Ford Mustang was made in 1967"  
ghci> tellCar (Car "Ford" "Mustang" "nineteen sixty seven")  
"This Ford Mustang was made in \"nineteen sixty seven\""  
ghci> :t Car "Ford" "Mustang" 1967  
Car "Ford" "Mustang" 1967 :: (Num t) => Car [Char] [Char] t  
ghci> :t Car "Ford" "Mustang" "nineteen sixty seven"  
Car "Ford" "Mustang" "nineteen sixty seven" :: Car [Char] [Char] [Char]
-}

data Vector a = Vector a a a deriving (Show)  
-- Adds two vectors together
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
-- Multiples vector with scalar
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
-- Scalar product of two vectors
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

{-
ghci> Vector 3 5 8 `vplus` Vector 9 2 8  
Vector 12 7 16  
ghci> Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3  
Vector 12 9 19  
ghci> Vector 3 9 7 `vectMult` 10  
Vector 30 90 70  
ghci> Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0  
74.0  
ghci> Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)  
Vector 148 666 222
-}


{-
           Derived Instances
-}
data Person'' = Person'' { firstName'' :: String  
                     , lastName'' :: String  
                     , age'' :: Int  
                     } deriving (Eq)

{-
ghci> let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}  
ghci> let adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}  
ghci> let mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}  
ghci> mca == adRock  
False  
ghci> mikeD == adRock  
False  
ghci> mikeD == mikeD  
True  
ghci> mikeD == Person {firstName = "Michael", lastName = "Diamond", age = 43}  
True

ghci> let beastieBoys = [mca, adRock, mikeD]  
ghci> mikeD `elem` beastieBoys  
True 
-}

data Person''' = Person''' { firstName''' :: String  
                     , lastName''' :: String  
                     , age''' :: Int  
                     } deriving (Eq, Show, Read)

{-
ghci> let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}  
ghci> mikeD  
Person {firstName = "Michael", lastName = "Diamond", age = 43}  
ghci> "mikeD is: " ++ show mikeD  
"mikeD is: Person {firstName = \"Michael\", lastName = \"Diamond\", age = 43}"
-}

{-
Read is the opposite of show so instead of converting values of our type to a string, its from string to type.

ghci> read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person  
Person {firstName = "Michael", lastName = "Diamond", age = 43}  

ghci> read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" == mikeD  
True 
-}

{-
We can think of Bool as being constructed as data Bool = False | True deriving (Ord)

ghci> True `compare` False  
GT  
ghci> True > False  
True  
ghci> True < False  
False  
-}

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
data Day' = Monday' | Tuesday' | Wednesday' | Thursday' | Friday' | Saturday' | Sunday'   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
{-
- Because it's part of the Show and Read typeclasses, we can convert values of this type to and from strings.
ghci> Wednesday  
Wednesday  
ghci> show Wednesday  
"Wednesday"  
ghci> read "Saturday" :: Day  
Saturday

- Because it's part of the Eq and Ord typeclasses, we can compare or equate days.
ghci> Saturday == Sunday  
False  
ghci> Saturday == Saturday  
True  
ghci> Saturday > Friday  
True  
ghci> Monday `compare` Wednesday  
LT

- It's also part of Bounded, so we can get the lowest and highest day.
ghci> minBound :: Day  
Monday  
ghci> maxBound :: Day  
Sunday

- It's also an instance of Enum. We can get predecessors and successors of days and we can make list ranges from them
ghci> succ Monday  
Tuesday  
ghci> pred Saturday  
Friday  
ghci> [Thursday .. Sunday]  
[Thursday,Friday,Saturday,Sunday]  
ghci> [minBound .. maxBound] :: [Day]  
[Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday] 
-}



{-
           Type Synonyms
-}

-- How the standard library defines string:
--type String = [Char]

phoneBook :: [(String,String)]  
phoneBook =      
    [("betty","555-2938")     
    ,("bonnie","452-2928")     
    ,("patsy","493-2928")     
    ,("lucille","205-2928")     
    ,("wendy","939-8282")     
    ,("penny","853-2492")     
    ]

type PhoneNumber = String  
type Name = String  
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool  
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

type AssocList k v = [(k,v)]

{-
a function that gets the value by a key in an association list can have a type of (Eq k) => k -> AssocList k v -> Maybe v
-}

type IntMap v = Map Int v
-- or type IntMap = Map Int

-- How the standard library would define Either:
--data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

{-
ghci> Right 20  
Right 20  
ghci> Left "w00t"  
Left "w00t"  
ghci> :t Right 'a'  
Right 'a' :: Either a Char  
ghci> :t Left True  
Left True :: Either Bool b
-}
  
data LockerState = Taken | Free deriving (Show, Eq)  
  
type Code = String  
  
type LockerMap = Map.Map Int (LockerState, Code)

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ] 

lockerLookup :: Int -> LockerMap -> Either String Code  
lockerLookup lockerNumber map =   
    case Map.lookup lockerNumber map of   
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
        Just (state, code) -> if state /= Taken   
                                then Right code  
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

{-
ghci> lockerLookup 101 lockers  
Right "JAH3I"  
ghci> lockerLookup 100 lockers  
Left "Locker 100 is already taken!"  
ghci> lockerLookup 102 lockers  
Left "Locker number 102 doesn't exist!"  
ghci> lockerLookup 110 lockers  
Left "Locker 110 is already taken!"  
ghci> lockerLookup 105 lockers  
Right "QOTSA" 
-}


{-
            Recursive Data Structures
-}

-- How the standard libary would define a list (sort of)
--data List a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)

{-
ghci> Empty  
Empty  
ghci> 5 `Cons` Empty  
Cons 5 Empty  
ghci> 4 `Cons` (5 `Cons` Empty)  
Cons 4 (Cons 5 Empty)  
ghci> 3 `Cons` (4 `Cons` (5 `Cons` Empty))  
Cons 3 (Cons 4 (Cons 5 Empty)) 
-}

{-
Another way of representing lists but instead of the format x:(y:[]) it is in x:-:(y:-:[])

infixr 5 :-:  
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

ghci> 3 :-: 4 :-: 5 :-: Empty  
(:-:) 3 ((:-:) 4 ((:-:) 5 Empty))  
ghci> let a = 3 :-: 4 :-: 5 :-: Empty  
ghci> 100 :-: a  
(:-:) 100 ((:-:) 3 ((:-:) 4 ((:-:) 5 Empty)))


infixr 5  ++ 
(++) :: [a] -> [a] -> [a]  
[]     ++ ys = ys  
(x:xs) ++ ys = x : (xs ++ ys)

infixr 5  .++  
(.++) :: List a -> List a -> List a   
Empty .++ ys = ys  
(x :-: xs) .++ ys = x :-: (xs .++ ys)

ghci> let a = 3 :-: 4 :-: 5 :-: Empty  
ghci> let b = 6 :-: 7 :-: Empty  
ghci> a .++ b  
(:-:) 3 ((:-:) 4 ((:-:) 5 ((:-:) 6 ((:-:) 7 Empty)))) 
-}

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
singleton' :: a -> Tree a  
singleton' x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton' x
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right

{-
ghci> let nums = [8,6,4,1,7,3,5]  
ghci> let numsTree = foldr treeInsert EmptyTree nums  
ghci> numsTree  
Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))

ghci> 8 `treeElem` numsTree  
True  
ghci> 100 `treeElem` numsTree  
False  
ghci> 1 `treeElem` numsTree  
True  
ghci> 10 `treeElem` numsTree  
False
-}


{-
            Typeclasses 102
-}

{-
This is how the Eq class is defined in the standard Prelude

class Eq a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool  
    x == y = not (x /= y)  
    x /= y = not (x == y) 
-}

data TrafficLight = Red | Yellow | Green
-- Instance of TrafficLight that Derives Eq where everything is defined:
instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False

instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"

{-
ghci> Red == Red  
True  
ghci> Red == Yellow  
False  
ghci> Red `elem` [Red, Yellow, Green]  
True  
ghci> [Red, Yellow, Green]  
[Red light,Yellow light,Green light]  
-}

{-
You can also make typeclasses that are subclasses of other typeclasses.
    The class declaration for Num is a bit long, but here's the first part:

class (Eq a) => Num a where  
   ...

If you wanted to implement a Maybe type, it would have to be done using: (as if not its trying to do Maybe m before Eq x)
instance (Eq m) => Eq (Maybe m) where  
    Just x == Just y = x == y  
    Nothing == Nothing = True  
    _ == _ = False 
-}



{-
           A Yes-No Typeclass
-}

--  Defining return type and input type
class YesNo a where  
    yesno :: a -> Bool

-- Implements numbers so true if any number thats not 0
instance YesNo Int where  
    yesno 0 = False  
    yesno _ = True 

-- Implements list so true is lists is not empty
instance YesNo [a] where  
    yesno [] = False  
    yesno _ = True

-- Takes the parameter and returns the same thing using id (a standard library function)
instance YesNo Bool where  
    yesno = id

-- False if its nothing truish if its something
instance YesNo (Maybe a) where  
    yesno (Just _) = True  
    yesno Nothing = False

-- Returns true on non-empty Tree's
instance YesNo (Tree a) where  
    yesno EmptyTree = False  
    yesno _ = True

-- Returns False if its Red, True otherwise
instance YesNo TrafficLight where  
    yesno Red = False  
    yesno _ = True

{-
ghci> yesno $ length []  
False  
ghci> yesno "haha"  
True  
ghci> yesno ""  
False  
ghci> yesno $ Just 0  
True  
ghci> yesno True  
True  
ghci> yesno EmptyTree  
False  
ghci> yesno []  
False  
ghci> yesno [0,0,0]  
True  
ghci> :t yesno  
yesno :: (YesNo a) => a -> Bool 
-}

-- Mimics if using YesNo instances
yesnoIf :: (YesNo y) => y -> a -> a -> a  
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

{-
ghci> yesnoIf [] "YEAH!" "NO!"  
"NO!"  
ghci> yesnoIf [2,3,4] "YEAH!" "NO!"  
"YEAH!"  
ghci> yesnoIf True "YEAH!" "NO!"  
"YEAH!"  
ghci> yesnoIf (Just 500) "YEAH!" "NO!"  
"YEAH!"  
ghci> yesnoIf Nothing "YEAH!" "NO!"  
"NO!"  
-}



{-
           The Functor Typeclass
-}


-- map is just a fmap that works only on lists. Here's how the list is an instance of the Functor typeclass.
-- How the Functor class is implemented:
--class Functor f where  
--    fmap :: (a -> b) -> f a -> f b

-- map is just a fmap that works only on lists. Here's how the list is an instance of the Functor typeclass.
--instance Functor [] where
--    map :: (a -> b) -> [a] -> [b]
--    fmap = map

{-
map :: (a -> b) -> [a] -> [b]  
ghci> fmap (*2) [1..3]  
[2,4,6]  
ghci> map (*2) [1..3]  
[2,4,6]
-}

-- In this case Maybe is a functor and we are passing (a -> b) -> Maybe a -> Maybe b instead of
    -- (a -> b) -> Maybe m a -> Maybe m b
--instance Functor Maybe where  
--    fmap f (Just x) = Just (f x)  
--    fmap f Nothing = Nothing

{-
ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")  
Just "Something serious. HEY GUYS IM INSIDE THE JUST"  
ghci> fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing  
Nothing  
ghci> fmap (*2) (Just 200)  
Just 400  
ghci> fmap (*2) Nothing  
Nothing 
-}

-- It maps the tree by using recursion and mapping each element on the right or left
instance Functor Tree where  
    fmap f EmptyTree = EmptyTree  
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)
{-
ghci> fmap (*2) EmptyTree  
EmptyTree  
ghci> fmap (*4) (foldr treeInsert EmptyTree [5,7,3,2,1,7])  
Node 28 (Node 4 EmptyTree (Node 8 EmptyTree (Node 12 EmptyTree (Node 20 EmptyTree EmptyTree)))) EmptyTree
-}

-- The type for Either would be (b -> c) -> Either a b -> Either a c so it Could be a used to map a tree but you'd have to pass a value with it
--instance Functor (Either a) where  
--    fmap f (Right x) = Right (f x)  
--    fmap f (Left x) = Left x

{-
This is due to Either being defined as:
data Either a b = Left a | Right b  
-}



{-
           Kinds and some type-foo
-}

{-
ghci> :k Int  
Int :: *

ghci> :k Maybe  
Maybe :: * -> *

ghci> :k Maybe Int  
Maybe Int :: *

ghci> :k Either  
Either :: * -> * -> *

ghci> :k Either String  
Either String :: * -> *  
ghci> :k Either String Int  
Either String Int :: *  
-}

-- When we wanted to make Either a part of the Functor typeclass,
    --we had to partially apply it because Functor wants types that take only one parameter while Either takes two.

{-
Standard Definition of the Function typeclass:

class Functor f where   
    fmap :: (a -> b) -> f a -> f b
-}

class Tofu t where  
    tofu :: j a -> t a j

-- Example of: * -> (* -> *) -> *
data Frank a b  = Frank {frankField :: b a} deriving (Show) 

{-
ghci> :t Frank {frankField = Just "HAHA"}  
Frank {frankField = Just "HAHA"} :: Frank [Char] Maybe  
ghci> :t Frank {frankField = Node 'a' EmptyTree EmptyTree}  
Frank {frankField = Node 'a' EmptyTree EmptyTree} :: Frank Char Tree  
ghci> :t Frank {frankField = "YES"}  
Frank {frankField = "YES"} :: Frank Char []  
-}

-- As Frank has a Type of form a b, frank can be made an instance of Tofu and returns a reverse of the input
    -- Parameters
instance Tofu Frank where  
    tofu x = Frank x

{-
ghci> tofu (Just 'a') :: Frank Char Maybe  
Frank {frankField = Just 'a'}  
ghci> tofu ["HELLO"] :: Frank [Char] []  
Frank {frankField = ["HELLO"]}
-}

-- Type: something -> something -> something -> *
data Barry t k p = Barry { yabba :: p, dabba :: t k }
{-
ghci> :k Barry  
Barry :: (* -> *) -> * -> * -> *  
-}

-- This functor maps f over the first field and appends the the last element to the end 
instance Functor (Barry a b) where  
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}