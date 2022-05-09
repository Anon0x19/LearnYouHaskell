module Chapter11 where
import Data.List
import Data.Char
import Control.Monad.Instances
import Control.Applicative
import qualified Control.Applicative as Control
import Data.Monoid
import qualified GHC.Tuple as True
import qualified Data.Foldable as F


{-
            Functors, Applicative Functors and Monoids
-}

--                          Functors redux

{-
fmap has a type:
fmap :: (a -> b) -> f a -> f b

Functors have a kind of * -> *
-}


{-
- maps over an I/O action using fmap but then returns the result of the I/O
    action

instance Functor IO where  
    fmap f action = do  
        result <- action  
        return (f result)
-}

{-
-- You can play around with the Functor IO and eventually get an operation which
    prints the original line, and prints the reversed line

- So you'd go from:

main = do line <- getLine   
          let line' = reverse line  
          putStrLn $ "You said " ++ line' ++ " backwards!"  
          putStrLn $ "Yes, you really said" ++ line' ++ " backwards!" 

- To (by implementing fmap):
main = do line <- fmap reverse getLine  
          putStrLn $ "You said " ++ line ++ " backwards!"  
          putStrLn $ "Yes, you really said" ++ line ++ " backwards!"

-- The I/O action fmap (++"!") getLine behaves just like getLine, only that it
    appends an ! at the end
-}

main :: IO ()
main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine  
          putStrLn line

{-
$ runhaskell fmapping_io.hs  
hello there  
E-R-E-H-T- -O-L-L-E-H 
-}

{-
instance Functor ((->) r) where         -- if the Syntax allows for it, it could have been defined by `instance Functor (r ->) where`
    fmap f g = (\x -> f (g x))

- Another way to write the previous definition is by:
instance Functor ((->) r) where  
    fmap = (.)

- fmap implementation for Maybe
instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing 
-}

{-
ghci> :t fmap (*3) (+100)  
fmap (*3) (+100) :: (Num a) => a -> a  
ghci> fmap (*3) (+100) 1  
303  
ghci> (*3) `fmap` (+100) $ 1  
303  
ghci> (*3) . (+100) $ 1  
303  
ghci> fmap (show . (*3)) (*100) 1  
"300" 
-}

{-
ghci> :t fmap (*2)  
fmap (*2) :: (Num a, Functor f) => f a -> f a  
ghci> :t fmap (replicate 3)  
fmap (replicate 3) :: (Functor f) => f a -> f [a]  
-}

{-
ghci> fmap (replicate 3) [1,2,3,4]  
[[1,1,1],[2,2,2],[3,3,3],[4,4,4]]  
ghci> fmap (replicate 3) (Just 4)  
Just [4,4,4]  
ghci> fmap (replicate 3) (Right "blah")  
Right ["blah","blah","blah"]  
ghci> fmap (replicate 3) Nothing  
Nothing  
ghci> fmap (replicate 3) (Left "foo")  
Left "foo"  
-}

{-
ghci> fmap id (Just 3)  
Just 3  
ghci> id (Just 3)  
Just 3  
ghci> fmap id [1..5]  
[1,2,3,4,5]  
ghci> id [1..5]  
[1,2,3,4,5]  
ghci> fmap id []  
[]  
ghci> fmap id Nothing  
Nothing 
-}

data CMaybe a = CNothing | CJust Int a deriving (Show)

{-
ghci> CNothing  
CNothing  
ghci> CJust 0 "haha"  
CJust 0 "haha"  
ghci> :t CNothing  
CNothing :: CMaybe a  
ghci> :t CJust 0 "haha"  
CJust 0 "haha" :: CMaybe [Char]  
ghci> CJust 100 [1,2,3]  
CJust 100 [1,2,3]  
-}

{-
- If we use the CNothing constructor, there are no fields, and if we use the CJust constructor,
    the first field is an integer and the second field can be any type.
- By making it an instance of Functor, 

instance Functor CMaybe where  
    fmap f CNothing = CNothing  
    fmap f (CJust counter x) = CJust (counter+1) (f x)
-}

{-
ghci> fmap (++"ha") (CJust 0 "ho")  
CJust 1 "hoha"  
ghci> fmap (++"he") (fmap (++"ha") (CJust 0 "ho"))  
CJust 2 "hohahe"  
ghci> fmap (++"blah") CNothing  
CNothing 
-}

{-
We know that the first functor law states that if we map id over a functor,
    it should be the same as just calling id with the same functor,
    but as we've seen from this example, this is not true for our CMaybe functor.

ghci> fmap id (CJust 0 "haha")  
CJust 1 "haha"  
ghci> id (CJust 0 "haha")  
CJust 0 "haha"  
-}




{-
            Applicative Functors
-}

-- Control.Applicative is used for Applicative typeclasses

{-
ghci> :t fmap (++) (Just "hey")  
fmap (++) (Just "hey") :: Maybe ([Char] -> [Char])  
ghci> :t fmap compare (Just 'a')  
fmap compare (Just 'a') :: Maybe (Char -> Ordering)  
ghci> :t fmap compare "A LIST OF CHARS"  
fmap compare "A LIST OF CHARS" :: [Char -> Ordering]  
ghci> :t fmap (\x y z -> x + y / z) [3,4,5,6]  
fmap (\x y z -> x + y / z) [3,4,5,6] :: (Fractional a) => [a -> a -> a]
-}

{-
ghci> let a = fmap (*) [1,2,3,4]  
ghci> :t a  
a :: [Integer -> Integer]  
ghci> fmap (\f -> f 9) a  
[9,18,27,36]
-}

{-
The following is the Applicative typeclass, it lies in Control.Applicative module and it defines
    the <*> and pure method.

The class tells us that if we wanty to make a type contructor part of the Applicative typeclass,
    it has to be in Functor first.

class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b 
-}

-- The <*> Function has a similar typeclass definition as that of fmap

{-
Applicative instance, implementation for Maybe

- P.S its written (Maybe where) instead of ((Maybe a) where) as you are casting the Maybe to the where clause

instance Applicative Maybe where  
    pure = Just  
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something 
-}

{-
ghci> Just (+3) <*> Just 9  
Just 12  
ghci> pure (+3) <*> Just 10  
Just 13  
ghci> pure (+3) <*> Just 9  
Just 12  
ghci> Just (++"hahah") <*> Nothing  
Nothing  
ghci> Nothing <*> Just "woot"  
Nothing 
-}

{-
If the function is going to pass Nothing, then it'll return nothing, even though you append something
-}

{-
ghci> pure (+) <*> Just 3 <*> Just 5  
Just 8  
ghci> pure (+) <*> Just 3 <*> Nothing  
Nothing  
ghci> pure (+) <*> Nothing <*> Just 5  
Nothing  
-}

{-
ghci> pure (+) <*> Just 3 <*> Just 5  
Just 8  
ghci> pure (+) <*> Just 3 <*> Nothing  
Nothing  
ghci> pure (+) <*> Nothing <*> Just 5  
Nothing


-- <*> kind of works like a logical and for different operators, you are only able to apply the operator
    -- when neither values is equal to Nothing
-}

{-
<$> works like fmap but its imlemented as an infix operator

(<$>) :: (Functor f) => (a -> b) -> f a -> f b  
f <$> x = fmap f x 
-}

{-
Examples:

ghci> (++) <$> Just "johntra" <*> Just "volta"  
Just "johntravolta"
ghci> (++) "johntra" "volta"  
"johntravolta" 
-}

{-
Funilly enough, list constructors are applicative functors 

instance Applicative [] where  
    pure x = [x]  
    fs <*> xs = [f x | f <- fs, x <- xs]  
-}

{-
ghci> pure "Hey" :: [String]  
["Hey"]  
ghci> pure "Hey" :: Maybe String  
Just "Hey" 
-}

{-
ghci> [(*0),(+100),(^2)] <*> [1,2,3]  
[0,0,0,101,102,103,1,4,9] 

ghci> [(+),(*)] <*> [1,2] <*> [3,4]  
[4,5,5,6,3,4,6,8]  

ghci> (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]  
["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]

ghci> (*) <$> [2,5,10] <*> [8,10,11]  
[16,20,22,40,50,55,80,100,110]


- Works the same as the 2 above, but filters out any that are smaller than 50
ghci> filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]  
[55,80,100,110]  
-}

{-
- Pure is all about returning a value in a minimal context that still holds it as a result.
- <*> can be defined by :: IO (a -> b) -> IO a -> IO b. it takes an I/O action that yields
    - a value as its result, but it doesn't do any I/O operations.
- With Maybe and [], we can think of <*> as simply extracting a function
    from its left parameter and then sort of applying over the right one

instance Applicative IO where  
    pure = return  
    a <*> b = do  
        f <- a  
        x <- b  
        return (f x)  
-}


{-
This is an I/O action that will ask the user for 2 lines in the form of input, and it
    concatenates both and returns the result of the concatenation.
-}
myAction :: IO String  
myAction = do  
    a <- getLine  
    b <- getLine  
    return $ a ++ b

-- Same function but represented in 2 lines of code, 1 being datatype definiton and the
    -- Other being the function definition
myAction' :: IO String  
myAction' = (++) <$> getLine <*> getLine 

-- Similar code but re-written to use variables and pattern matching
main' :: IO ()
main' = do  
    a <- (++) <$> getLine <*> getLine  
    putStrLn $ "The two lines concatenated turn out to be: " ++ a

{-
(->) r is a functor. Functors aren't a collection of functions directly.
- They're two mappings, the object and arrow mappings Ob and ar.
1. The object mapping Ob t = r -> t takes objects of Hask (types) to
    objects of Hask again (function types from r to that type)
2. The arrow mapping has to take morphisms in Hask to morphisms in r -> Hask,
    in other words it takes Hask morphisms (a ---> b) to (r -> Hask) morphisms ((r -> a) ---> (r -> b)).

instance Applicative ((->) r) where  
    pure x = (\_ -> x)  
    f <*> g = \x -> f x (g x)  
-}


{-
As pure is definied as pure :: a -> (r -> a), passing variables casted outside of pure will just ignore them i.e:

ghci> (pure 3) "blah"  
3 
-}

{-
The instance implementation for <*> is a bit cryptic. So lets have some more examples
    of how it works

ghci> :t (+) <$> (+3) <*> (*100)  
(+) <$> (+3) <*> (*100) :: (Num a) => a -> a  
ghci> (+) <$> (+3) <*> (*100) $ 5  
508

ghci> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5  
[8.0,10.0,2.5] 
-}




{-
The typeclass ZipList a, because of how it works, the resulting list will be as long as
    the shorter of the 2 lists passed as parameters.
- P.S Works using recursion

instance Applicative ZipList where  
        pure x = ZipList (repeat x)  
        ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs) 
-}


{-
As ZipList type doesn't have a show instance, we use the getZipList function to extract
    a raw list out of a zip list.

ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]  
[101,102,103]  
ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]  
[101,102,103]  
ghci> getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]  
[5,3,3,4]  
ghci> getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"  
[('d','c','r'),('o','a','a'),('g','t','t')] 
-}


-- P.S The (,,) function is the same as \x y z -> (x,y,z). Also, the (,) function is the same as \x y -> (x,y)


{-
Control.Applicative defines a function that's called liftA2, which has a type:

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
liftA2 f a b = f <$> a <*> b

The reason we're looking at it is because it clearly showcases why applicative functors are more powerful than just ordinary functors.


It's also interesting to look at this function's type as (a -> b -> c) -> (f a -> f b -> f c).
    When we look at it like this, we can say that liftA2 takes a normal binary function and promotes
    it to a function that operates on two functors.
-}

{-
This function is an example of how to parse a type from Just int to [int]

ghci> fmap (\x -> [x]) (Just 4)  
Just [4]

ghci> liftA2 (:) (Just 3) (Just [4])  
Just [3,4]  
ghci> (:) <$> Just 3 <*> Just [4]  
Just [3,4]
-}


{-
This function takes a list of applicatives and returns an applicative that has a list
    as its result value.

sequenceA :: (Applicative f) => [f a] -> f [a]  
sequenceA [] = pure []  
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

- Another way of implementing the same functionallity using different code.
sequenceA :: (Applicative f) => [f a] -> f [a]  
sequenceA = foldr (liftA2 (:)) (pure [])


- Creates a Maybe value with all results inside and if 1 is Nothing, returns Nothing, else
    returns the list

ghci> sequenceA [Just 3, Just 2, Just 1]  
Just [3,2,1]  
ghci> sequenceA [Just 3, Nothing, Just 1]  
Nothing  
ghci> sequenceA [(+3),(+2),(+1)] 3  
[6,5,4]  
ghci> sequenceA [[1,2,3],[4,5,6]]  
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]  
ghci> sequenceA [[1,2,3],[4,5,6],[3,4,4],[]]  
[] 
-}


{-
-- Way to check whether a number is satisfied by all the predicates in a list.
ghci> map (\f -> f 7) [(>4),(<10),odd]  
[True,True,True]  
ghci> and $ map (\f -> f 7) [(>4),(<10),odd]  
True  


-- By using and, it performes a boolean operation over the list, returns True if they are
    all true, False otherwise.
ghci> sequenceA [(>4),(<10),odd] 7  
[True,True,True]  
ghci> and $ sequenceA [(>4),(<10),odd] 7  
True 
-}


{- Examples of using sequenceA

ghci> sequenceA [[1,2,3],[4,5,6]]  
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]  
ghci> [[x,y] | x <- [1,2,3], y <- [4,5,6]]  
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]  
ghci> sequenceA [[1,2],[3,4]]  
[[1,3],[1,4],[2,3],[2,4]]  
ghci> [[x,y] | x <- [1,2], y <- [3,4]]  
[[1,3],[1,4],[2,3],[2,4]]  
ghci> sequenceA [[1,2],[3,4],[5,6]]  
[[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]  
ghci> [[x,y,z] | x <- [1,2], y <- [3,4], z <- [5,6]]  
[[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]] 
-}

{- Example of using sequenceA with getLine

ghci> sequenceA [getLine, getLine, getLine]  
heyh  
ho  
woo  
["heyh","ho","woo"]  
-}

{- Functor laws:

- pure f <*> x = fmap f x
- pure id <*> v = v
- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
- pure f <*> pure x = pure (f x)
- u <*> pure y = pure ($ y) <*> u
-}




{-
            The newtype keyword
-}


{-
Performes the first list of operators, etc on the second list by using 

ghci> [(+1),(*100),(*5)] <*> [1,2,3]  
[2,3,4,100,200,300,5,10,15] 
-}

{-
By combining ZipList and getZipList we are able to wrap and unwrap the field

ghci> getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3]  
[2,200,15] 
-}

{- ZipList a : being defined

data ZipList a = ZipList [a]

By applying record syntax, we are able to automatically get a function that extracts a list from ZipList.

data ZipList a = ZipList { getZipList :: [a] }
-}


{-
The newtype keyword in Haskell is made exactly for these cases when we want to just take one type and wrap it in something to present it as another type.

newtype ZipList a = ZipList { getZipList :: [a] }  
-}

{-
When using the newtype keyword, you can only have one value constructor and that value constructor can only have one field.

- By using the data constructor, we are able to use more than 1 field for the constructor
    of the datatype

data Profession = Fighter | Archer | Accountant  
  
data Race = Human | Elf | Orc | Goblin  
  
data PlayerCharacter = PlayerCharacter Race Profession 
-}

{-
We can use the deriving keyword with newtype just like with can with data, you can derive
    multiple classes. It makes sense as newtype just wraps an existing type so by adding
    the derivation, we are able to equate, print or mess around with our value/newtype

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

i.e:
ghci> CharList "this will be shown!"  
CharList {getCharList = "this will be shown!"}  
ghci> CharList "benny" == CharList "benny"  
True  
ghci> CharList "benny" == CharList "oisters"  
False  

- CharList :: [Char] -> CharList
- getCharList :: CharList -> [Char]  
-}

{- Using newtype to make type class instances:

class Functor f where  
    fmap :: (a -> b) -> f a -> f b

the fmap definition is basically fmap :: (a -> b) -> Maybe a -> Maybe b  as Functor F is:
instance Functor Maybe where, followed by the fmap implementation
-}

{- To be able to use the Pair type for the functor, we firstly have to define it as a newtype
    Then we can make it into an instance of Funcotr so that the function is mapped over the first component.

newtype Pair b a = Pair { getPair :: (a,b) }

instance Functor (Pair c) where  
    fmap f (Pair (x,y)) = Pair (f x, y) 

As can be seen, here we are pattern matching the types defined in newtype Pair and the
    types within the Functor. When defined here, fmap takes the type:
    fmap :: (a -> b) -> Pair c a -> Pair c b  as it would only work on new pairs. But instead
    of f, we use Pair C so we can pass another value and it can be mapped over

ghci> getPair $ fmap (*100) (Pair (2,3))  
(200,3)  
ghci> getPair $ fmap reverse (Pair ("london calling", 3))  
("gnillac nodnol",3)
-}


{- On newtype laziness

The undefined value in Haskell represents an erronous computation thats why when printing
undefinied, it will return an Exception.

I.e:
ghci> undefined  
*** Exception: Prelude.undefined 
ghci> head [3,4,5,undefined,2,undefined]  
3 
-}

data CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String  
helloMe (CoolBool _) = "hello"

{- When running it with values
ghci> helloMe undefined  
"*** Exception: Prelude.undefined

but by replacing `data CoolBool = CoolBool { getCoolBool :: Bool }` with:
    newtype CoolBool = CoolBool { getCoolBool :: Bool }

I/O Operation:
ghci> helloMe undefined  
"hello" 
-}




{-
            type vs newtype vs data
-}

{- The type keyword is for making type synonyms. What that means is that we just give
    another name to an already existing type so that the type is easier to refer to.

type IntList = [Int]

ghci> ([1,2,3] :: IntList) ++ ([1,2,3] :: [Int])  
[1,2,3,1,2,3]

(I.e its similar with the type synonym that was given to PhoneBook [(String, String)] )
-}

{- The newtype keyword is for taking existing types and wrapping them in new types,
    mostly so that it's easier to make them instances of certain type classes.

newtype CharList = CharList { getCharList :: [Char] }
-}




{-
            Monoids
-}


{-
ghci> (3 * 2) * (8 * 5)  
240  
ghci> 3 * (2 * (8 * 5))  
240  
ghci> "la" ++ ("di" ++ "da")  
"ladida"  
ghci> ("la" ++ "di") ++ "da"  
"ladida" 
-}


{-
- The Monoid type class is defined in improt Data.Monoid

First of all, we see that only concrete types can be made instances of Monoid. (This is due to m)
This is different from Functor and Applicative, which require their instances to be
    type constructors which take one parameter.

The first function is mempty. It's not really a function, since it doesn't take parameters,
    so it's a polymorphic constant, kind of like minBound from Bounded. mempty represents
    the identity value for a particular monoid.

Next up, we have mappend, which, as you've probably guessed, is the binary function. It takes
    two values of the same type and returns a value of that type as well.
It's worth noting that the decision to name mappend as it's named was kind of unfortunate,
    because it implies that we're appending two things in some way. While ++ does take two lists
    and append one to the other, * doesn't really do any appending, it just multiplies two numbers together. 

The last function in this type class definition is mconcat. It takes a list of monoid
    values and reduces them to a single value by doing mappend between the list's elements.
It has a default implementation, which just takes mempty as a starting value and folds
    the list from the right with mappend.

class Monoid m where  
    mempty :: m  
    mappend :: m -> m -> m  
    mconcat :: [m] -> m  
    mconcat = foldr mappend mempty  
-}

{- Monoid Laws:

We mentioned that there has to be a value that acts as the identity with respect to the
    binary function and that the binary function has to be associative.
It's possible to make instances of Monoid that don't follow these rules, but such
    instances are of no use to anyone because when using the Monoid type class,
    we rely on its instances acting like monoids.

- mempty `mappend` x = x
- x `mappend` mempty = x
- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
-}



{-
            Lists are monoids
-}

{-
instance Monoid [a] where  
    mempty = []  
    mappend = (++)

Lists are an instance of the Monoid type class regardless of the type of the elements they hold.

ghci> [1,2,3] `mappend` [4,5,6]  
[1,2,3,4,5,6]  
ghci> ("one" `mappend` "two") `mappend` "tree"  
"onetwotree"  
ghci> "one" `mappend` ("two" `mappend` "tree")  
"onetwotree"  
ghci> "one" `mappend` "two" `mappend` "tree"  
"onetwotree"  
ghci> "pang" `mappend` mempty  
"pang"  
ghci> mconcat [[1,2],[3,6],[9]]  
[1,2,3,6,9]  
ghci> mempty :: [a]  
[]

ghci> "one" `mappend` "two"  
"onetwo"  
ghci> "two" `mappend` "one"  
"twoone"
-}




{-
            Product and Sum
-}

{-
One way for numbers to be considered monoids is to have the binrary fuction to be */+ and
    the identity value to be 0/1

ghci> 0 + 4  
4  
ghci> 5 + 0  
5  
ghci> (1 + 3) + 5  
9  
ghci> 1 + (3 + 5)  
9

The monoid laws hold for these examples as if you add 0 to any number, you return that number
    and addition is also associative.
-}

{-
The Data.Monoid module exports two types for this, these are Product and Sum which can
    be defined by:

newtype Product a =  Product { getProduct :: a }  
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where  
    mempty = Product 1  
    Product x `mappend` Product y = Product (x * y)

mempty is just 1 wrapped in a Product constructor, mappend pattern matches on the Product
    constructor, multiples the two numbers and then wraps the resulting number back.

Due to there being a Num a class constraint. This means that Product a is an instance of
    Monoid for all a's that are alreayd an instance of Num.

To use Product a as a monoid you need to do some newtype wrapping and unrapping as following:
ghci> getProduct $ Product 3 `mappend` Product 9  
27  
ghci> getProduct $ Product 3 `mappend` mempty  
3  
ghci> getProduct $ Product 3 `mappend` Product 4 `mappend` Product 2  
24  
ghci> getProduct . mconcat . map Product $ [3,4,2]  
24

Sum is defined like Product and therefore the instance is done in a similar method:
ghci> getSum $ Sum 2 `mappend` Sum 9  
11  
ghci> getSum $ mempty `mappend` Sum 3  
3  
ghci> getSum . mconcat . map Sum $ [1,2,3]  
6  
-}



{-
Any and All
-}

{-
The type Bool can also act like a monoid in two distinct but equally valid ways. One way
    is to have the or function (||) act as the binrary function along with False as the
    identity value. So it returns False if or'ed with False and True if or'ed with True
-}

{-
The Any newtype constructor is an instance of Monoid and its defined by:

newtype Any = Any { getAny :: Bool }  
    deriving (Eq, Ord, Read, Show, Bounded)

- The instance goes as the following:
instance Monoid Any where  
        mempty = Any False  
        Any x `mappend` Any y = Any (x || y)

- The reason its called Any is bacause x `mappend` y will be True if any one of those two
    is True. Even if three or more Any wrapped Bool's mappend'ed together, The result will
    be True is any of those are True.

ghci> getAny $ Any True `mappend` Any False  
True  
ghci> getAny $ mempty `mappend` Any True  
True  
ghci> getAny . mconcat . map Any $ [False, False, False, True]  
True  
ghci> getAny $ mempty `mappend` mempty  
False  
-}



{-
The other way for Bool to be an instancw of Monoid is being using && and looking for the
    opposite pattern, return True when False and False when True.

newtype All = All { getAll :: Bool }  
        deriving (Eq, Ord, Read, Show, Bounded)

- And the insstance if defined by the following:
instance Monoid All where  
        mempty = All True  
        All x `mappend` All y = All (x && y)

- When we mappend values of the All type, the result will be True only if all the values 
    used in the mappend operations are True:

ghci> getAll $ mempty `mappend` All True  
True  
ghci> getAll $ mempty `mappend` All False  
False  
ghci> getAll . mconcat . map All $ [True, True, True]  
True  
ghci> getAll . mconcat . map All $ [True, True, False]  
False 
-}



{-
            The Ordering Monoid
-}

{-
ghci> 1 `compare` 2  
LT  
ghci> 2 `compare` 2  
EQ  
ghci> 3 `compare` 2  
GT

- The instance is defined by the following:
instance Monoid Ordering where  
    mempty = EQ  
    LT `mappend` _ = LT  
    EQ `mappend` y = y  
    GT `mappend` _ = GT

The instance is set up to first mappened two Ordering values, the one on the left is kept
    unless they are both EQ in which case, the right is the result, so you just try to see
    what cases the right one is bigger than the one on the left

- Its important to note that in the Monoid instance for Ordering, x `mappend` y is not the
    same as y `mappend` x because the first parameter is kept.
ghci> LT `mappend` GT  
LT  
ghci> GT `mappend` LT  
GT  
ghci> mempty `mappend` LT  
LT  
ghci> mempty `mappend` GT  
GT

-}
--Example of how this monoid is useful, as it returns an ordering:
lengthCompare :: String -> String -> Ordering  
lengthCompare x y = let a = length x `compare` length y   
                        b = x `compare` y  
                    in  if a == EQ then b else a

-- By employing monoids, this function can be rewritten to lengthCompare :: String -> String -> Ordering  
lengthCompare' :: String -> String -> Ordering
lengthCompare' x y = (length x `compare` length y) `mappend`  
                    (x `compare` y)

lengthCompare'' :: String -> String -> Ordering  
lengthCompare'' x y = (length x `compare` length y) `mappend`  
                    (vowels x `compare` vowels y) `mappend`  
                    (x `compare` y)  
    where vowels = length . filter (`elem` "aeiou")  

{-
ghci> lengthCompare "zen" "ants"  
LT  
ghci> lengthCompare "zen" "ant"  
GT

ghci> lengthCompare'' "zen" "anna"  
LT  
ghci> lengthCompare'' "zen" "ana"  
LT  
ghci> lengthCompare'' "zen" "ann"  
GT  
-}




{-
            Maybe the monoid
-}

{-
One way is to treat Maybe a as a monoid only if its type parameter a is a monoid as well
    and then implement mappend in such a way that it uses the mappend operation of the
    values that are wrapped with Just.

We use Nothing as the identity, and so if one of the two values that we're mappending is
    Nothing, we keep the other value.

instance Monoid a => Monoid (Maybe a) where  
    mempty = Nothing  
    Nothing `mappend` m = m  
    m `mappend` Nothing = m  
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

- Maybe a is an instance of Monoid only if a is an instance of Monoid.

- We are able to see if the results of the computations that may have failed as we can see
    if the return is Just or Nothing, if its Nothing, it failed otherwise it passed.
ghci> Nothing `mappend` Just "andy"  
Just "andy"  
ghci> Just LT `mappend` Nothing  
Just LT  
ghci> Just (Sum 3) `mappend` Just (Sum 4)  
Just (Sum {getSum = 7})



- And for when the type of the contents of Maybe isn't a Monoid, we can't use mappend
    so we use First a. We can only use mappend when both values are Just values.


- First a definition:
newtype First a = First { getFirst :: Maybe a }  
    deriving (Eq, Ord, Read, Show)


- Instance of Maybe a wrapped in a newtype:
instance Monoid (First a) where  
    mempty = First Nothing  
    First (Just x) `mappend` _ = First (Just x)  
    First Nothing `mappend` x = x

- mempty is still just a Nothing wrapped with the First newtype constructor. And if mappends
    first parameter is a Just value, we ignore the second one, if its Nothing, then we
    return the second one as the result.


ghci> getFirst $ First (Just 'a') `mappend` First (Just 'b')  
Just 'a'  
ghci> getFirst $ First Nothing `mappend` First (Just 'b')  
Just 'b'  
ghci> getFirst $ First (Just 'a') `mappend` First Nothing  
Just 'a'

ghci> getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]  
Just 9
-}

{-
In Data.Monoid there exists Last a, which in similar fashion to First a, takes the last
    non-Nothing value when mappend'ing and using mconcat.

ghci> getLast . mconcat . map Last $ [Nothing, Just 9, Just 10]  
Just 10  
ghci> getLast $ Last (Just "one") `mappend` Last (Just "two")  
Just "two"
-}




{-
            Using monoids to fold data structures
-}

{-
Because there are so many data structures that work nicely with folds, the Foldable type
    class was introduced. Much like Functor is for things that can be mapped over, Foldable
    is for things that can be folded up.

import qualified Foldable as F 
-}

{-
ghci> :t foldr  
foldr :: (a -> b -> b) -> b -> [a] -> b  
ghci> :t F.foldr  
F.foldr :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b

- So whereas foldr takes a list and folds it up, the foldr from Data.Foldable accepts
    any type that can be folded up, not just lists! As expected, both foldr functions
    do the same for lists:

ghci> F.foldl (+) 2 (Just 9)  
11  
ghci> F.foldr (||) False (Just True)  
True  
-}

{-
Using the Tree definition from Chapter 8

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
-}

{-
A tree is either an empty tree that doesn't hold any values or it's a node
    that holds one value and also two other trees. After defining it, it's an instance
    of Functor and with that we can now fmap functions over it. 
    
One way to make a type constructor an instance of Foldable is to just directly implement
    foldr for it. But another, often much easier way, is to implement the foldMap
    function, which is also a part of the Foldable type class. The foldMap function has
    the following type:
    foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m 

- The parameter is a function that takes a value of the type that our foldable
    structure contains (denoted previously by an a) and returns a monoid value.
- Once it maps that function over the foldable structure, thus producing a foldable
    structure that contains monoid values. Then, by doing mappend between those monoid
    values, it joins them all into a single monoid value.

-- By implementing foldMap for some type, we get foldr and foldl on that type.

To make Tree an instance of Foldable:
instance F.Foldable Tree where  
    foldMap f Empty = mempty  
    foldMap f (Node x l r) = F.foldMap f l `mappend`  
                             f x           `mappend`  
                             F.foldMap f r 

In this instance, it keeps maping the left and right sub-trees to essentially find how
    long the tree is. We then mappend all the values (naturall the left sub-tree comes first,
    then the node value and then thr right sub-tree).

-- Note: foldMap parameter is returned as a monoid value
-}

{- I.e using the specific tree and examples:

testTree = Node 5  
            (Node 3  
                (Node 1 Empty Empty)  
                (Node 6 Empty Empty)  
            )  
            (Node 9  
                (Node 8 Empty Empty)  
                (Node 10 Empty Empty)  
            )


- When applying F.foldl/r
ghci> F.foldl (+) 0 testTree  
42  
ghci> F.foldl (*) 1 testTree  
64800


-- Note that foldMap is not only usefull for making new instances of Foldable, it is also
    handy when reducing a structure to a single monoid value.

ghci> getAny $ F.foldMap (\x -> Any $ x == 3) testTree  
True

- Here, \x -> Any $ x == 3 is a function that takes a number and returns a monoid value
    its a Bool wrapped in Any. foldMap applies this function to every element in our tree and
    reduces the resulting monoids into a single monoid with mappend.
ghci> getAny $ F.foldMap (\x -> Any $ x > 15) testTree  
False


All of the nodes in our tree would hold the value Any False after having the function in
    the lambda applied to them. But to end up True, mappend for Any has to have at least
    sone True value as a parameter.
That's why the final result is False, which makes sense because no value in our tree is
    greater than 15.



-- Note: We can also easily turn our tree into a list by doing a foldMap with the \x -> [x]
    function.
By first projecting that function onto our tree, each element becomes a singleton list.
    The mappend action that takes place between all those singleton list results in a
    single list that holds all of the elements that are in our tree:
ghci> F.foldMap (\x -> [x]) testTree  
[1,3,6,5,8,9,10]  
-}