module Chapter13 where
import Data.List
import Data.Char
import Control.Monad.Instances
import Control.Applicative
import qualified Control.Applicative as Control
import Data.Monoid
import qualified Data.Foldable as F
import Control.Monad
import Control.Monad.Instances
import qualified Control.Monad.State
import System.Random
import Data.Ratio

{-
                For a Few Monads More
-}


{- Writer Monad -}

{-
The write monad is for values that have another value attached that acts as a sort of log
    value. Writer allows us to do computations while making sure that all the log values are
    combined into one log values that then gets attached to the result.
-}

isBigGang :: Int -> Bool  
isBigGang x = x > 9
-- Now instead of True or False, we want to return a string along side the bool
isBigGang' :: Int -> (Bool, String)  
isBigGang' x = (x > 9, "Compared gang size to 9.")
-- isBigGang' at the the minute is taking a normal value and returning a value with context
{-
We've seen how to take a normal string and return a value with context, now how do you
    take a value with context and pase it in a function. For that we have applyLog
-}
applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)  
applyLog (x,log) f =
    let (y,newLog) = f x
    in (y,log ++ newLog)

{-
ghci> (3, "Smallish gang.") `applyLog` isBigGang  
(False,"Smallish gang.Compared gang size to 9")  
ghci> (30, "A freaking platoon.") `applyLog` isBigGang  
(True,"A freaking platoon.Compared gang size to 9")

ghci> ("Tobin","Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))  
(5,"Got outlaw name.Applied length.")  
ghci> ("Bathcat","Got outlaw name.") `applyLog` (\x -> (length x, "Applied length"))  
(7,"Got outlaw name.Applied length")
-}


{- Monoids to the rescue -}
{-
applyLog couldn't be written in the form - applyLog :: (a,[c]) -> (a -> (b,[c])) -> (b,[c])
    this is because if not the list returned should be the same size as the original
-}

{-
ghci> [1,2,3] `mappend` [4,5,6]  
[1,2,3,4,5,6]  
ghci> B.pack [99,104,105] `mappend` B.pack [104,117,97,104,117,97]  
Chunk "chi" (Chunk "huahua" Empty)
-}

-- Now applyLog can work for any monoid as we change from ++ to `mappend`
applyLog' :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)  
applyLog' (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)


type Food = String  
type Price = Sum Int  
  
addDrink :: Food -> (Food,Price)  
addDrink "beans" = ("milk", Sum 25)  
addDrink "jerky" = ("whiskey", Sum 99)  
addDrink _ = ("beer", Sum 30)

{-
ghci> Sum 3 `mappend` Sum 9  
Sum {getSum = 12}

ghci> ("beans", Sum 10) `applyLog` addDrink  
("milk",Sum {getSum = 35})  
ghci> ("jerky", Sum 25) `applyLog` addDrink  
("whiskey",Sum {getSum = 124})  
ghci> ("dogmeat", Sum 5) `applyLog` addDrink  
("beer",Sum {getSum = 35})

ghci> ("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink  
("beer",Sum {getSum = 65})
-}


{- The Writer type -}

{-
the addDrink example shows that a value with a monoid attached acts like a monadic value.
    The Control.Monad.Writer module exports the Writer w a type along with its Monad
    instance and some useful functions for dealing with values of this type.

The Writer w a type is just a newtype wrapper to attach a monoid to a value, this is done
    by putting them in a tuple
-}
-- Defined as
newtype Writer w a = Writer { runWriter :: (a, w) }
{-
Its wrapped in a newtype so that it can be made an instance of Monad and that its type is
    separate from a normal tuple. The a parameter represents the type of the value
    and the w parameter repressents the type of the attached monoid value.

The Monad instance for Writer is defined by:
instance (Monoid w) => Monad (Writer w) where  
    return x = Writer (x, mempty)  
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')
-}

{-
The implmentation of >>= is pretty much the same as applyLog, only difference is that
    but the tuple is wrapped in the Writer newtype. It then needs to be unwrapped
    before being able to pattern match it.
This gives us a Writer w a value and we use a let expression to pattern match on it.
We present y as the new result and use mappend to combine the old monoid value with
    the new one.
The result is then packed up in a tuple and then wrapped with the Writer constructor
    so that our result is a Writer value instead of just an unwrapped tuple.
-}

{-
ghci> runWriter (return 3 :: Writer String Int)  
(3,"")  
ghci> runWriter (return 3 :: Writer (Sum Int) Int)  
(3,Sum {getSum = 0})  
ghci> runWriter (return 3 :: Writer (Product Int) Int)  
(3,Product {getProduct = 1}) 
-}

{-
Because Writer doesn't have a Show instance, we have to use runWriter to convert our
    Writer values to normal tuples that can be shown. For String the monoid value is
    the empty string. With Sum it's 0 because anything plus 0 stays the same. For
    Product, the identity is 1.

The Writer instance doesn't feature an implementation for fail, so if a pattern match
    fails in do notation, error is called.
-}



{- Using do notation with Writer -}

{-
Example of multiplying 2 numbers with the Writer notation using do
-}
{-
import Control.Monad.Writer  
  
logNumber :: Int -> Writer [String] Int  
logNumber x = Writer (x, ["Got number: " ++ show x])  
  
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b) 

ghci> runWriter multWithLog  
(15,["Got number: 3","Got number: 5"])

-- multWithLog but wiht some extra reporting added:
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    tell ["Gonna multiply these two"]  
    return (a*b)

ghci> runWriter multWithLog  
(15,["Got number: 3","Got number: 5","Gonna multiply these two"])

-- Its important that the last line of a do expression returns the whole result of the
    do expression. Had we put tell as the last line, '()' would have been the result.
-}



{- Adding Logging to Programs -}

-- Greatest common divisor
gcd' :: Int -> Int -> Int  
gcd' a b   
    | b == 0    = a  
    | otherwise = gcd' b (a `mod` b)
{-
ghci> gcd' 8 3
1
-}
-- If we want to equip our result with context, and for the context to be a monoid
-- gcd'' :: Int -> Int -> Writer [String] Int  
-- gcd'' a b  
--     | b == 0 = do  
--         tell ["Finished with " ++ show a]  
--         return a  
--     | otherwise = do  
--         tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
--         gcd' b (a `mod` b)

{- I.e
ghci> fst $ runWriter (gcd' 8 3)
1

ghci> mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)  
8 mod 3 = 2  
3 mod 2 = 1  
2 mod 1 = 0  
Finished with 1
-}




{- Inefficient list construction -}

{-
In our gcd' function, the logging is fast because the list appending ends up looking like this:
    a ++ (b ++ (c ++ (d ++ (e ++ f))))

Lists are data structures that are constructed from left to right, we usually want to
    start on the list and slowly append more lists, because we don't want to end up with
    something that looks like ((((a ++ b) ++ c) ++ d) ++ e) ++ f as the Writer monad can
    produce a list that can look like this.
-}

-- The following function is like gcd' but only it logs stuff in reverse. First it
    -- produces
-- gcdReverse :: Int -> Int -> Writer [String] Int  
-- gcdReverse a b  
--     | b == 0 = do  
--         tell ["Finished with " ++ show a]  
--         return a  
--     | otherwise = do  
--         result <- gcdReverse b (a `mod` b)  
--         tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
--         return result

{-
ghci> mapM_ putStrLn $ snd $ runWriter (gcdReverse 8 3)  
Finished with 1  
2 mod 1 = 0  
3 mod 2 = 1  
8 mod 3 = 2
-}



{- Difference List -}

{-
The difference with equalanence of a list [1,2,3] would be the function
    \xs -> [1,2,3] ++ xs
Where as the empty difference list is the function \xs -> [] ++ xs
Defining the difference list as a function can be done by:
    f `append` g = \xs -> f (g xs)
-}

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)  
  
fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []

{-
To make a normal list into a difference list we just do what we did before and make it
    a function that prepends it to another list.
Because a difference list is a function that prepends osmething to another list, if we
    want that something, we apply the function to an empty list.
The Monoid instance:
instance Monoid (DiffList a) where  
    mempty = DiffList (\xs -> [] ++ xs)  
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

Notice how for lists, mempty is just the id function and mappend is actually just function
    composition. Let's see if this works:
ghci> fromDiffList (toDiffList [1,2,3,4] `mappend` toDiffList [1,2,3])  
[1,2,3,4,1,2,3]

-- By implementing difference lists instead of using normal lists, we can increase the
    efficiency of gcdReverse:
gcd' :: Int -> Int -> Writer (DiffList String) Int  
gcd' a b  
    | b == 0 = do  
        tell (toDiffList ["Finished with " ++ show a])  
        return a  
    | otherwise = do  
        result <- gcd' b (a `mod` b)  
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])  
        return result

ghci> mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdReverse 110 34  
Finished with 2  
8 mod 2 = 0  
34 mod 8 = 2  
110 mod 34 = 8
- We do gcdReverse 110 34, then use runWriter to unwrap it from the newtype, then apply
    snd to that to just get the log, then apply fromDiffList to convert it to a normal
    list and then finally print its entries to the screen.
-}



{- Comparing Performance -}

-- Counts from 0 to n and prints all the numbers on the way
-- finalCountDown :: Int -> Writer (DiffList String) ()  
-- finalCountDown 0 = do  
--     tell (toDiffList ["0"])  
-- finalCountDown x = do  
--     finalCountDown (x-1)  
--     tell (toDiffList [show x])

{-
ghci> mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 500000  
0  
1  
2  
...
500000

How ever if we run it without including toDiffList, the counting is very slow
-}





{- Reader -}
addStuff :: Int -> Int  
addStuff = do  
    a <- (*2)  
    b <- (+10)  
    return (a+b)
-- Another way of writting it using the Reader Monad
addStuff' :: Int -> Int  
addStuff' x = let  
    a = (*2) x  
    b = (+10) x  
    in a+b

{-
ghci> addStuff 3  
19 
-}

{- Tasteful stateful computation -}

{-
Haskell has state monads, which make dealing with stetful problems a lot easier while
    still keeping everything nice and pure.
-}
-- Using StdGen we are able to generate a number 3 times based of a single generation

-- threeCoins :: StdGen -> (Bool, Bool, Bool)  
-- threeCoins gen =   
--     let (firstCoin, newGen) = random gen  
--         (secondCoin, newGen') = random newGen  
--         (thirdCoin, newGen'') = random newGen'  
--     in  (firstCoin, secondCoin, thirdCoin)

-- The reason you return a Bool value with the new generator, is to be able to generate
    -- a future number.

{-
We'll say that a stateful computation is a function that takes some state and returns
    a value along with some new state. That function would have the following type:
s -> (a,s)
s is the type of the state
a is the result of the stateful computation


This stateful computation, a function that takes a state and returns a result and a new
    state, can be thought of as a value with a context as well. The actual value is the
    result, whereas the context is that we have to provide some initial state to actually
    get that result and that apart from getting a result we also get a new state.
-}



{- Stacks and stones -}
-- Examples of modeling and operating a stack.
type Stack = [Int]  
  
pop :: Stack -> (Int,Stack)  
pop (x:xs) = (x,xs)  
  
push :: Int -> Stack -> ((),Stack)  
push a xs = ((),a:xs)

stackManip :: Stack -> (Int, Stack)  
stackManip stack = let  
    ((),newStack1) = push 3 stack  
    (a ,newStack2) = pop newStack1  
    in pop newStack2

{-
ghci> stackManip [5,8,2,1]  
(5,[8,2,1])


stackManip = do  
    push 3  
    a <- pop  
    pop  
Well, using the state monad will allow us to do exactly this. With it, we will be able
    to take stateful computations like these and use them without having to manage the
    state manually.
-}



{- The State Monad -}
newtype State s a = State { runState :: s -> (a,s) }
{-
Monad instance for stateful computations

instance Monad (State s) where  
    return x = State $ \s -> (x,s)  
    (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a  
                                    in  g newState

Return has minimal context which means it will make stateful computation that presents a certain
    value as the result and keeps the state unchanged.

The lambda is the new stateful computation.
-}
-- pop' :: State Stack Int  
-- pop' = State $ \(x:xs) -> (x,xs)  
  
-- push' :: Int -> State Stack ()  
-- push' a = State $ \xs -> ((),a:xs)

-- stackManip' :: State Stack Int  
-- stackManip' = do  
--     push' 3  
--     a <- pop'  
--     pop'

{-
ghci> runState stackManip [5,8,2,1]  
(5,[8,2,1])

stackManip :: State Stack Int  
stackManip = do  
    push 3  
    pop  
    pop

stackStuff :: State Stack ()  
stackStuff = do  
    a <- pop  
    if a == 5  
        then push 5  
        else do  
            push 3  
            push 8

ghci> runState stackStuff [9,0,2,1,0]
((),[8,3,0,2,1,0])

- Remember do expressions result in monadic values and with the state monad, a single do
    expression is also a stateful function, because stackManip and stackStuff are ordinary
    stateful computations, we gcan glue them together to produce furhter stateful computations

moreStack :: State Stack ()  
moreStack = do  
    a <- stackManip  
    if a == 100  
        then stackStuff  
        else return ()
-}

{-
The Control.Monad.State module provides a type class that's called MonadState and it features
    two pretty useful functions, namely get and put. For State, the get function is implemented
    like this:

get = State $ \s -> (s,s)

So it just takes the current state and presents it as the result. The put function takes some
    state and makes a stateful function that replaces the current state with it:
put newState = State $ \s -> ((),newState) 


stackyStack :: State Stack ()  
stackyStack = do  
    stackNow <- get  
    if stackNow == [1,2,3]  
        then put [8,3,1]  
        else put [9,2,1]

What would >>= look like if it wokred for State values:
    (>>=) :: State s a -> (a -> State s b) -> State s b
    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
-}



{- Randomness and the state monad -}
{-
random :: (RandomGen g, Random a) => g -> (a, g)


-- It takes a random generator and produces a random number along with a new generator.
import System.Random
import Control.Monad.State
-
randomSt :: (RandomGen g, Random a) => State g a
randomSt = State random
-}

-- The previous three coin example can also be written in do notation using:

-- threeCoins :: State StdGen (Bool,Bool,Bool)  
-- threeCoins = do  
--     a <- randomSt  
--     b <- randomSt  
--     c <- randomSt  
--     return (a,b,c)

{-
ghci> runState threeCoins (mkStdGen 33)  
((True,False,True),680029187 2103410263)
-}



{- Error's -}

{-
Maybe is used to add a context of possible failure to values.
Either type allows us to incorporate a context of possible failure to our values.

ghci> :t Right 4  
Right 4 :: (Num t) => Either a t  
ghci> :t Left "out of cheese error"  
Left "out of cheese error" :: Either [Char] b

The Monad instance is similar to that of maybe and it can be found in Control.Monad.Error:
instance (Error e) => Monad (Either e) where  
    return x = Right x   
    Right x >>= f = f x  
    Left err >>= f = Left err  
    fail msg = Left (strMsg msg)


The >>= examines two possible cases: a Left and a Right. In the case of a Right, the function f is applied to
    the value inside it, similar to how in the case of a Just, the function is just applied to its contents.
    In the case of an error, the Left value is kept, along with its contents, which describe the failure.

A good example of an Error instance is the String type. The strMsg function just returns the string that it got:
ghci> :t strMsg  
strMsg :: (Error a) => String -> a  
ghci> strMsg "boom!" :: String  
"boom!"

Usually a String is used to describe the error when using Either. When a pattern match fails in do notation,
    a Left value is used to signify this failure

ghci> Left "boom" >>= \x -> return (x+1)  
Left "boom"  
ghci> Right 100 >>= \x -> Left "no way!"  
Left "no way!"



When we try to feed a Right value to a function that also succeeds, we're tripped up by a peculiar type error.

ghci> Right 3 >>= \x -> return (x + 100)  
  
<interactive>:1:0:  
    Ambiguous type variable `a' in the constraints:  
      `Error a' arising from a use of `it' at <interactive>:1:0-33  
      `Show a' arising from a use of `print' at <interactive>:1:0-33  
    Probable fix: add a type signature that fixes these type variable(s)



Haskell says that it doesn't know which type to choose for the e part of our Either e a typed value, even
    though we're just printing the Right part. This is due to the Error e constraint on the Monad instance.
    So if you get type errors like this one when using Either as a monad, just add an explicit type signature:
ghci> Right 3 >>= \x -> return (x + 100) :: Either String Int  
Right 103
-}


{- Some useful Monadic functions -}
-- Functions that operate on monadic values or return monadic values

{-
But even though every monad is a functor, we don't have to rely on it having a Functor instance because of the
    liftM function. liftM takes a function and a monadic value and maps it over the monadic value. So it's
    pretty much the same thing as fmap. This is liftM's type:
liftM :: (Monad m) => (a -> b) -> m a -> m b 
And this is thje type for fmap:
fmap :: (Functor f) => (a -> b) -> f a -> f b 
-}

{-
If the Functor and Monad instances for a type obey the functor and monad laws, these two amount to the same
    thing (and all the monads that we've met so far obey both). This is kind of like pure and return do the
    same thing, only one has an Applicative class constraint whereas the other has a Monad one.

ghci> liftM (*3) (Just 8)  
Just 24  
ghci> fmap (*3) (Just 8)  
Just 24  
ghci> runWriter $ liftM not $ Writer (True, "chickpeas")  
(False,"chickpeas")  
ghci> runWriter $ fmap not $ Writer (True, "chickpeas")  
(False,"chickpeas")  
ghci> runState (liftM (+100) pop) [1,2,3,4]  
(101,[2,3,4])  
ghci> runState (fmap (+100) pop) [1,2,3,4]  
(101,[2,3,4]) 
-}

{-
This is how liftM is implemented:
liftM :: (Monad m) => (a -> b) -> m a -> m b  
liftM f m = m >>= (\x -> return (f x)) 

- or using do notation:
liftM :: (Monad m) => (a -> b) -> m a -> m b  
liftM f m = do  
    x <- m  
    return (f x)
-}

{-
The Applicative type class allows us to apply functions between values with contexts as if they were normal
    values. Like this:

ghci> (+) <$> Just 3 <*> Just 5  
Just 8  
ghci> (+) <$> Just 3 <*> Nothing  
Nothing

Using this applicative style makes things pretty easy. <$> is just fmap and <*> is a function from the
    Applicative type class that has the following type:
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b

The ap function is basically <*>, only it has a Monad constraint instead of an Applicative one.
    Here's its definition:
-}
ap :: (Monad m) => m (a -> b) -> m a -> m b  
ap mf m = do        -- mf is a monadic value whose result is a function.
    f <- mf
    x <- m  
    return (f x)
{-
Because the function is in a context as well as the value, we get the function from the context and call it
    f, then get the value and call that x and then finally apply the function to the value and present that
    as a result.

ghci> Just (+3) <*> Just 4  
Just 7  
ghci> Just (+3) `ap` Just 4  
Just 7  
ghci> [(+1),(+2),(+3)] <*> [10,11]  
[11,12,12,13,13,14]  
ghci> [(+1),(+2),(+3)] `ap` [10,11]  
[11,12,12,13,13,14]

Now we see that monads are stronger than applicatives as well, because we can use the functions from Monad
    to implement the ones for Applicative.
In fact, many times when a type is found to be a monad, people first write up a Monad instance and then make
    an Applicative instance by just saying that pure is return and <*> is ap. Similarly, if you already have
    a Monad instance for something, you can give it a Functor instance just saying that fmap is liftM.


The liftA2 function is a convenience function for applying a function between two applicative values. It's
    defined by:
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
liftA2 f x y = f <$> x <*> y
The liftM2 function does the same thing, only it has a Monad constraint. There also exist liftM3 and liftM4
    and liftM5.


We saw how monads are stronger than applicatives and functors and how even though all monads are functors and
    applicative functors, they don't necessarily have Functor and Applicative instances, so we examined the
    monadic equivalents of the functions that functors and applicative functors use.
-}



{- The join function -}
{-
It turns out that any nested monadic value can be flattened and that this is actually a property unique to monads.
    For this, the join function exists. Its type is this:
join :: (Monad m) => m (m a) -> m a

So it takes a monadic value within a monadic value and gives us just a monadic value, so it sort of flattens it.
    Here it is with some Maybe values:
ghci> join (Just (Just 9))  
Just 9  
ghci> join (Just Nothing)  
Nothing  
ghci> join Nothing  
Nothing

ghci> join [[1,2,3],[4,5,6]]  
[1,2,3,4,5,6]


As you can see, for lists, join is just concat. To flatten a Writer value whose result is a Writer value
    itself, we have to mappend the monoid value.
ghci> runWriter $ join (Writer (Writer (1,"aaa"),"bbb"))  
(1,"bbbaaa")

Flattening Either values is very similar to flattening Maybe values:
ghci> join (Right (Right 9)) :: Either String Int  
Right 9  
ghci> join (Right (Left "error")) :: Either String Int  
Left "error"  
ghci> join (Left "error") :: Either String Int  
Left "error"

- If we apply join to a stateful computation whose result is a stateful computation, the result is a stateful
    computation that first runs the outer stateful computation and then the resulting one.
ghci> runState (join (State $ \s -> (push 10,1:2:s))) [0,0,0]  
((),[10,1,2,0,0,0])
-}


{-
The implementation for join is as follows

join :: (Monad m) => m (m a) -> m a  
join mm = do  
    m <- mm  
    m

Because the result of mm is a monadic value, we get that result and then just put it on a line of its own
    because it's a monadic value. The trick here is that when we do m <- mm, the context of the monad in
    which we are in gets taken care of.

erhaps the most interesting thing about join is that for every monad, feeding a monadic value to a function
    with >>= is the same thing as just mapping that function over the value and then using join to flatten
    the resulting nested monadic value.
In other words, m >>= f is always the same thing as join (fmap f m)
With >>=, we're always thinking about how to feed a monadic value to a function that takes a normal value
    but returns a monadic value. If we just map that function over the monadic value, we have a monadic
    value inside a monadic value.
-}


{- filterM -}
{-
he filter function is pretty much the bread of Haskell programming (map being the butter). It takes a
    predicate and a list to filter out and then returns a new list where only the elements that satisfy the
    predicate are kept. Its type is this:
filter :: (a -> Bool) -> [a] -> [a]

The filterM function from Control.Monad does just what we want
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]

ghci> filter (\x -> x < 4) [9,1,5,2,10,3]
[1,2,3]
-}

-- keepSmall :: Int -> Writer [String] Bool  
-- keepSmall x  
--     | x < 4 = do  
--         tell ["Keeping " ++ show x]  
--         return True  
--     | otherwise = do  
--         tell [show x ++ " is too large, throwing it away"]  
--         return False

{-
Because the predicate returns a Writer value, the resulting list will also be a Writer value.

ghci> fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]  
[1,2,3]

Examining the result of the resulting Writer value, we see that everything is in order.
ghci> mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]  
9 is too large, throwing it away  
Keeping 1  
5 is too large, throwing it away  
Keeping 2  
10 is too large, throwing it away  
Keeping 3


A very cool Haskell trick is using filterM to get the powerset of a list (if we think of them as sets for
    now). The powerset of some set is a set of all subsets of that set.
[1,2,3]  
[1,2]  
[1,3]  
[1]  
[2,3]  
[2]  
[3]  
[]


To make a function that returns a powerset of some list, we're going to rely on non-determinism.
We take the list [1,2,3] and then look at the first element, which is 1 and we ask ourselves: should we keep it or drop it? Well, we'd like to do both actually. So we are going to filter a list and we'll use a predicate that non-deterministically both keeps and drops every element from the list. Here's our powerset function
-}
powerset :: [a] -> [[a]]  
powerset xs = filterM (\x -> [True, False]) xs

{- foldM -}
{-
The monadic counterpart to foldl is foldM.
foldl takes a binary function, a starting accumulator and a list to fold up and then folds
    it from the left into a single value by using the binary function. foldM does the same
    thing, except it takes a binary function that produces a monadic value and folds the
    list up with that. Unsurprisingly, the resulting value is also monadic.
    The type of foldl is:
foldl :: (a -> b -> a) -> a -> [b] -> a  
Whereas foldM has the following type:
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a

The value that the binary function returns is monadic and so the result of the whole fold
    is monadic as well. Let's sum a list of numbers with a fold:
ghci> foldl (\acc x -> acc + x) 0 [2,8,3,1]  
14
-}
binSmalls :: Int -> Int -> Maybe Int  
binSmalls acc x  
    | x > 9     = Nothing  
    | otherwise = Just (acc + x) 
{-
ghci> foldM binSmalls 0 [2,8,3,1]  
Just 14  
ghci> foldM binSmalls 0 [2,11,3,1]  
Nothing  
-}


{- Making a safe RPN calculator -}
solveRPN :: String -> Double  
solveRPN = head . foldl foldingFunction [] . words

foldingFunction :: [Double] -> String -> [Double]  
foldingFunction (x:y:ys) "*" = (x * y):ys  
foldingFunction (x:y:ys) "+" = (x + y):ys  
foldingFunction (x:y:ys) "-" = (y - x):ys  
foldingFunction xs numberString = read numberString:xs

foldingFunction' :: [Double] -> String -> Maybe [Double]
foldingFunction' (x:y:ys) "*" = return ((x * y):ys)  
foldingFunction' (x:y:ys) "+" = return ((x + y):ys)  
foldingFunction' (x:y:ys) "-" = return ((y - x):ys)  
foldingFunction' xs numberString = liftM (:xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a  
readMaybe st = case reads st of [(x,"")] -> Just x  
                                _ -> Nothing

-- solveRPN' :: String -> Maybe Double  
-- solveRPN' st = do  
--     [result] <- foldM foldingFunction [] (words st)  
--     return result 

{-
ghci> readMaybe "1" :: Maybe Int  
Just 1  
ghci> readMaybe "GO TO HELL" :: Maybe Int  
Nothing

ghci> foldingFunction [3,2] "*"  
Just [6.0]  
ghci> foldingFunction [3,2] "-"  
Just [-1.0]  
ghci> foldingFunction [] "*"  
Nothing  
ghci> foldingFunction [] "1"  
Just [1.0]  
ghci> foldingFunction [] "1 wawawawa"  
Nothing  

ghci> solveRPN "1 2 * 4 +"  
Just 6.0  
ghci> solveRPN "1 2 * 4 + 5 *"  
Just 30.0  
ghci> solveRPN "1 2 * 4"  
Nothing  
ghci> solveRPN "1 8 wharglbllargh"  
Nothing
-}



{- Composing monadic functions -}
{-
When we were learning about the monad laws, we said that the <=< function is just like
    composition, only instead of working for normal functions like a -> b, it works for
    monadic functions like a -> m b.

ghci> let f = (+1) . (*100)  
ghci> f 4  
401  
ghci> let g = (\x -> return (x+1)) <=< (\x -> return (x*100))  
ghci> Just 4 >>= g  
Just 401

ghci> let f = foldr (.) id [(+1),(*100),(+1)]  
ghci> f 1  
201


We had a function called moveKnight which took the knight's position on the board and
    returned all the possible moves that he can make next. Then, to generate all the
    possible positions that he can have after taking three moves, we made the following
    function:
-}
-- in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

-- canReachIn3 :: KnightPos -> KnightPos -> Bool  
-- canReachIn3 start end = end `elem` in3 start

-- inMany :: Int -> KnightPos -> [KnightPos]  
-- inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

-- canReachIn :: Int -> KnightPos -> KnightPos -> Bool  
-- canReachIn x start end = end `elem` inMany x start

-- Modified for it to reach a position in x amount of moves from x position


{- Making Monads -}
newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show

instance Functor Prob where  
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs

{-
ghci> fmap negate (Prob [(3,1%2),(5,1%4),(9,1%4)])  
Prob {getProb = [(-3,1 % 2),(-5,1 % 4),(-9,1 % 4)]}
-}


{-
Seems kind of tricky, so let's make use of the fact that m >>= f always equals join
    (fmap f m) for monads and think about how we would flatten a probability list of
    probability lists. As an example, let's consider this list where there's a 25% chance
    that exactly one of 'a' or 'b' will happen. Both 'a' and 'b' are equally likely to
    occur. Also, there's a 75% chance that exactly one of 'c' or 'd' will happen. 'c' and
    'd' are also equally likely to happen. Here's a picture of a probability list that
    models this scenario:
To find out, all we have to do is multiply each probability with all of probabilities that
    it contains. 'a' would occur one time out of eight, as would 'b', because if we
    multiply one half by one quarter we get one eighth. 'c' would happen three times out
    of eight because three quarters multiplied by one half is three eighths. 'd' would
    also happen three times out of eight. If we sum all the probabilities, they still add
    up to one.
-}
thisSituation :: Prob (Prob Char)  
thisSituation = Prob  
    [( Prob [('a',1%2),('b',1%2)] , 1%4 )  
    ,( Prob [('c',1%2),('d',1%2)] , 3%4)  
    ]

{-
we can write >>= simply as join (fmap f m) and we have ourselves a monad.
So here's flatten, which we'll use because the name join is already taken:
-}
flatten :: Prob (Prob a) -> Prob a  
flatten (Prob xs) = Prob $ concat $ map multAll xs  
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs

-- instance Monad Prob where  
--     return x = Prob [(x,1%1)]  
--     m >>= f = flatten (fmap f m)  
--     fail _ = Prob []

{-
We also defined the fail function, which is the same as it is for lists, so if there's a
    pattern match failure in a do expression, a failure occurs within the context of a
    probability list.

The third law states that f <=< (g <=< h) should be the same as (f <=< g) <=< h.
-}



{-
Now that we have a monad, what can we do with it? Well, it can help us do calculations
    with probabilities. We can treat probabilistic events as values with contexts and
    the probability monad will make sure that those probabilities get reflected in the
    probabilities of the final result.
-}
-- data Coin = Heads | Tails deriving (Show, Eq)  
  
-- coin :: Prob Coin  
-- coin = Prob [(Heads,1%2),(Tails,1%2)]  
  
-- loadedCoin :: Prob Coin  
-- loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

-- flipThree :: Prob Bool  
-- flipThree = do  
--     a <- coin  
--     b <- coin  
--     c <- loadedCoin  
--     return (all (==Tails) [a,b,c])
{-
ghci> getProb flipThree  
[(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),  
 (False,1 % 40),(False,9 % 40),(False,1 % 40),(True,9 % 40)]
-}