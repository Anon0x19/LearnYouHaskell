module Chapter12 where
import Data.List
import Data.Char
import Control.Monad.Instances
import Control.Applicative
import qualified Control.Applicative as Control
import Data.Monoid
import qualified Data.Foldable as F
import Control.Monad

{-
            A Fistful of Monads
-}

{-
fmap definition:
fmap :: (Functor f) => (a -> b) -> f a -> f b

Then all you have to do is make it work by writting the appropriate Functor instance.


The applicative typeclass was introduced as an imrpoved for cases of doing thing such as
    applying Just (*3) to Just 5, or applying Just 5 to Nothing, or [(*2),(+4)] to
    [1,2,3].
Defined by:
    (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b

With the applicative typeclass, it was discovered that you could wrap a 1 to become [1]
    or Just 1, or an I/O that does nothing and yeld 1. This is called pure.

Applicative values are just values with added context
i.e
'a' is normal whereas Just 'a' has more context
Char is normal whereas Maybe Char has more added context

Maybe a represents that a computation might have failed
[a] represents a computation with several results
-}

{- Using Applicative type class on normal functions
ghci> (*) <$> Just 2 <*> Just 8  
Just 16  
ghci> (++) <$> Just "klingon" <*> Nothing  
Nothing  
ghci> (-) <$> [3,4] <*> [1,2,3]  
[2,1,0,3,2,1]  
-}




{- If we have a fancy value and a function that takes a normal value but returns a fancy value, how do we feed that fancy value into the function?
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b

We write m a instead of f a because the m stands for Monad, but monads are just
    applicative functors that support >>=
The >>= function is pronounced as bind.
-}

-- To no one-s suprise, Maybe is a monad
{-
A value of Maybe a represents a value of type a whereas the value Nothing represents
    absence as one of the previous computations might have failed.

When looking a Maybe as a functor, we saw that when we want to fmap a function over it,
    it gets mapped over the insides if it's a Just value, otherwise the Nothing is kept
    kept as there is nothing to map it over.

ghci> fmap (++"!") (Just "wisdom")  
Just "wisdom!"  
ghci> fmap (++"!") Nothing  
Nothing

As Maybe is an applicative functor is such way that when we use <*> to apply a function
    inside a Maybe, talues have to be Just values for the result to be a Just value,
    otherwise the result is Nothing.

ghci> max <$> Just 3 <*> Just 6  
Just 6  
ghci> max <$> Just 3 <*> Nothing  
Nothing
-}

{-
>>= would take a Maybe a value and a function of type a -> Maybe b and somehow apply
    the function to the Maybe a. 
-}


{-
Instead of calling it >>=, let's call it applyMaybe for now. It takes a Maybe a and a
    function that returns a Maybe b and manages to apply that function to the Maybe a.
    Here it is in code:
-}
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b  
applyMaybe Nothing f  = Nothing  
applyMaybe (Just x) f = f x

{-
ghci> Just 3 `applyMaybe` \x -> Just (x+1)  
Just 4  
ghci> Just "smile" `applyMaybe` \x -> Just (x ++ " :)")  
Just "smile :)"  
ghci> Nothing `applyMaybe` \x -> Just (x+1)  
Nothing  
ghci> Nothing `applyMaybe` \x -> Just (x ++ " :)")  
Nothing  
-}

{-
ghci> Just 3 `applyMaybe` \x -> if x > 2 then Just x else Nothing  
Just 3  
ghci> Just 1 `applyMaybe` \x -> if x > 2 then Just x else Nothing  
Nothing  
-}

-- As expected, if the monadic value on the left is a Nothing, the whole thing is Nothing.
-- And if the function on the right returns a Nothing, the result is Nothing again.



{-
            The Monad type class
-}

-- Just like functors have the Functor type class, and Applicative functors have the
    -- Applicative type class, monads also come with their own typeclass (Monad)

{-
class Monad m where  
    return :: a -> m a  
  
    (>>=) :: m a -> (a -> m b) -> m b  
  
    (>>) :: m a -> m b -> m b  
    x >> y = x >>= \_ -> y  
  
    fail :: String -> m a  
    fail msg = error msg

- The first return is the Same as pure (from the Applicative typeclass), just with a
    different name (and its type is (Monad m) => a -> m a)
    We used it to take a value and make a bogus I/O action that does nothing but yield
    that value. For Maybe it takes a value and wraps it in a Just.

-- Note: every monad is an applicative functor even if the Monad class declaration doesn't say so. 
    BUT you don't define it as class (Applicative m) = > Monad m where ... but when
    Hasekell was made it hadn't occur ot people that applicative functos are a good
    fit for Haskell.

>>=, or bind. is like function application, only instead of taking a normal value and
    feeding it to a normal function, it takes a monadic value (that is, a value with a
    context) and feeds it to a function that takes a normal value but returns a monadic
    value.

>>, is pretty much never implemented when making Monad instances as it comes with
    default implementation

fail, is the final function in the Monad type class, its not explicitly used much in code,
    instead its used by Haskell to enable failure in a special syntatic construct for monads.
-}

{-
The Maybe instance of a monad

instance Monad Maybe where  
    return x = Just x  
    Nothing >>= f = Nothing  
    Just x >>= f  = f x  
    fail _ = Nothing  

The return is the same as pure. So its the same as what is done in the Applicative type
    class and you wrap it in a Just
-}

{-
The >>= function is the same as the applyMaybe. When feeding the Maybe a to our function,
    we return Nothing if the value one the left is Nothing. As there is no way to apply
    our function to the value. Meanwhile if its a Just, we take what's inside and apply
    f to it.

i.e
ghci> return "WHAT" :: Maybe String  
Just "WHAT"  
ghci> Just 9 >>= \x -> return (x*10)  
Just 90  
ghci> Nothing >>= \x -> return (x*10)  
Nothing

-- First i.e shows nothing new as its a pure with Maybe (and we know return is just pure
    with a different name). The other 2 do show >>= a bit better.

- When we feed Just 9 to the function \x -> return (x*10), the x takes on the value 9
    inside the function. Although it seems you are able to extract the value from
    Maybe without pattern matching, without losing context of the Maybe. This is due
    to the result of >>= being Nothing when its Nothing.
-}



{- Walk The Line -}

{-
let's see how we can use >>= repeatedly to handle computations of several Maybe a values.
-}

{-
We are going to represent the birds flying to and from the pole as if Pierre is still
    at it.
We can represent the pole with a simple pair of integers. The first component will
    signify the number of birds on the left side and the second component the number of
    birds on the right side:
First we made a type synonym for Int, called Birds, because we're using integers to
    represent how many birds there are. And then we made a type synonym (Birds,Birds)
    and we called it Pole.
-}
type Birds = Int  
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Pole  
landLeft n (left,right) = (left + n,right)  
  
landRight :: Birds -> Pole -> Pole  
landRight n (left,right) = (left,right + n)

{-
To make birds fly away we just had a negative number of birds land on one side.
    Because landing a bird on the Pole returns a Pole

ghci> landLeft 2 (0,0)  
(2,0)  
ghci> landRight 1 (1,2)  
(1,3)  
ghci> landRight (-1) (1,2)  
(1,1)
ghci> landLeft 2 (landRight 1 (landLeft 1 (0,0)))  
(3,1)  
-}

-- Cleaner way of writing f x
(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

{-
ghci> 100 -: (*3)  
300  
ghci> True -: not  
False  
ghci> (0,0) -: landLeft 2  
(2,0)

ghci> (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2  
(3,1)

ghci> landLeft 10 (0,3)  
(10,3)

ghci> (0,0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)  
(0,2)
-}


-- Instead of returning a Pole these functions now return a Maybe Pole.
landLeft' :: Birds -> Pole -> Maybe Pole  
landLeft' n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  
  
landRight' :: Birds -> Pole -> Maybe Pole  
landRight' n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing

{-
We use guards to check if the difference between the number of birds on the new pole is
    less than 4. If it is, we wrap the new pole in a Just and return that. If it isn't,
    we return a Nothing, indicating failure.
-}

{-
ghci> landLeft 2 (0,0)  
Just (2,0)  
ghci> landLeft 10 (0,3)  
Nothing

- We can also bind it to the function by passing Maybe Pole to a function that takes
    pole and returning Maybe Pole and then we use the bind function to pass it to another
    function.
ghci> landRight 1 (0,0) >>= landLeft 2  
Just (2,1)

ghci> Nothing >>= landLeft 2  
Nothing

we can now chain landings that may fail because >>= allows us to feed a monadic value
    to a function that takes a normal one.
i.e:
ghci> return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2  
Just (2,4)

ghci> (0,0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)  
(0,2)
ghci> return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)  
Nothing        -- Expected as at one point one side is over 4 bigger than the other
-}

banana :: Pole -> Maybe Pole  
banana _ = Nothing

{-
ghci> return (0,0) >>= landLeft 1 >>= banana >>= landRight 1  
Nothing
-}

{-
Instead of making functions that ignore their input and just return a predetermined
    monadic value, we can use the >> function, whose default implementation is this:

(>>) :: (Monad m) => m a -> m b -> m b  
m >> n = m >>= \_ -> n

With monads however, their context and meaning has to be considered as well. Here's
    how >> acts with Maybe:
ghci> Nothing >> Just 3  
Nothing  
ghci> Just 3 >> Just 4  
Just 4  
ghci> Just 3 >> Nothing  
Nothing


If you replace >> with >>= \_ ->, it's easy to see why it acts like it does.

We can replace our banana function in the chain with a >> and then a Nothing:
ghci> return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1  
Nothing 
-}

routine :: Maybe Pole
routine = case landLeft' 1 (0,0) of  
    Nothing -> Nothing  
    Just pole1 -> case landRight' 4 pole1 of   
        Nothing -> Nothing  
        Just pole2 -> case landLeft' 2 pole2 of  
            Nothing -> Nothing  
            Just pole3 -> landLeft' 1 pole3
-- In the case of failure it returns Nothing

{-
            Do Notation
-}

{-
Monads in Haskell are so useful that they got their own special syntax called do notation.
The use fof the do notation is to 'glue' together monadic values in sequence.

ghci> Just 3 >>= (\x -> Just (show x ++ "!"))  
Just "3!"
ghci> Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))  
Just "3!"   -- x is still 3 and y becomes "!"

ghci> let x = 3; y = "!" in show x ++ y  
"3!"

ghci> Nothing >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))  
Nothing  
ghci> Just 3 >>= (\x -> Nothing >>= (\y -> Just (show x ++ y)))  
Nothing  
ghci> Just 3 >>= (\x -> Just "!" >>= (\y -> Nothing))  
Nothing 
-}

-- an example function to concat both x and y
foo :: Maybe String  
foo = Just 3   >>= (\x -> 
      Just "!" >>= (\y -> 
      Just (show x ++ y)))

-- Written the same but using do instead of having to stick to lambda functions
    -- every line in a do expression is a monadic value and to inspect the result we use
    -- <-. Just like >>= feeds monadic values to lamdahs. The last line of a do expression
    -- can't be inspected, and rather returns the 'glued up' monadic value.
foo' :: Maybe String  
foo' = do  
    x <- Just 3  
    y <- Just "!"  
    Just (show x ++ y)

{-
ghci> Just 9 >>= (\x -> Just (x > 8))  
Just True  
-}

-- Rewrapping the previous instance into a do notation:
marySue :: Maybe Bool  
marySue = do   
    x <- Just 9  
    Just (x > 8)
{-
If we compare these two, it's easy to see why the result of the whole monadic value is
    the result of the last monadic value in the do expression with all the previous
    ones chained into it.
-}

{-
This is the tightwalker's routine which can be expressed in do notation
-}
routine' :: Maybe Pole  
routine' = do  
    start <- return (0,0)  
    first <- landLeft' 2 start  
    second <- landRight' 2 first  
    landLeft' 1 second
{-
ghci> routine'
Just (3,2) 
-}

-- same notation as do but you write all the steps and its longer
routine'' :: Maybe Pole  
routine'' =   
    case Just (0,0) of   
        Nothing -> Nothing  
        Just start -> case landLeft' 2 start of  
            Nothing -> Nothing  
            Just first -> case landRight' 2 first of  
                Nothing -> Nothing  
                Just second -> landLeft' 1 second

-- Returns nothing as Pierre falls
routine''' :: Maybe Pole  
routine''' = do  
    start <- return (0,0)  
    first <- landLeft' 2 start  
    Nothing             -- Prettier than writting _ <- Nothing
    second <- landRight' 2 first  
    landLeft' 1 second


-- When pattern matching if the first pattern fails, the next pattern is attempted to match.
justH :: Maybe Char  
justH = do  
    (x:xs) <- Just "hello"  
    return x
-- If the matching fails, the program is halted and an error is outputted
-- On the other hand, failed pattern matching in let expressions results in an error
    -- being produced right away
-- When pattern matching fails in a do expression, the fail function is called. 
    -- (As its part of the Monad type clas) instead of making our program crash

{-
The default implementation is:

fail :: (Monad m) => String -> m a  
fail msg = error msg

So by default it does make our program crash, but monads that incorporate a context of possible failure (like Maybe) usually implement it on their own. For Maybe, its implemented like so:
fail _ = Nothing

It ignores the error message and makes a Nothing. So when pattern matching fails in a
    Maybe value that's written in do notation, the whole value results in a Nothing.
    This is preferable to having our program crash. Here's a do expression with a
    pattern that's bound to fail:
wopwop :: Maybe Char  
wopwop = do  
    (x:xs) <- Just ""  
    return x

ghci> wopwop  
Nothing
-}



{-
            The list monad
-}

{-
ghci> (*) <$> [1,2,3] <*> [10,100,1000]  
[10,100,1000,20,200,2000,30,300,3000] 
-}

{-
Monad instance for lists:

- return does the same thing as pure
instance Monad [] where  
    return x = [x]  
    xs >>= f = concat (map f xs)  
    fail _ = []
-}

{-
ghci> [3,4,5] >>= \x -> [x,-x]  
[3,-3,4,-4,5,-5]

ghci> [] >>= \x -> ["bad","mad","rad"]  
[]  
ghci> [1,2,3] >>= \x -> []  
[]

ghci> (*) <$> [1,2,3] <*> [10,100,1000] >>= \x -> return (x)
[10,100,1000,20,200,2000,30,300,3000]

ghci> [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)  
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]
-}

listOfTuples :: [(Int,Char)]  
listOfTuples = do  
    n <- [1,2]  
    ch <- ['a','b']  
    return (n,ch)

{-
ghci> [ (n,ch) | n <- [1,2], ch <- ['a','b'] ]  
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]

ghci> [ x | x <- [1..50], '7' `elem` show x ]  
[7,17,27,37,47]  
-}

{-
We apply show to x to turn our number into a string and then we check if the character
    '7' is part of that string.

To see how filtering in list comprehensions translates to the list monad, we have to
    check out the guard function and the MonadPlus type class.
class Monad m => MonadPlus m where  
    mzero :: m a  
    mplus :: m a -> m a -> m a

mzero is synonymous to mempty from the Monoid type class and mplus corresponds to
    mappend. Because lists are monoids as well as monads, they can be made an instance
    of this type class:
instance MonadPlus [] where  
    mzero = []  
    mplus = (++)

For lists mzero represents a non-deterministic computation that has no results at
    all — a failed computation. mplus joins two non-deterministic values into one.
    The guard function is defined like this:
guard :: (MonadPlus m) => Bool -> m ()  
guard True = return ()  
guard False = mzero
-}

{-
It takes a boolean value and if it's True, takes a () and puts it in a minimal default
    context that still succeeds. Otherwise, it makes a failed monadic value.

ghci> guard (5 > 2) :: Maybe ()  
Just ()  
ghci> guard (1 > 2) :: Maybe ()  
Nothing  
ghci> guard (5 > 2) :: [()]  
[()]  
ghci> guard (1 > 2) :: [()]  
[]

ghci> [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)  
[7,17,27,37,47]


ghci> guard (5 > 2) >> return "cool" :: [String]  
["cool"]  
ghci> guard (1 > 2) >> return "cool" :: [String]  
[]  
-}


-- If you don't include the return as the end, the list would be an empty list of tuples
sevensOnly :: [Int]  
sevensOnly = do  
    x <- [1..50]  
    guard ('7' `elem` show x)  
    return x

{-
ghci> [ x | x <- [1..50], '7' `elem` show x ]  
[7,17,27,37,47] 
-}




{-
            A knights quest
-}

{-
Here's a problem that really lends itself to being solved with non-determinism. Say
    you have a chess board and only one knight piece on it. We want to find out if the
    knight can reach a certain position in three moves. We'll just use a pair of numbers
    to represent the knight's position on the chess board. The first number will determine
    the column he's in and the second number will determine the row.
-}

type KnightPos = (Int,Int)

{-
The knight can always take one step horizontally or vertically and two steps
    horizontally or vertically but its movement has to be both horizontal and vertical.
    (c',r') takes on every value from the list of movements and then guard makes sure
    that the new move, (c',r') is still on the board. If it it's not, it produces an
    empty list, which causes a failure and return (c',r') isn't carried out for that
    position.
-}
moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = do  
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]  
    guard (c' `elem` [1..8] && r' `elem` [1..8])  
    return (c',r')

moveKnight' :: KnightPos -> [KnightPos]  
moveKnight' (c,r) = filter onBoard  
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
    ]  
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

{-
ghci> moveKnight (6,2)  
[(8,1),(8,3),(4,1),(4,3),(7,4),(5,4)]  
ghci> moveKnight (8,1)  
[(6,2),(7,3)]  
-}

-- Returns next 3 possible positions
in3 :: KnightPos -> [KnightPos]  
in3 start = do   
    first <- moveKnight start  
    second <- moveKnight first  
    moveKnight second

in3' :: KnightPos -> [KnightPos]
in3' start = return start >>= moveKnight >>= moveKnight >>= moveKnight
-- Returns bool to a coordinate spot (whether or not in can move there in 3 steps)
canReachIn3 :: KnightPos -> KnightPos -> Bool  
canReachIn3 start end = end `elem` in3 start

{-
ghci> (6,2) `canReachIn3` (6,1)  
True

ghci> (6,2) `canReachIn3` (7,3)  
False  
-}





{-
                Monad Laws
-}

{-
Just like applicative functors and functors before them, monads also come with laws that
    must be abided by. Just because something is an instance of a monad doesn't mean it is.
    For a type to truely be a monad, the monad laws must hold for that type-
-}

-- Left identity:
{-
The first monad law states that if we take a value, put it in a default context with
    return and then feed it to a function by using >>=, it's the same as just taking
    the value and applying the function to it.

return x >>= f is the same damn thing as f x

For the Maybe monad return is defined as Just. I.e:
ghci> return 3 >>= (\x -> Just (x+100000))  
Just 100003  
ghci> (\x -> Just (x+100000)) 3  
Just 100003 


For the list monad return puts something in a singleton list. The >>= implementation
    for lists goes over all the values in the list and applies the function to them. I.e:
ghci> return "WoM" >>= (\x -> [x,x,x])  
["WoM","WoM","WoM"]  
ghci> (\x -> [x,x,x]) "WoM"  
["WoM","WoM","WoM"]

We said that for IO, using return makes an I/O action that has no side-effects but just
    presents a value as its result. So it makes sense that this law holds for IO as well.
-}

-- Right identity:
{-
he second law states that if we have a monadic value and we use >>= to feed it to return,
    the result is our original monadic value.

m >>= return is no different than just m

When we feed monadic values to functions by using >>=, those functions take normal values
    and return monadic ones. return is also one such function, if you consider its type.
        Like we said, return puts a value in a minimal context that still presents that 
        value as its result. This means that, for instance, for Maybe, it doesn't introduce
        any failure and for lists, it doesn't introduce any extra non-determinism. 

ghci> Just "move on up" >>= (\x -> return x)  
Just "move on up"  
ghci> [1,2,3,4] >>= (\x -> return x)  
[1,2,3,4]  
ghci> putStrLn "Wah!" >>= (\x -> return x)  
Wah!

If we take a closer look at the list example, the implementation for >>= is:
xs >>= f = concat (map f xs)

So when we feed [1,2,3,4] to return, first return gets mapped over [1,2,3,4], resulting
    in [[1],[2],[3],[4]] and then this gets concatenated and we have our original list.


Left identity and right identity are basically laws that describe how return should behave.
    It's an important function for making normal values into monadic ones and it wouldn't
    be good if the monadic value that it produced did a lot of other stuff.
-}

-- Associativity
{-
The final monad law says that when we have a chain of monadic function applications with
    >>=, it shouldn't matter how they're nested.
Doing (m >>= f) >>= g is just like doing m >>= (\x -> f x >>= g)
We have one monadic value, m and two monadic functions f and g. When we're doing
    (m >>= f) >>= g, we're feeding m to f, which results in a monadic value. Then, we
    feed that monadic value to g. In the expression m >>= (\x -> f x >>= g), we take a
    monadic value and we feed it to a function that feeds the result of f x to g.

Using the tightrope code, we can stimulate birds landing on his plancing pole by making
    a chain of several function that might produce failure.
ghci> return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2  
Just (2,4)

We started with Just (0,0) and then bound that value to the next monadic function,
    landRight 2. The result of that was another monadic value which got bound into the
    next monadic function, and so on. If we were to explicitly parenthesize this, we'd
    write:
ghci> ((return (0,0) >>= landRight 2) >>= landLeft 2) >>= landRight 2  
Just (2,4)

Although the function can be written as:
return (0,0) >>= (\x -> 
landRight 2 x >>= (\y -> 
landLeft 2 y >>= (\z -> 
landRight 2 z)))

- return (0,0) is the same as Just (0,0) and when we feed it to the lambda, the x becomes (0,0)
- landRight takes a number of birds and a pole (a tuple of numbers)
- This then results in a Just (0,2) and so on

So it doesn't matter how you nest feeding values to monadic functions, what matters is
    their meaning.

Here's another way to look at this law: consider composing two functions, f and g.
    Composing two functions is implemented using:
(.) :: (b -> c) -> (a -> b) -> (a -> c)  
f . g = (\x -> f (g x))


If the type of g is a -> b and the type of f is b -> c, we arrange them into a new
    function which has a type of a -> c  so that its parameter is passed between those
    functions.



If we had a function of type a -> m b, we couldn't just pass its result to a function
    of type b -> m c, because that function accepts a normal b, not a monadic one. We
    could however, use >>= to make that happen. So by using >>=, we can compose two
    monadic functions:
(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)  
f <=< g = (\x -> g x >>= f)

So we can now compose two monadic functions:
ghci> let f x = [x,-x]  
ghci> let g x = [x*3,x*2]  
ghci> let h = f <=< g  
ghci> h 3  
[9,-9,6,-6]

Well, when we look at the law as a law of compositions, it states that f <=< (g <=< h)
    should be the same as (f <=< g) <=< h. This also shows that for monads, the nesting
    of operations shouldn't matter.


If we translate the first two laws to use <=<, then the left identity law states that
    for every monadic function f, f <=< return is the same as writing just f and the
    right identity law says that return <=< f is also no different from f.

This is very similar to how if f is a normal function, (f . g) . h is the same as
    f . (g . h), f . id is always the same as f and id . f is also just f.
-}