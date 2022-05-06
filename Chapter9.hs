module Chapter9 where
import Data.Char
import Data.List
import Control.Monad
-- Allows you to import utils to allow to open files, etc
import System.IO
import System.Directory
-- System.Environment has the getArgs
import System.Environment
-- Allows you to generate random numbers
--import System.Random
import System.Random.Stateful

import qualified Data.ByteString.Lazy as BSL  
import qualified Data.ByteString as BS  




-- List of all of ioe's (IOError's): https://downloads.haskell.org/~ghc/6.10.1/docs/html/libraries/base/System-IO-Error.html#3






{-
           Hello, world!
-}

main :: IO ()
main = putStrLn "hello, world"

{-
ghci> :t putStrLn  
putStrLn :: String -> IO ()  
ghci> :t putStrLn "hello, world"  
putStrLn "hello, world" :: IO ()
-}

main' :: IO ()
main' = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")

{-
ghci> :t getLine  
getLine :: IO String
-}

-- Runs the tellFortune function and passes the name as a parameters, then returns
    -- Yours fortune for the name
-- main'' :: IO ()
-- main'' = do  
--     putStrLn "Hello, what's your name?"  
--     name <- getLine  
--     putStrLn $ "Read this carefully, because this is your future: " ++ tellFortune name 

-- Allows user to input a text and returns a string followed by the input
main''' :: IO ()
main''' = do  
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Hello, my name is " ++ name

{-
Using name = getLine won't give you the input but rather the I/O action by using "<-"
    The compiler understands that it needs to forward the inputted data to the variable.
-} 
  
main'''' :: IO ()
main'''' = do  
    putStrLn "What's your first name?"  
    firstName <- getLine  
    putStrLn "What's your last name?"  
    lastName <- getLine  
    let bigFirstName = map toUpper firstName  
        bigLastName = map toUpper lastName  
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

-- Reverses an Inputted String
main''''' :: IO ()
main''''' = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            main'''''
  
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words

-- Return won't cause the execution cycle to stop
main'''''' :: IO ()
main'''''' = do  
    return ()  
    return "HAHAHA"  
    line <- getLine  
    return "BLAH BLAH BLAH"  
    return 4  
    putStrLn line

-- Concats 2 strings
main''''''' :: IO ()
main''''''' = do  
    a <- return "hell"  
    b <- return "yeah!"  
    putStrLn $ a ++ " " ++ b


-- Concats 2 strings
main2''''''' :: IO ()
main2''''''' = do  
    let a = "hell"  
        b = "yeah"  
    putStrLn $ a ++ " " ++ b

-- Prints strings
mainputStr :: IO ()
mainputStr = do
    putStr "Hey, "  
    putStr "I'm "  
    putStrLn "Andy!"

-- Prints some characters
mainputChat :: IO ()
mainputChat = do
    putChar 't'
    putChar 'e'
    putChar 'h'

-- Prints some type x when runned
mainPrint :: IO ()
mainPrint = do
    print True  
    print 2  
    print "haha"  
    print 3.2  
    print [3,4,3]

{-
ghci> 3  
3  
ghci> print 3  
3  
ghci> map (++"!") ["hey","ho","woo"]  
["hey!","ho!","woo!"]  
ghci> print (map (++"!") ["hey","ho","woo"])  
["hey!","ho!","woo!"]
-}

maingetChar :: IO ()
maingetChar = do     
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
            maingetChar
        else return ()

-- Example of when function for input and output
mainWhen :: IO ()
mainWhen = do  
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        mainWhen

--These Two Are the exact same, takes 3 inputs and prints them in a list
mainSequence :: IO ()
mainSequence = do  
    a <- getLine  
    b <- getLine  
    c <- getLine  
    print [a,b,c]
mainSequence' :: IO ()
mainSequence' = do  
    rs <- sequence [getLine, getLine, getLine]  
    print rs

{-
ghci> sequence (map print [1,2,3,4,5])  
1  
2  
3  
4  
5  
[(),(),(),(),()] 
-}

{-
ghci> mapM print [1,2,3]  
1  
2  
3  
[(),(),()]  
ghci> mapM_ print [1,2,3]  
1  
2  
3
-}

-- forever acts as an infinite look
mainforever :: IO b
mainforever = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l


-- forM is like mapM (also in Control.Monad), only that the parameters are switched around
-- Usage: var <- forM [] (Expression)
mainforM :: IO [()]
mainforM = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors


{-
Allows you to write to a file firstly by compiling it using ghc --make (file)
And after by using cat (file).txt | ./(file) 
-}
mainIn :: IO b
mainIn = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l

-- Samething as above
mainIn' :: IO ()
mainIn' = do  
    contents <- getContents  
    putStr (map toUpper contents)


{-
- ghc --make (file)
- cat file.txt | ./(file)
-}
-- Program that taks input and returns those that are shorter than 10
mainProgram :: IO ()
mainProgram = do  
    contents <- getContents  
    putStr (shortLinesOnly contents)  
  
shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result


{-
interact takes a function of type String -> String as a parameter and returns an I/O action that will take some input,
run that function on it and then print out the function's result
- ghc --make (file)
- cat file.txt | ./(file)
-}
mainProgram' :: IO ()
mainProgram' = interact shortLinesOnly'
  
shortLinesOnly' :: String -> String  
shortLinesOnly' input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result

-- Samething but in 1 line to show function composition
mainProgram'' :: IO ()
mainProgram'' = interact $ unlines . filter ((<10) . length) . lines


{-
Takes an input and returns whether its a palindrome or not
-}
respondPalindromes :: String -> String
respondPalindromes contents = unlines (map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") (lines contents))  
    where   isPalindrome xs = xs == reverse xs
-- Same program but written point-free (so using f(g(x)) as f . g)
respondPalindromes' :: String -> String
respondPalindromes' = unlines . map (\xs -> if isPalindrome' xs then "palindrome" else "not a palindrome") . lines  
    where   isPalindrome' xs = xs == reverse xs

-- Makes it infinite
mainProgram''' :: IO ()
mainProgram''' = interact respondPalindromes 

{-
-- Prints the contents of the file:

openFile :: FilePath -> IOMode -> IO Handle

"""
Hey! Hey! You! You!   
I don't like your girlfriend!   
No way! No way!   
I think you need a new one! 
"""

import System.IO  
  
main = do  
    handle <- openFile "girlfriend.txt" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle

$ runhaskell girlfriend.hs  
Hey! Hey! You! You!  
I don't like your girlfriend!  
No way! No way!  
I think you need a new one!
-}

-- hGetContents, takes a Handle, so it knows which file to get the contents from

-- hClose, takes the Handle and closes the file

{-
Another way of opening files instead of using openFile, is by using withFile, which has
a type signature of withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a

It takes a path to a file, an IOMode and then it takes a function that takes a handle and returns some I/O action.
    What it returns is an I/O action that will open that file, do something we want with the file and then close it.
-}

{-
FilePath is a type synonym for String

type FilePath = String
-}

{-
IOMode is an enumeration of the different Modes available
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode  
-}

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a  
withFile' path mode f = do  
    handle <- openFile path mode   
    result <- f handle  
    hClose handle  
    return result

{-
Just like gGetConents we have the handle equivalents for I/O:
- hGetLine
- hPutStr
- hPutStrLn
- hGetChar
-}


{-
readFile has a type signature of readFile :: FilePath -> IO String

readFile takes a path to a file and returns an I/O action that will read that
    file (lazily, of course) and bind its contents to something as a string
-}


{-
Function to read the contents of (this case) girlfriend.txt

main = do  
    contents <- readFile "girlfriend.txt"  
    putStr contents 
-}


{-
writeFile has a type of writeFile :: FilePath -> String -> IO ().

It takes a path to a file and a string to write to that file and returns an I/O
    action that will do the writing. If such a file already exists, it will be stomped
    down to zero length before being written on.


import System.IO     
import Data.Char  
    
main = do     
    contents <- readFile "girlfriend.txt"     
    writeFile "girlfriendcaps.txt" (map toUpper contents)


$ runhaskell girlfriendtocaps.hs  
$ cat girlfriendcaps.txt  
HEY! HEY! YOU! YOU!  
I DON'T LIKE YOUR GIRLFRIEND!  
NO WAY! NO WAY!  
I THINK YOU NEED A NEW ONE!
-}


{-
appendFile has a type of appendFile :: FilePath -> String -> IO ().

appendFile is similar to writeFile but appendFile doesn't truncate the file to zero
    length if it already exists but it appends stuff to it.


import System.IO     

main = do     
    todoItem <- getLine  
    appendFile "todo.txt" (todoItem ++ "\n")


$ runhaskell appendtodo.hs  
Iron the dishes  
$ runhaskell appendtodo.hs  
Dust the dog  
$ runhaskell appendtodo.hs  
Take salad out of the oven  
$ cat todo.txt  
Iron the dishes  
Dust the dog  
Take salad out of the oven
-}


{-
This code is an example oh how files can though of as streams

main = do   
    withFile "something.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle  
        putStr contents) 
-}


{-
- Using hSetBuffering you are able to control exactly how buffering is done.
- It takes a handle and a BufferMode and returns an I/O action that sets the buffering
- BufferMode is a simple enumeration data type and the possible values it can hold are:
    NoBuffering, LineBuffering or BlockBuffering (Maybe Int).
- The Maybe Int is for how big the chunk should be, in bytes. If it's Nothing,
    then the operating system determines the chunk size.
- NoBuffering means that it will be read one character at a time, although NoBuffering
    isn't great as it has to access the disk a lot


main = do   
    withFile "something.txt" ReadMode (\handle -> do  
        hSetBuffering handle $ BlockBuffering (Just 2048)  
        contents <- hGetContents handle  
        putStr contents)
-}


-- We can also use hFlush, which is a function that takes a handle and returns an I/O action that will flush
    -- the buffer of the file associated with the handle.



{-

Program for removing an item from todo.txt



import System.IO  
import System.Directory  
import Data.List  
  
main = do        
    handle <- openFile "todo.txt" ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let todoTasks = lines contents     
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks     
    putStrLn "These are your TO-DO items:"  
    putStr $ unlines numberedTasks  
    putStrLn "Which one do you want to delete?"     
    numberString <- getLine     
    let number = read numberString
        -- delete and !! are from Data.list, !! returns an element from a list with
            -- an index of n, and delete removes the first occurance of it    
        newTodoItems = delete (todoTasks !! number) todoTasks     
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile "todo.txt"  
    renameFile tempName "todo.txt" 

-}



{-
            Command Line Arguments
-}


{-
- getArgs           getArgs :: IO [String]
- getProgName       getProgName :: IO String


import System.Environment   
import Data.List  
  
main = do  
   args <- getArgs
   progName <- getProgName
   putStrLn "The arguments are:"
   mapM putStrLn args
   putStrLn "The program name is:"
   putStrLn progName

-}



{-
Allows you to add, remove and view items from a list through a text file


Usage: todo (add, view, remove) (file).txt "text"           When compiled as todo
otherwise pass through Main




import System.Environment   
import System.Directory  
import System.IO  
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", add)
            , ("view", view)
            , ("remove", remove)
            ]

main = do  
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    action args

add :: [String] -> IO ()  
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()  
view [fileName] = do  
    contents <- readFile fileName  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()  
remove [fileName, numberString] = do  
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName

-}



{-
            Randomness
-}

-- The RandomGen typeclass is for types that can act as sources of randomness.
-- The Random typeclass is for things that can take on random values.
-- StdGen is an instance of the RandomGen typeclass.
-- mkStdGen, It takes an integer and based on that, gives us a random generator (manual random number generator)
-- random :: (RandomGen g, Random a) => g -> (a, g)

{-
Examples of usage:

ghci> random (mkStdGen 100)
-- (9216477508314497915,StdGen {unStdGen = SMGen 712633246999323047 2532601429470541125})
ghci> random (mkStdGen 100) :: (Int, StdGen)
-- (9216477508314497915,StdGen {unStdGen = SMGen 712633246999323047 2532601429470541125})
ghci> random (mkStdGen 949488) :: (Float, StdGen)
-- (0.28144592,StdGen {unStdGen = SMGen 2179256218976226416 4388660484137712901})
-}

threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen =   
    let (firstCoin, newGen) = random gen  
        (secondCoin, newGen') = random newGen  
        (thirdCoin, newGen'') = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin)

{-
ghci> threeCoins (mkStdGen 21)  
(True,True,True)  
ghci> threeCoins (mkStdGen 22)  
(True,False,True)  
ghci> threeCoins (mkStdGen 943)  
(True,False,True)  
ghci> threeCoins (mkStdGen 944)  
(True,True,True)  
-}

{-
ghci> take 5 $ randoms (mkStdGen 11) :: [Int]
[-1807975507,545074951,-1015194702,-1622477312,-502893664]
ghci> take 5 $ randoms (mkStdGen 11) :: [Bool]
[True,True,True,True,False]
ghci> take 5 $ randoms (mkStdGen 11) :: [Float]
[7.904789e-2,0.62691015,0.26363158,0.12223756,0.38291094]
-}


{-
-- Randoms returns a list and a new generator
random' :: (RandomGen g, Random a) => g -> [a]  
random' gen = let (value, newGen) = random gen in value:random' newGen

-- finiteRandoms returns a list of size n consisting of random numbers
finiteRandoms :: (RandomGen g, Random a, Num n) => n -> g -> ([a], g)  
finiteRandoms 0 gen = ([], gen)  
finiteRandoms n gen =   
    let (value, newGen) = random gen  
        (restOfList, finalGen) = finiteRandoms (n-1) newGen  
    in  (value:restOfList, finalGen) 
-}

-- randomR : Allows you to generate a random number between a given range.

{-
There also exists randomRs which produces a stream of random values within the defined reange.
ghci> take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]  
"ndkxbvmomg"  
-}


{-
Generates 20 random characterts between a and z

main = do  
    gen <- getStdGen  
    putStr $ take 20 (randomRs ('a','z') gen) 
-}

{-
By performing getStdGen twice, it will ask the system for the same variable,
    so the same string will be printed twice.

main = do  
    gen <- getStdGen  
    putStrLn $ take 20 (randomRs ('a','z') gen)  
    gen2 <- getStdGen  
    putStr $ take 20 (randomRs ('a','z') gen2)


To fix this we can generate a list double in size and split it when its halfway printed:
main = do  
    gen <- getStdGen  
    let randomChars = randomRs ('a','z') gen  
        (first20, rest) = splitAt 20 randomChars  
        (second20, _) = splitAt 20 rest  
    putStrLn first20  
    putStr second20

Another method is by using newStdGen which splits the number generator into two, then it
    updates the random number generator and encapsulates one with another keeping the 1 value
    in 1 and the other in the other.

main = do     
    gen <- getStdGen     
    putStrLn $ take 20 (randomRs ('a','z') gen)     
    gen' <- newStdGen  
    putStr $ take 20 (randomRs ('a','z') gen')    
-}


mainGame :: IO ()
mainGame = do  
    gen <- getStdGen  
    askForNumber gen  
  
askForNumber :: StdGen -> IO ()  
askForNumber gen = do  
    let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)  
    putStr "Which number in the range from 1 to 10 am I thinking of? "  
    numberString <- getLine  
    when (not $ null numberString) $ do  
        let number = read numberString  
        if randNumber == number   
            then putStrLn "You are correct!"  
            else putStrLn $ "Sorry, it was " ++ show randNumber  
        askForNumber newGen

--mainGame2 shows another way of writting the code above
mainGame2 :: IO ()
mainGame2 = do  
    gen <- getStdGen  
    let (randNumber, _) = randomR (1,10) gen :: (Int, StdGen)     
    putStr "Which number in the range from 1 to 10 am I thinking of? "  
    numberString <- getLine  
    when (not $ null numberString) $ do  
        let number = read numberString  
        if randNumber == number  
            then putStrLn "You are correct!"  
            else putStrLn $ "Sorry, it was " ++ show randNumber  
        newStdGen  
        mainGame2


{-
            ByteStrings
-}


{-
ghci> B.pack [99,97,110]  
Chunk "can" Empty  
ghci> B.pack [98..120]  
Chunk "bcdefghijklmnopqrstuvwx" Empty  
-}

{-
- unpack is the inverse function of pack. It takes a bytestring and turns it into a list of bytes
- fromChunks takes a list of strict bytestrings and converts it to a lazy bytestring.
- toChunks takes a lazy bytestring and converts it to a list of strict ones.
-}

{-
ghci> B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]  
Chunk "()*" (Chunk "+,-" (Chunk "./0" Empty))
-}

-- The bytestring version of : is called cons It takes a byte and a bytestring and
--    puts the byte at the beginning.
-- It's lazy though, so it will make a new chunk even if the first chunk in the
--    bytestring isn't full.

{-
ghci> B.cons 85 $ B.pack [80,81,82,84]  
Chunk "U" (Chunk "PQRT" Empty)  
ghci> B.cons' 85 $ B.pack [80,81,82,84]  
Chunk "UPQRT" Empty  
ghci> foldr B.cons B.empty [50..60]  
Chunk "2" (Chunk "3" (Chunk "4" (Chunk "5" (Chunk "6" (Chunk "7" (Chunk "8" (Chunk "9" (Chunk ":" (Chunk ";" (Chunk "<"  
Empty))))))))))  
ghci> foldr B.cons' B.empty [50..60]  
Chunk "23456789:;<" Empty 
-}

-- empty makes an empty bytestring.

{-
Difference between cons and cons' (with foldr):

With the foldr, we started with an empty bytestring and then went over the list
    of numbers from the right, adding each number to the beginning of the bytestring.
    When we used cons, we ended up with one chunk for every byte, which kind
    of defeats the purpose.
-}

{-
Otherwise, the bytestring modules have a load of functions that are analogous to
    those in Data.List, including, but not limited to, head, tail, init, null,
    length, map, reverse, foldl, foldr, concat, takeWhile, filter, etc.
-}

{-
readFile in System.IO/ByteString's Module:

For instance, the readFile function in System.IO has a type of
    readFile :: FilePath -> IO String, while the readFile from the bytestring modules
    has a type of readFile :: FilePath -> IO ByteString
-}

--import System.Environment  
--import qualified Data.ByteString.Lazy as B  

{-
Takes 2 files and copies the content from the first one onto the second file
-}
mainFileCopy :: IO ()
mainFileCopy = do  
    (fileName1:fileName2:_) <- getArgs  
    copyFile' fileName1 fileName2  
  
copyFile' :: FilePath -> FilePath -> IO ()  
copyFile' source dest = do  
    contents <- BSL.readFile source  
    BSL.writeFile dest contents



{-
            Exceptions
-}


{-
ghci> 4 `div` 0  
*** Exception: divide by zero  
ghci> head []  
*** Exception: Prelude.head: empty list
-}





{-
Tells you how many lines the program has


import System.Environment  
import System.IO  
  
main = do (fileName:_) <- getArgs  
          contents <- readFile fileName  
          putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!" 

error: ... (file).txt: openFile: does not exist (No such file or directory)  
-}

{-
- The Program tells you how many lines in a file


import System.Environment  
import System.IO  
import System.Directory  
  
main = do (fileName:_) <- getArgs
          fileExists <- doesFileExist fileName  
          if fileExists  
              then do contents <- readFile fileName  
                      putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
              else do putStrLn "The file doesn't exist!" 


As fileExists has the type :: fileExists <- doesFileExist fileName
    and doesFileExist has type doesFileExist :: FilePath -> IO Bool,
    which means that it returns an I/O action that has a boolean value as a
    result.
-}


{-
- IOError is an example of error handling so its similar to using try catch on
    languages like Java or Python
- The Program is meant to tell you how many lines the program has


import System.Environment  
import System.IO  
import System.IO.Error  
  
main = toTry `catch` handler  
              
toTry :: IO ()  
toTry = do (fileName:_) <- getArgs  
           contents <- readFile fileName  
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
  
handler :: IOError -> IO ()  
handler e = putStrLn "Whoops, had some trouble!" 
-}



{-
- The Program tells you how many lines a file has


import System.Environment  
import System.IO  
import System.IO.Error  
  
main = toTry `catch` handler  
              
toTry :: IO ()  
toTry = do (fileName:_) <- getArgs  
           contents <- readFile fileName  
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
  
handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
    | otherwise = ioError e  



isDoesNotExistError is a predicate over IOErrors, which means that it's a
    function that takes an IOError and returns a True or False

So we are using isDoesNotExistError to handle when the file does not exist
-}


{-
So the exception thrown in the toTry I/O action that we glued together with 
    a do block isn't caused by a file existing, toTry `catch` handler will
    catch that and then re-throw it. Pretty cool, huh?
-}


{-
IOError Predicates include:

- isAlreadyExistsError
- isDoesNotExistError
- isAlreadyInUseError
- isFullError
- isEOFError
- isIllegalOperation
- isPermissionError
- isUserError
-}


{-
isUserError evaluates to True when we use the function userError to make the
    exception, which is used for making exceptions from our code and equipping
    them with a string. For instance, you can do ioError $ userError
    "remote computer unplugged!"
-}


{-
A error handler could look something as following:

handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
    | isFullError e = freeSomeSpace      //IO Action
    | isIllegalOperation e = notifyCops  //IO Action
    | otherwise = ioError e 
-}

{-
ioeGetFileName :: IOError -> Maybe FilePath

Either returns (Maybe String) or Nothing, the String represents the FilePath which
    is extracted from the path from the IOError
-}


-- Modification of previous modification to print FilePath when it doesn't exist

{-
You don't have to use one handler to catch exceptions in your whole I/O part.
    You can just cover certain parts of your I/O code with catch or you can cover
    several of them with catch and use different handlers for them.

main = do toTry `catch` handler1  
          thenTryThis `catch` handler2  
          launchRockets  
-}

-- Haskell can offer something similar to IO (Either a b) which represents its
    -- Either Left a or Right b