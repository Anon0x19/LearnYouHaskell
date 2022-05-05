import Data.List    
import qualified Distribution.InstalledPackageInfo as Cube

{-
usage: numUniques[] - nub is a function in Data.List that removes
    duplicates. comosing length and nub by doing length . nub has the
    equivalence of \xs -> length (nub xs)
-}
numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub


{-
by using :m + (module) you are able to load modules onto GHCI
ghci> :m + Data.List
- Multiple modules imported
ghci> :m + Data.List Data.Map Data.Set
- To import specific functions (i.e nub and sort from Data.List)
import Data.List (nub, sort) 
- To import all functions except (i.e nub)
import Data.List hiding (nub)
- To prevent name clashes we can use a qualified import which looks up to values using keys
import qualified Data.Map
- We rename the function Data.Map to M
import qualified Data.Map as M
-}


{-
          --intersperse--
-----------------------------------
-- Places a . Between every element
intersperse '.' "MONKEY"
"M.O.N.K.E.Y"
-----------------------------------
-- Places a 0 between every element
intersperse 0 [1,2,3,4,5,6]
[1,0,2,0,3,0,4,0,5,0,6]

          --intercalate--
-----------------------------------
-- Flattens the list and passes the first list of list onto the second one
intercalate " " ["hey","there","guys"]
"hey there guys"  
-----------------------------------
-- Flattens the list and passes the first list of list onto the second one
intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]
[1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]

          --transpose--
-----------------------------------
-- Transposes a list of list as if it was 2D so rows become cols and viceversa
transpose [[1,2,3],[4,5,6],[7,8,9]]
[[1,4,7],[2,5,8],[3,6,9]]
-----------------------------------
-- Transposes a list of list as if it was 2D so rows become cols and viceversa
transpose ["hey","there","guys"]
["htg","ehu","yey","rs","e"]
-----------------------------------
-- Say we have the polynomials 3xˆ2 + 5x + 9, 10xˆ3 + 9 and 8xˆ3 + 5xˆ2 + x - 1
    and we want to add them together. We can use the lists [0,3,5,9], [10,0,0,9]
    and [8,5,1,-1] to represent them
map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]
[18,8,6,17] 


          --concat--
-----------------------------------
-- Flattens a list and concatenates the values
concat ["foo","bar","car"]  
"foobarcar"
-----------------------------------
-- Flattens a list and concatenates the values
concat [[3,4,5],[2,3,4],[2,1,1]]  
[3,4,5,2,3,4,2,1,1]

          --concatMap--
-----------------------------------
-- same thing as firstly mapping the function and then concatenating the lists
concatMap (replicate 4) [1..3]  
[1,1,1,1,2,2,2,2,3,3,3,3]
-----------------------------------
-- same thing as firstly mapping the function and then concatenating the lists
concat [[3,4,5],[2,3,4],[2,1,1]]  
[3,4,5,2,3,4,2,1,1]

          --and--
-----------------------------------
-- Takes a list of boolean values and returns True only if all the values in the list are all True
and $ map (>4) [5,6,7,8]  
True
-----------------------------------
-- Takes a list of boolean values and returns True only if all the values in the list are all True
and $ map (==4) [4,4,4,3,4]  
False

          --or--
---------------------------------
-- Takes a list of boolean values and returns True if any values on the list are True
or $ map (==4) [2,3,4,5,6,1]  
True
---------------------------------
-- Takes a list of boolean values and returns True if any values on the list are True
or $ map (>4) [1,2,3]  
False

        --any & all--
---------------------------------
-- Any and all take a predicate and then check if any or all the elements in a list satisfy the predicate,
    respectively. Usually we use these two functions instead of mapping over a list and then doing and or or
---------------------------------
ghci> any (==4) [2,3,5,6,1,4]
True
ghci> all (>4) [6,9,10]
True
ghci> all (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
False
ghci> any (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
True

          --iterate--
---------------------------------
-- Takes a function and a starting value. It applies the function to the starting value, then it applies
    that function to the result, then it applies the function to that result again, etc. It returns all the results in the
    form of an infinite list
---------------------------------
ghci> take 10 $ iterate (*2) 1  
[1,2,4,8,16,32,64,128,256,512]  
ghci> take 3 $ iterate (++ "haha") "haha"  
["haha","hahahaha","hahahahahaha"]

          --splitAt--
---------------------------------
-- Takes a number and a list. It then splits the list at that many elements, returning the resulting two lists in a tuple
---------------------------------
ghci> splitAt 3 "heyman"  
("hey","man")  
ghci> splitAt 100 "heyman"  
("heyman","")  
ghci> splitAt (-3) "heyman"  
("","heyman")  
ghci> let (a,b) = splitAt 3 "foobar" in b ++ a  
"barfoo"

          --takeWhile--
---------------------------------
-- Takes elements from a list while the predicate holds and then when an element is encountered that doesn't satisfy the predicate,
    it's cut off. It turns out this is very useful
---------------------------------
ghci> takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]  
[6,5,4]  
ghci> takeWhile (/=' ') "This is a sentence"  
"This"
---------------------------------
ghci> sum $ takeWhile (<10000) $ map (^3) [1..]  
53361

          --dropWhile--
---------------------------------
-- Takes a list and drops all elements that meet a condition
---------------------------------
ghci> dropWhile (/=' ') "This is a sentence"  
" is a sentence"
ghci> dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]  
[3,4,5,4,3,2,1]
---------------------------------
ghci> let stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)] 
ghci> head (dropWhile (\(val,y,m,d) -> val < 1000) stock)  
(1001.4,2008,9,4)

          --span--
---------------------------------
-- Returns a pair of lists. The first list contains everything resulting list 
---------------------------------
ghci> let (fw, rest) = span (/=' ') "This is a sentence" in "First word:" ++ fw ++ ", the rest:" ++ rest  
"First word: This, the rest: is a sentence"
---------------------------------
ghci> break (==4) [1,2,3,4,5,6,7]  
([1,2,3],[4,5,6,7])
ghci> span (/=4) [1,2,3,4,5,6,7]  
([1,2,3],[4,5,6,7])

          --sort--
---------------------------------
-- Returns the list sorted. The type of elements in the list has to be part of the Ord typeclass.
---------------------------------
ghci> sort [8,5,3,2,1,6,4,2]
[1,2,2,3,4,5,6,8]
ghci> sort "This will be sorted soon"
"    Tbdeehiillnooorssstw"

          --group--
---------------------------------
-- Takes a list and groups adjacents elements into sublists if they are equal
---------------------------------
ghci> group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
[[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]
ghci> map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
[(1,4),(2,7),(3,2),(5,1),(6,1),(7,1)]
---------------------------------

      --inits & tails--
---------------------------------
-- inits and tails are like init and tail, only they recursively apply that to a list until there's nothing left.
---------------------------------
ghci> inits "w00t"
["","w","w0","w00","w00t"]
ghci> tails "w00t"
["w00t","00t","0t","t",""]
ghci> let w = "w00t" in zip (inits w) (tails w)
[("","w00t"),("w","00t"),("w0","0t"),("w00","t"),("w00t","")]
---------------------------------
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

      --isInfixOf--
---------------------------------
-- Searches for a sublist within a list and returns True if the sublist we are looking for is somewhere inside the target area
---------------------------------
ghci> "cat" `isInfixOf` "im a cat burglar"  
True  
ghci> "Cat" `isInfixOf` "im a cat burglar"  
False  
ghci> "cats" `isInfixOf` "im a cat burglar"  
False

  --isPrefixOf & isSuffixOf--
---------------------------------
-- Searches for a sublist within a list and returns True if the sublist we are looking for is somewhere inside the target area
---------------------------------
ghci> "hey" `isPrefixOf` "hey there!"
True
ghci> "hey" `isPrefixOf` "oh hey there!"
False
ghci> "there!" `isSuffixOf` "oh hey there!"
True
ghci> "there!" `isSuffixOf` "oh hey there"
False

-- elem and notElem check if an element is or isn't inside a list
-- partition takes a list and a predicate and returns a pairs of lists.
    The first list in the result contains all the elements that satisfy the and the second one are the ones that don't
---------------------------------
ghci> partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"  
("BOBMORGAN","sidneyeddy")  
ghci> partition (>3) [1,3,5,6,3,2,1,0,3,7]  
([5,6,7],[1,3,3,2,1,0,3])
---------------------------------
ghci> span (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"
("BOB","sidneyMORGANeddy")

          --find--
---------------------------------
-- Takes a list and returns the first element that satisfies the operation
---------------------------------
ghci> find (>4) [1,2,3,4,5,6]  
Just 5  
ghci> find (>9) [1,2,3,4,5,6]  
Nothing  
ghci> :t find  
find :: (a -> Bool) -> [a] -> Maybe a

        --elemIndex--
---------------------------------
-- elemIndex is kind of like elem, only it doesn't return a boolean value.
    It maybe returns the index of the element we're looking for. If that element isn't in our list, it returns a Nothing
---------------------------------
ghci> :t elemIndex  
elemIndex :: (Eq a) => a -> [a] -> Maybe Int  
ghci> 4 `elemIndex` [1,2,3,4,5,6]  
Just 3  
ghci> 10 `elemIndex` [1,2,3,4,5,6]  
Nothing

      --elemIndices--
---------------------------------
-- elemIndices is like elemIndex only it returns a list of indices
---------------------------------
ghci> ' ' `elemIndices` "Where are the spaces?"  
[5,9,13]

      --findIndex--
---------------------------------
-- findIndex is like find, but it maybe returns the index of the first element that satisfies the predicate.
-- findIndices returns the indices of all elements that satisfy the predicate in the form of a list.
---------------------------------
ghci> findIndex (==4) [5,3,2,1,6,4]  
Just 5  
ghci> findIndex (==7) [5,3,2,1,6,4]  
Nothing  
ghci> findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"  
[0,6,10,14] 

--zip3/zip4 & zipWith3/zipWith4--
---------------------------------
-- The variants of zip go up to 7 elements but takes the lists and puts all elements
    with the same index into a tuple and then puts all the tuples into a list.
---------------------------------
ghci> zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]  
[7,9,8]  
ghci> zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]  
[(2,2,5,2),(3,2,5,2),(3,2,3,2)]

          --lines--
---------------------------------
-- lines is a useful function when dealing with files or input from somewhere.
    It takes a string and returns every line of that string in a separate list
---------------------------------
ghci> lines "first line\nsecond line\nthird line"
["first line","second line","third line"]

          --unlines--
---------------------------------
-- unlines is the inverse function of lines.
    It takes a list of strings and joins them together using a '\n'.
---------------------------------
ghci> unlines ["first line", "second line", "third line"]  
"first line\nsecond line\nthird line\n"

      --words & unwords--
---------------------------------
-- words and unwords are for splitting a line of text into words or joining a list of words into a text.
---------------------------------
ghci> words "hey these are the words in this sentence"  
["hey","these","are","the","words","in","this","sentence"]  
ghci> words "hey these           are    the words in this\nsentence"  
["hey","these","are","the","words","in","this","sentence"]  
ghci> unwords ["hey","there","mate"]  
"hey there mate"

          --nub--
---------------------------------
-- It takes a list and weeds out the duplicate elements, returning a list whose every element is a unique snowflake!
    The function does have a kind of strange name. It turns out that "nub" means a small lump or essential part of something.
---------------------------------
ghci> nub [1,2,3,4,3,2,1,2,3,4,3,2,1]  
[1,2,3,4]  
ghci> nub "Lots of words and stuff"  
"Lots fwrdanu"

         --delete--
---------------------------------
\\ is the list difference function. It acts like a set difference, basically.
    For every element in the right-hand list, it removes a matching element in the left one.
---------------------------------
ghci> delete 'h' "hey there ghang!"  
"ey there ghang!"  
ghci> delete 'h' . delete 'h' $ "hey there ghang!"  
"ey tere ghang!"  
ghci> delete 'h' . delete 'h' . delete 'h' $ "hey there ghang!"  
"ey tere gang!"

         --union--
---------------------------------
-- union also acts like a function on sets. It returns the union of two lists.
    It pretty much goes over every element in the second list and appends it to the first one if it isn't already in yet.
---------------------------------
ghci> "hey man" `union` "man what's up"  
"hey manwt'sup"  
ghci> [1..7] `union` [5..10]  
[1,2,3,4,5,6,7,8,9,10]

        --intersect--
---------------------------------
-- intersect works like set intersection. It returns only the elements that are found in both lists.
---------------------------------
ghci> [1..7] `intersect` [5..10]  
[5,6,7]

        --insert--
---------------------------------
-- insert takes an element and a list of elements that can be sorted and inserts it into the last position
    where it's still less than or equal to the next element.
---------------------------------
ghci> insert 4 [3,5,1,2,8,2]  
[3,4,5,1,2,8,2]  
ghci> insert 4 [1,3,4,4,1]  
[1,3,4,4,4,1]

ghci> insert 4 [1,2,3,5,6,7]  
[1,2,3,4,5,6,7]  
ghci> insert 'g' $ ['a'..'f'] ++ ['h'..'z']  
"abcdefghijklmnopqrstuvwxyz"  
ghci> insert 3 [1,2,4,3,2,1]  
[1,2,3,4,3,2,1]


Data.List has the more generic equivalent of length, take drop and splitAt which are
genericLength, genericTake, genericDrop, genericSplitAt, genericIndex and genericReplicate

The nub, delete, union, intersect and group functions all have their more general counterparts called nubBy, deleteBy, unionBy, intersectBy and groupBy


        --groupBy--
---------------------------------
-- The equality function supplied to the By functions should take two elements of the same type and return True if it considers them equal by its standards.
    Then its ordered in the list based on that.
---------------------------------
ghci> let values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]  
ghci> groupBy (\x y -> (x > 0) == (y > 0)) values  
[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]

          --on--
---------------------------------
-- Definition for Data.Function `on`
---------------------------------
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c  
f `on` g = \x y -> f (g x) (g y)

ghci> groupBy ((==) `on` (> 0)) values  
[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]


--sortBy, insertBy, maximumBy & minimumBy--
---------------------------------
-- sortBy, insertBy, maximumBy and minimumBy take a function that determine if one element is greater,
    smaller or equal to the other
---------------------------------
ghci> let xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]  
ghci> sortBy (compare `on` length) xs  
[[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]]

}----------------------------------------------------------------------{

                                Data.Char

'isControl' checks whether a character is a control character.

'isSpace' checks whether a character is a white-space characters. That includes spaces, tab characters, newlines, etc.

'isLower' checks whether a character is lower-cased.

'isUpper' checks whether a character is upper-cased.

'isAlpha' checks whether a character is a letter.

'isAlphaNum' checks whether a character is a letter or a number.

'isPrint' checks whether a character is printable. Control characters, for instance, are not printable.

'isDigit' checks whether a character is a digit.

'isOctDigit' checks whether a character is an octal digit.

'isHexDigit' checks whether a character is a hex digit.

'isLetter' checks whether a character is a letter.

'isMark' checks for Unicode mark characters. Those are characters that combine with preceding letters to form latters with accents. Use this if you are French.

'isNumber' checks whether a character is numeric.

'isPunctuation' checks whether a character is punctuation.

'isSymbol' checks whether a character is a fancy mathematical or currency symbol.

'isSeparator' checks for Unicode spaces and separators.

'isAscii' checks whether a character falls into the first 128 characters of the Unicode character set.

'isLatin1' checks whether a character falls into the first 256 characters of Unicode.

'isAsciiUpper' checks whether a character is ASCII and upper-case.

'isAsciiLower' checks whether a character is ASCII and lower-case.



ghci> words "hey guys its me"  
["hey","guys","its","me"]  
ghci> groupBy ((==) `on` isSpace) "hey guys its me"  
["hey"," ","guys"," ","its"," ","me"]

ghci> filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me"  
["hey","guys","its","me"]

ghci> generalCategory ' '  
Space  
ghci> generalCategory 'A'  
UppercaseLetter



toUpper converts a character to upper-case. Spaces, numbers, and the like remain unchanged.

toLower converts a character to lower-case.

toTitle converts a character to title-case. For most characters, title-case is the same as upper-case.

digitToInt converts a character to an Int. To succeed, the character must be in the ranges '0'..'9', 'a'..'f' or 'A'..'F'.

intToDigit is the inverse function of digitToInt. It takes an Int in the range of 0..15 and converts it to a lower-case character.

The ord and chr functions convert characters to their corresponding numbers and vice versa
ghci> ord 'a'  
97  
ghci> chr 97  
'a'  
ghci> map ord "abcdefgh"  
[97,98,99,100,101,102,103,104] 


--Caesar cipher implementation--
encode :: Int -> String -> String  
encode shift msg = 
    let ords = map ord msg  
        shifted = map (+ shift) ords  
    in  map chr shifted

ghci> encode 3 "Heeeeey"  
"Khhhhh|"  
ghci> encode 4 "Heeeeey"  
"Liiiii}"  
ghci> encode 1 "abcd"  
"bcde"  
ghci> encode 5 "Marry Christmas! Ho ho ho!"  
"Rfww~%Hmwnxyrfx&%Mt%mt%mt&"

decode :: Int -> String -> String  
decode shift msg = encode (negate shift) msg

ghci> encode 3 "Im a little teapot"  
"Lp#d#olwwoh#whdsrw"  
ghci> decode 3 "Lp#d#olwwoh#whdsrw"  
"Im a little teapot"  
ghci> decode 5 . encode 5 $ "This is a sentence"  
"This is a sentence"

}----------------------------------------------------------------------{

                                Data.Map

import qualified Data.Map as Map

phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ]

findKey :: (Eq k) => k -> [(k,v)] -> v  
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey key [] = Nothing  
findKey key ((k,v):xs) = if key == k  
                            then Just v  
                            else findKey key xs

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

ghci> findKey "penny" phoneBook  
Just "853-2492"  
ghci> findKey "betty" phoneBook  
Just "555-2938"  
ghci> findKey "wilma" phoneBook  
Nothing

ghci> Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]  
fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]  
ghci> Map.fromList [(1,2),(3,4),(3,2),(5,5)]  
fromList [(1,2),(3,2),(5,5)]

Map.fromList :: (Ord k) => [(k, v)] -> Map.Map k v

ghci> Map.empty  
fromList []  
ghci> Map.insert 3 100 Map.empty  
fromList [(3,100)]  
ghci> Map.insert 5 600 (Map.insert 4 200 ( Map.insert 3 100  Map.empty))  
fromList [(3,100),(4,200),(5,600)]  
ghci> Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty  
fromList [(3,100),(4,200),(5,600)]

fromList' :: (Ord k) => [(k,v)] -> Map.Map k v  
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

ghci> Map.null Map.empty  
True  
ghci> Map.null $ Map.fromList [(2,3),(5,5)]  
False

ghci> Map.size Map.empty  
0  
ghci> Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)]  
5

ghci> Map.singleton 3 9  
fromList [(3,9)]  
ghci> Map.insert 5 9 $ Map.singleton 3 9  
fromList [(3,9),(5,9)]

lookup works like the Data.List lookup, only it operates on maps.
    It returns Just something if it finds something for the key and Nothing if it doesn't.

member is a predicate takes a key and a map and reports whether the key is in the map or not.
ghci> Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)]  
True  
ghci> Map.member 3 $ Map.fromList [(2,5),(4,5)]  
False

map and filter work much like their list equivalents.
ghci> Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)]  
fromList [(1,100),(2,400),(3,900)]  
ghci> Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]  
fromList [(2,'A'),(4,'B')]

ghci> Map.toList . Map.insert 9 2 $ Map.singleton 4 3  
[(4,3),(9,2)]

keys and elems return lists of keys and values respectively. keys is the equivalent of map fst . Map.toList and elems is the equivalent of map snd . Map.toList.

fromListWith is a cool little function. It acts like fromList, only it doesn't discard duplicate keys but it uses a function supplied to it to decide what to do with them.
    Let's say that a girl can have several numbers and we have an association list set up like this

phoneBook =   
    [("betty","555-2938")  
    ,("betty","342-2492")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("patsy","943-2929")  
    ,("patsy","827-9162")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ,("penny","555-2111")  
    ]

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String  
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs

ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook  
"827-9162, 943-2929, 493-2928"  
ghci> Map.lookup "wendy" $ phoneBookToMap phoneBook  
"939-8282"  
ghci> Map.lookup "betty" $ phoneBookToMap phoneBook  
"342-2492, 555-2938"

phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]  
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs

ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook  
["827-9162","943-2929","493-2928"]

ghci> Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]  
fromList [(2,100),(3,29),(4,22)]

ghci> Map.fromListWith (+) [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]  
fromList [(2,108),(3,62),(4,37)]

insertWith is to insert what fromListWith is to fromList. It inserts a key-value pair into a map, but if that map already contains the key,
    it uses the function passed to it to determine what to do.
ghci> Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]  
fromList [(3,104),(5,103),(6,339)]


}----------------------------------------------------------------------{
                                Data.Set

import qualified Data.Set as Set 

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"  
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"

ghci> let set1 = Set.fromList text1  
ghci> let set2 = Set.fromList text2  
ghci> set1  
fromList " .?AIRadefhijlmnorstuy"  
ghci> set2  
fromList " !Tabcdefghilmnorstuvwy

ghci> Set.intersection set1 set2  
fromList " adefhilmnorstuy"

ghci> Set.difference set1 set2  
fromList ".?AIRj"  
ghci> Set.difference set2 set1  
fromList "!Tbcgvw

ghci> Set.union set1 set2  
fromList " !.?AIRTabcdefghijlmnorstuvwy"

ghci> Set.null Set.empty  
True  
ghci> Set.null $ Set.fromList [3,4,5,5,4,3]  
False  
ghci> Set.size $ Set.fromList [3,4,5,3,4,5]  
3  
ghci> Set.singleton 9  
fromList [9]  
ghci> Set.insert 4 $ Set.fromList [9,3,8,1]  
fromList [1,3,4,8,9]  
ghci> Set.insert 8 $ Set.fromList [5..10]  
fromList [5,6,7,8,9,10]  
ghci> Set.delete 4 $ Set.fromList [3,4,5,4,3,4,5]  
fromList [3,5]

ghci> Set.fromList [2,3,4] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]  
True  
ghci> Set.fromList [1,2,3,4,5] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]  
True  
ghci> Set.fromList [1,2,3,4,5] `Set.isProperSubsetOf` Set.fromList [1,2,3,4,5]  
False  
ghci> Set.fromList [2,3,4,8] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]  
False

ghci> Set.filter odd $ Set.fromList [3,4,5,6,7,2,3,4]  
fromList [3,5,7]  
ghci> Set.map (+1) $ Set.fromList [3,4,5,6,7,2,3,4]  
fromList [3,4,5,6,7,8]

Sets are often used to weed a list of duplicates from a list by first making it into a set with fromList and then converting it back to a list with toList.
he Data.List function nub already does that, but weeding out duplicates for large lists is much faster if you cram them into a set and then convert them back to a list than using nub.
    But using nub only requires the type of the list's elements to be part of the Eq typeclass, whereas if you want to cram elements into a set, the type of the list has to be in Ord.
ghci> let setNub xs = Set.toList $ Set.fromList xs  
ghci> setNub "HEY WHATS CRACKALACKIN"  
" ACEHIKLNRSTWY"  
ghci> nub "HEY WHATS CRACKALACKIN"  
"HEY WATSCRKLIN"



}----------------------------------------------------------------------{
                        Making your own module


module Geometry  
( sphereVolume  
, sphereArea  
, cubeVolume  
, cubeArea  
, cuboidArea  
, cuboidVolume  
) where
sphereVolume :: Float -> Float  
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)  
  
sphereArea :: Float -> Float  
sphereArea radius = 4 * pi * (radius ^ 2)  
  
cubeVolume :: Float -> Float  
cubeVolume side = cuboidVolume side side side  
  
cubeArea :: Float -> Float  
cubeArea side = cuboidArea side side side  
  
cuboidVolume :: Float -> Float -> Float -> Float  
cuboidVolume a b c = rectangleArea a b * c  
  
cuboidArea :: Float -> Float -> Float -> Float  
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  
  
rectangleArea :: Float -> Float -> Float  
rectangleArea a b = a * b

'import Geometry'

Sphere.hs

module Geometry.Sphere  
( volume  
, area  
) where  
  
volume :: Float -> Float  
volume radius = (4.0 / 3.0) * pi * (radius ^ 3)  
  
area :: Float -> Float  
area radius = 4 * pi * (radius ^ 2)


Cuboid.hs

module Geometry.Cuboid  
( volume  
, area  
) where  
  
volume :: Float -> Float -> Float -> Float  
volume a b c = rectangleArea a b * c  
  
area :: Float -> Float -> Float -> Float  
area a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  
  
rectangleArea :: Float -> Float -> Float  
rectangleArea a b = a * b


Cube.hs

module Geometry.Cube  
( volume  
, area  
) where  
  
import qualified Geometry.Cuboid as Cuboid  
  
volume :: Float -> Float  
volume side = Cuboid.volume side side side  
  
area :: Float -> Float  
area side = Cuboid.area side side side


import Geometry.Sphere

import qualified Geometry.Sphere as Sphere  
import qualified Geometry.Cuboid as Cuboid  
import qualified Geometry.Cube as Cube  
-}