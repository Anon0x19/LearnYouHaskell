module Chapter14 where
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


{- Zippers -}

{- Tree's using Zippers -}

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        )

data Direction = L | R deriving (Show)  
type Directions = [Direction]  

changeToP :: Directions-> Tree Char -> Tree Char  
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r  
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)  
changeToP [] (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a  
elemAt (L:ds) (Node _ l _) = elemAt ds l  
elemAt (R:ds) (Node _ _ r) = elemAt ds r  
elemAt [] (Node x _ _) = x

{-
ghci> let newTree = changeToP [R,L] freeTree  
ghci> elemAt [R,L] newTree  
'P' 
-}


{-
A direction list of [R] focuses on the sub-tree that's right of the root, for example.
    An empty direction list focuses on the main tree itself.
-}



{- A trail of breadcrumbs -}

-- Will be represented using L or R, this way we can leave a breadcrumb and leave some
    -- sort of track of where we have previously been (right or left node)

-- 
-- type Breadcrumbs = [Direction]

-- Allows you to move on the left or right of the tree and allows you to navigate
    -- through it
-- goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)  
-- goLeft (Node _ l _, bs) = (l, L:bs)

-- goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)  
-- goRight (Node _ _ r, bs) = (r, R:bs)

{-
ghci> goLeft (goRight (freeTree, []))
(Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])
-}

-- Allows us to clean our tree as we won't need to be currying the functions
x -: f = f x

{-
ghci> (freeTree, []) -: goRight -: goLeft
(Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty),[L,R])
-}

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs a = [Crumb a]


-- By modifying the goRight and goLeft function, we are able to store the previous
    -- states and see the previous paths taken down.

goLeft' :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft' (Node x l r, bs) = (l, LeftCrumb x r:bs)

goRight' :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight' (Node x l r, bs) = (r, RightCrumb x l:bs)

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)  
goUp (t, LeftCrumb x r:bs) = (Node x t r, bs) 
goUp (t, RightCrumb x l:bs) = (Node x l t, bs)


{-
With a pair of Tree a and Breadcrumbs a, we have enough information to reduild the
    tree.
-}

type Zipper a = (Tree a, Breadcrumbs a)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

{-
- Now we can start off with a tree, move to anywhere we want and modify an element, all
    while keeping focus on that element so that we can easily move further up or down.

ghci> let newFocus = modify (\_ -> 'P') (goRight (goLeft (freeTree,[])))
ghci> let newFocus = (freeTree,[]) -: goLeft -: goRight -: modify (\_ -> 'P')


- We go left, then right and then modify the root element by replacing it with a 'P'.
    This reads even better if we use -::

ghci> let newFocus2 = modify (\_ -> 'X') (goUp newFocus)
ghci> let newFocus2 = newFocus -: goUp -: modify (\_ -> 'X')

Each node has two sub-trees, even if those sub-trees are empty trees. So if we're
    focusing on an empty sub-tree, one thing we can do is to replace it with a non-empty
    subtree, thus attaching a tree to a leaf node. The code for this is simple:
-}
attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

{-
ghci> let farLeft = (freeTree,[]) -: goLeft -: goLeft -: goLeft -: goLeft
ghci> let newFocus = farLeft -: attach (Node 'Z' Empty Empty)
-}

{-
newFocus is now focused on the tree that we just attached and the rest of the tree lies
    inverted in the breadcrumbs.
-}

{-
Making a function that walks all the way to the top of the tree, regardless of what we're focusing on, is really easy. Here it is:
-}
topMost :: Zipper a -> Zipper a  
topMost (t,[]) = (t,[])  
topMost z = topMost (goUp z)




{- Focusing on lists -}

{-
Zippers can be used with pretty much any data structure, so its no suprise that they
    case be used to focus on sub-lists of ists. After all lists are like trees, only
    that where a node in a tree has an element (or not) and several sub-trees, a node
    in a list only has a single sub-list.

The List type is defined by:
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-}

type ListZipper a = ([a],[a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)
  
goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)

{-
ghci> let xs = [1,2,3,4]  
ghci> goForward (xs,[])  
([2,3,4],[1])  
ghci> goForward ([2,3,4],[1])  
([3,4],[2,1])  
ghci> goForward ([3,4],[2,1])  
([4],[3,2,1])  
ghci> goBack ([4],[3,2,1])  
([3,4],[2,1])

We can see here that breadcrumbs in the case of a list is nothing more but a reversed
    part of our list. The element that we move away from always goes into the head of
    the breadcrumbs and making it the head of our focus.

- Note: gives more context to why zippers are called zippers, as it looks like a zipper
    moving up and down.
-}



{- A very simple file system -}

{-

-}

type Name = String  
type Data = String  
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

{-
Example of file system

myDisk :: FSItem  
myDisk = 
    Folder "root"   
        [ File "goat_yelling_like_man.wmv" "baaaaaa"  
        , File "pope_time.avi" "god bless"  
        , Folder "pics"  
            [ File "ape_throwing_up.jpg" "bleargh"  
            , File "watermelon_smash.gif" "smash!!"  
            , File "skull_man(scary).bmp" "Yikes!"  
            ]  
        , File "dijon_poupon.doc" "best mustard"  
        , Folder "programs"  
            [ File "fartwizard.exe" "10gotofart"  
            , File "owl_bandit.dmg" "mov eax, h00t"  
            , File "not_a_virus.exe" "really not a virus"  
            , Folder "source code"  
                [ File "best_hs_prog.hs" "main = print (fix error)"  
                , File "random.hs" "main = print 4"  
                ]  
            ]  
        ]
-}

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSZipper -> FSZipper  
fsTo name (Folder folderName items, bs) =   
    let (ls, item:rs) = break (nameIs name) items  
    in  (item, FSCrumb folderName ls rs:bs)  
  
nameIs :: Name -> FSItem -> Bool  
nameIs name (Folder folderName _) = name == folderName  
nameIs name (File fileName _) = name == fileName


{-
ghci> let newFocus = (myDisk,[]) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"

-- newFocus is a zipper thats focused on skull_man...
ghci> fst newFocus
File "skull_man(scary).bmp" "Yikes!"

-- By moving up we are able to focus on a different neighboring file
ghci> let newFocus2 = newFocus -: fsUp -: fsTo "watermelon_smash.gif"  
ghci> fst newFocus2  
File "watermelon_smash.gif" "smash!!
-}



{- Manipulating our file system -}
-- Now that we know how to navigate through the file system, the following function
    -- renames the currently focused file or folder:

fsRename :: Name -> FSZipper -> FSZipper  
fsRename newName (Folder name items, bs) = (Folder newName items, bs)  
fsRename newName (File name dat, bs) = (File newName dat, bs)

{-
Let's add a file to our "pics" folder and then move back up the root:
ghci> let newFocus = (myDisk,[]) -: fsTo "pics" -: fsNewFile (File "heh.jpg" "lol") -: fsUp  
-}


{-
The goLeft function takes a zipper of a binary tree and moves the focus to its
    left sub-tree:
goLeft :: Zipper a -> Zipper a  
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)
-}

{-
So let's use the Maybe monad to add a context of possible failure to our movements.
We're going to take the functions that work on our binary tree zipper and we're going
    to make them into monadic functions.

The failure of functions that could fail was always reflected in their result, and this
    time is no different. So here are goLeft and goRight with an added possibility of
    failure:
-}

goLeft'' :: Zipper a -> Maybe (Zipper a)  
goLeft'' (Node x l r, bs) = Just (l, LeftCrumb x r:bs)  
goLeft'' (Empty, _) = Nothing  
  
goRight'' :: Zipper a -> Maybe (Zipper a)  
goRight'' (Node x l r, bs) = Just (r, RightCrumb x l:bs)  
goRight'' (Empty, _) = Nothing

{-
Cool, now if we try to take a step to the left of an empty tree, we get a Nothing!

ghci> goLeft (Empty, [])  
Nothing  
ghci> goLeft (Node 'A' Empty Empty, [])  
Just (Empty,[LeftCrumb 'A' Empty])
-}


{-
So then after modifying goLeft and goRight we also need to modify goUp
-}
goUp'' :: Zipper a -> Maybe (Zipper a)  
goUp'' (t, LeftCrumb x r:bs) = Just (Node x t r, bs)  
goUp'' (t, RightCrumb x l:bs) = Just (Node x l t, bs)  
goUp'' (_, []) = Nothing

{-
gchi> let newFocus = (freeTree,[]) -: goLeft -: goRight
-}


{-
But now, instead of returning Zipper a, they return Maybe (Zipper a), so chaining
    functions like this won't work. So we have to implement bind (>>=). Using bind
    handles the context of the Type:

ghci> let coolTree = Node 1 Empty (Node 3 Empty Empty)  
ghci> return (coolTree,[]) >>= goRight  
Just (Node 3 Empty Empty,[RightCrumb 1 Empty])  
ghci> return (coolTree,[]) >>= goRight >>= goRight  
Just (Empty,[RightCrumb 3 Empty,RightCrumb 1 Empty])  
ghci> return (coolTree,[]) >>= goRight >>= goRight >>= goRight  
Nothing

We used return to put a zipper in a Just and then used >>= to feed that to our goRight
    function.
-}