{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
module Radix where

import Prelude hiding (and, or)

data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)
            deriving (Eq, Show)

type IntTree = Tree Int

data Bit = Zero | One
         deriving (Eq, Show)

type RadixTree = Tree Bit

type BitString = [Int]

--------------------------------------------------------------------------

buildIntTree :: [Int] -> IntTree
buildIntTree
  = foldr add Empty
  where
    add x Empty
      = Leaf x
    add x (Leaf y)
      = add x (Node y Empty Empty)
    add x t@(Node y l r)
      | x == y    = t
      | x < y     = Node y (add x l) r
      | otherwise = Node y l (add x r)

--------------------------------------------------------------------------

a, m :: Integer
m = 1073741824
a = 16387

rand :: Integer -> [Double]
rand s
  = fromInteger s / fromInteger m : rand s' where s' = (s * a) `mod` m

randomInts :: Int -> Int -> Integer -> [Int]
randomInts m n s
  = take m (map (round . (+1) . (* (fromIntegral n))) (rand s))

rs :: [Int]
rs = randomInts 1000 500 765539

--------------------------------------------------------------------------
-- Pre (universal): all integers are non-negative
empty :: RadixTree
empty
  = Leaf Zero

sizeIT :: IntTree -> Int
sizeIT Empty
  = 1
sizeIT (Leaf _)
  = 4
sizeIT (Node x lt rt)
  = 12 + sizeIT lt + sizeIT rt 

sizeRT :: RadixTree -> Int
sizeRT (Leaf _)
  = 1
sizeRT (Node x lt rt)
  = 8 + sizeRT lt + sizeRT rt

--
-- NOTE: The above import Prelude hiding (and, or) 
-- will allow you to name these two functions without
-- a name clash
--
and :: Bit -> Bit -> Bit
and Zero _
  = Zero
and One b
  = b

or :: Bit -> Bit -> Bit
or One _
  = One
or Zero b
  = b

binary :: Int -> BitString
binary 0
  = [0]
binary n
  = bin n []
  where
    bin 0 bs 
      = bs
    bin n bs 
      = bin (n `div` 2) (n `mod` 2 : bs)

insert :: BitString -> RadixTree -> RadixTree
insert [] (Leaf _)
  = Leaf One
insert [] (Node _ lt rt)
  = Node One lt rt
insert (b : bs) (Node x lt rt)
  | b == 0    = Node x (insert bs lt) rt
  | otherwise = Node x lt (insert bs rt)
insert bs (Leaf x)
  = insert bs (Node x empty empty)

buildRadixTree :: [Int] -> RadixTree
buildRadixTree
  = foldr (insert . binary) empty

member :: Int -> RadixTree -> Bool
member n rt
  = rt == insert (binary n) rt

union :: RadixTree -> RadixTree -> RadixTree
union (Leaf x) (Leaf y)
  = Leaf (x `or` y)
union (Leaf x) (Node y lt rt)
  = Node (x `or` y) lt rt
union (Node x lt rt) (Leaf y)
  = Node (x `or` y) lt rt
union (Node x lt rt) (Node y lt' rt')
  = Node (x `or` y) (union lt lt') (union rt rt')

intersection :: RadixTree -> RadixTree -> RadixTree
intersection (Leaf x) (Leaf y)
  = Leaf (x `and` y)
intersection (Leaf x) (Node y lt rt)
  = Leaf (x `and` y)
intersection (Node x lt rt) (Leaf y)
  = Leaf (x `and` y)
intersection (Node x lt rt) (Node y lt' rt')
  = Node (x `and` y) (intersection lt lt') (intersection rt rt')

economical :: Int -> Int
economical n
  | compareTrees (n - 1) /= compareTrees n = n
  | otherwise                              = economical (n + 1)

compareTrees :: Int -> Int
compareTrees n
  | sizeIT it > sizeRT rt = 1
  | otherwise             = 0
  where
    it = buildIntTree (take n rs)
    rt = buildRadixTree (take n rs)

-- CONCLUSION: The break-even point is n = 198.

-----------------------------------------------------------------------------
-- Some test trees...

figure :: RadixTree
figure
  = Node Zero (Leaf One)
               (Node One (Leaf Zero)
                          (Node One (Node Zero (Leaf One)
                                                 (Leaf Zero))
                                     (Leaf One)))

t1 :: IntTree
t1 = Node 20 (Node 8 Empty
                     (Node 12 Empty
                              Empty))
             Empty

t2 :: RadixTree
t2 = Node Zero (Node Zero (Leaf One)
                            (Node One (Leaf Zero) (Leaf One)))
                (Leaf One)
