module Tries where

import Data.List hiding (insert)
import Data.Bits

import Types
import HashFunctions
import Examples

--------------------------------------------------------------------
-- Part I

-- Use this if you're counting the number of 1s in every
-- four-bit block...
bitTable :: [Int]
bitTable
  = [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4]

countOnes :: Int -> Int
countOnes 0
  = 0
countOnes n
  = bitTable !! (n `mod` 16) + countOnes (n `div` 16)

countOnesFrom :: Int -> Int -> Int
countOnesFrom i n
  = popCount (n .&. (bit i - 1))

getIndex :: Int -> Int -> Int -> Int
getIndex n i size
  = shiftR n (i * size) .&. (bit size - 1)

-- Pre: the index is less than the length of the list
replace :: Int -> [a] -> a -> [a]
replace 0 (x : xs) x'
  = x' : xs
replace i (x : xs) x'
  = x : replace (i - 1) xs x'

-- Pre: the index is less than or equal to the length of the list
insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x' xs
  = x' : xs
insertAt i x' (x : xs)
  = x : insertAt (i - 1) x' xs

--------------------------------------------------------------------
-- Part II

sumTrie :: (Int -> Int) -> ([Int] -> Int) -> Trie -> Int
sumTrie _ g (Leaf ns)
  = g ns
sumTrie f g (Node _ subs)
  = sum (map sumSubNodes subs)
  where
    sumSubNodes :: SubNode -> Int
    sumSubNodes (Term n)
      = f n
    sumSubNodes (SubTrie t)
      = sumTrie f g t

{-
--
-- If you get the sumTrie function above working you can uncomment
-- these three functions; they may be useful in testing.
--
trieSize :: Trie -> Int
trieSize t
  = sumTrie (const 1) length t

binCount :: Trie -> Int
binCount t
  = sumTrie (const 1) (const 1) t

meanBinSize :: Trie -> Double
meanBinSize t
  = fromIntegral (trieSize t) / fromIntegral (binCount t)
-}

member :: Int -> Hash -> Trie -> Int -> Bool
member n h t size
  = member' t 0
  where
    member' :: Trie -> Int -> Bool
    member' (Leaf ns) 2
      = n `elem` ns 
    member' (Node bv subs) lvl
      = testBit bv i && memberOfSub (subs !! countOnesFrom i bv)
      where
        i = getIndex h lvl size

        memberOfSub :: SubNode -> Bool
        memberOfSub (Term n')
          = n == n'
        memberOfSub (SubTrie t')
          = member' t' (lvl + 1)

--------------------------------------------------------------------
-- Part III

insert :: HashFun -> Int -> Int -> Int -> Trie -> Trie
insert hf depth size n t
  = insert' n t 0
  where
    insert' :: Int -> Trie -> Int -> Trie
    insert' n (Leaf ns) _
      | n `elem` ns = Leaf ns
      | otherwise   = Leaf (n : ns)
    insert' n _ lvl
      | lvl == depth - 1 = Leaf [n]
    insert' n (Node bv sns) lvl
      | testBit bv bvIdx = Node bv (replace snIdx sns (replaceSubNode sn))
      | otherwise        = Node (setBit bv bvIdx) (insertAt snIdx (Term n) sns)
      where
        bvIdx = getIndex (hf n) lvl size
        snIdx = countOnesFrom bvIdx bv
        sn    = sns !! snIdx

        replaceSubNode :: SubNode -> SubNode
        replaceSubNode (SubTrie st)
          = SubTrie (insert' n st (lvl + 1))
        replaceSubNode (Term n')
          | n == n'   = Term n'
          | otherwise = SubTrie (insert' n t' (lvl + 1))
          where
            t' = insert' n' empty (lvl + 1)
        
buildTrie :: HashFun -> Int -> Int -> [Int] -> Trie
buildTrie hf depth size
  = foldr (insert hf depth size) empty
