module Compression where

import Data.List
import Data.Char

data HTree a = Leaf Int a | Node Int (HTree a) (HTree a)
               deriving (Show)

instance Eq (HTree a) where
  t1 == t2 = freqCount t1 == freqCount t2

instance Ord (HTree a) where
  t1 <= t2' = freqCount t1 <= freqCount t2'

type Code = [Int]

freqCount :: HTree a -> Int
freqCount (Leaf n a)
  = n
freqCount (Node n t1 t2)
  = n

testString :: String
testString
  = "mississippi is missing"

--
-- Example Huffman coding tree from the spec.
--
fig :: HTree Char
fig
  = Node 22 (Node 8 (Node 4 (Leaf 2 'p') (Leaf 2 ' '))
                    (Node 4 (Node 2 (Leaf 1 'n') (Leaf 1 'g'))
                            (Leaf 2 'm')))
            (Node 14 (Leaf 7 'i') (Leaf 7 's'))

----------------------------------------------------------------------------

count :: Eq a => a -> [a] -> Int
count
  = undefined

countAll :: Eq a => [a] -> [a] -> [(a, Int)]
countAll
  = undefined

buildTable :: Eq a => [a] -> [(a, Int)]
buildTable
  = undefined

merge :: HTree a -> HTree a -> HTree a
merge
  = undefined

reduce :: [HTree a] -> HTree a
-- Pre: The argument list non-empty and sorted based on the ordering function
--      in the Ord instance above.
reduce
  = undefined

buildTree :: Eq a => [a] -> HTree a
-- Pre: The list is non-empty
buildTree
  = undefined

encode :: Eq a => [a] -> HTree a -> Code
-- Pre: The tree can encode each of the items the list
encode
  = undefined

decode :: Code -> HTree a -> [a]
-- Pre: The code is valid with respect to the tree
decode
  = undefined

compressTree :: HTree Char -> [Int]
compressTree
  = undefined

rebuildTree :: [Int] -> HTree Char
-- Pre: The bitstring ([Int]) is a valid encoding of a Huffman tree
--      of characters
rebuildTree
  = undefined

-----------------------------------------------------------------------------
-- THE SECRET TEST STRING (GIVEN)...
--

secretString :: String
secretString
  = decode code (rebuildTree tree)
  where
    code = [1,1,0,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,
            1,1,1,1,1,1,1,1,1,1,0,1,0,1,1,1,0,0,1,1,1,0,1,1,0,1,1,
            0,1,0,0,1,0,1,1,1,1,1,0,1,0,1,1,0,0,0,1,1,0,1,0,0,0,1,
            1,0,0,0,1,0,1,1,0,0,1,1,0,1,1,0,1,0,0,1,1,1,0,1,0,0,0,
            1,0,1,0,0,0,1,1,0,1,0]
    tree = [0,0,0,0,1,1,1,1,1,0,0,0,1,1,1,0,0,1,0,1,0,1,1,1,0,0,0,
            1,1,1,1,1,1,0,1,1,1,0,0,1,1,1,0,1,1,1,1,1,1,1,0,1,1,1,
            0,0,1,1,1,0,1,1,0,0,1,1,1,0,0,1,0,0,0,0,1,1,1,0,1,0,1,
            1,0,1,0,1,0,0,0,0,1,0,1,1,1,1,0,0,1,0,1,1,1,1,0,0,1,1,
            0,0,0,1,0,1,0,1,1,0,1,1,0,1,0,1,1,0,0,0,1,0,1,0,0,0,0,
            0,1,0,1,0,1,0,1,1,0,0,1,1,1,0,1,1,0,1,1,1,1,0,0,0,0,1,
            0,1,0,1,1,0,0,0,1,1,0,1,1,0,0,0,0]

