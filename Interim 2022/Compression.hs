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
count x xs
  = length (filter (x ==) xs)

countAll :: Eq a => [a] -> [a] -> [(a, Int)]
countAll xs ys
  = nub (map (\x -> (x, count x ys)) xs)

buildTable :: Eq a => [a] -> [(a, Int)]
buildTable xs
  = countAll xs xs

merge :: HTree a -> HTree a -> HTree a
merge t1 t2
  | n1 <= n2  = Node n t1 t2
  | otherwise = Node n t2 t1
  where
    n  = n1 + n2
    n1 = freqCount t1
    n2 = freqCount t2

reduce :: [HTree a] -> HTree a
-- Pre: The argument list non-empty and sorted based on the ordering function
--      in the Ord instance above.
reduce [t]
  = t
reduce (t1 : t2 : ts)
  = reduce (insert (merge t1 t2) ts)

buildTree :: Eq a => [a] -> HTree a
-- Pre: The list is non-empty
buildTree xs
  = reduce (sort (map (\(x, n) -> Leaf n x) (buildTable xs)))

encode :: Eq a => [a] -> HTree a -> Code
-- Pre: The tree can encode each of the items the list
encode xxs t
  = encode' xxs t []
  where
    encode' [] _ _ 
      = []
    encode' (x : xs) (Leaf _ x') path
      | x == x'   = path ++ encode' xs t []
      | otherwise = []
    encode' xs (Node _ t1 t2) path
      | null path1 = path2
      | otherwise  = path1
      where
        path1 = encode' xs t1 (path ++ [0])
        path2 = encode' xs t2 (path ++ [1])

decode :: Code -> HTree a -> [a]
-- Pre: The code is valid with respect to the tree
decode code t
  = decode' code t
  where
    decode' bs (Leaf _ x)
      = x : decode' bs t
    decode' [] _
      = []
    decode' (b : bs) (Node _ t1 t2)
      | b == 0 = decode' bs t1
      | b == 1 = decode' bs t2

compressTree :: HTree Char -> [Int]
compressTree (Leaf _ x)
  = 1 : intToBitstring (ord x)
  where
    -- Assume the character does not have ordinal 0, i.e. it is not \NUL
    intToBitstring 0
      = []
    intToBitstring n
      = intToBitstring (n `div` 2) ++ [n `mod` 2]
compressTree (Node _ t1 t2)
  = 0 : (compressTree t1 ++ compressTree t2)

rebuildTree :: [Int] -> HTree Char
-- Pre: The bitstring ([Int]) is a valid encoding of a Huffman tree
--      of characters
rebuildTree bstr
  = t
  where
    (t, _) = rebuildTree' bstr
    
    -- Returns a pair where the first element is the H(Sub)Tree
    -- and the second element is the remaining bits after the H(Sub)Tree
    rebuildTree' (b : bs)
      | b == 1 = (Leaf 0 (chr (bitstringToInt hd)), tl)
      | b == 0 = (Node 0 lt rt, rest')
      where
        (hd, tl)    = splitAt 7 bs
        (lt, rest)  = rebuildTree' bs 
        (rt, rest') = rebuildTree' rest
        bitstringToInt
          = foldl (\x y -> x * 2 + y) 0   

     

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

