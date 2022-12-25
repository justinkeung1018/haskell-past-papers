module BinomialHeaps where

type BinHeap a = [BinTree a]

data BinTree a = Node a Int (BinHeap a)
               deriving (Eq, Ord, Show)

--------------------------------------------------------------
-- PART I

key :: BinTree a -> a
key (Node k _ _)
  = k

rank :: BinTree a -> Int
rank (Node _ r _)
  = r

children :: BinTree a -> [BinTree a]
children (Node _ _ h)
  = h

-- Pre: the two trees have the same rank
combineTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
combineTrees t1@(Node k1 r1 h1) t2@(Node k2 r2 h2)
  | k1 < k2   = Node k1 (r1 + 1) (t2 : h1)
  | otherwise = Node k2 (r2 + 1) (t1 : h2)

--------------------------------------------------------------
-- PART II

extractMin :: Ord a => BinHeap a -> a
extractMin
  = minimum . map key

mergeHeaps :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
mergeHeaps [] h'
  = h'
mergeHeaps h []
  = h
mergeHeaps tts@(t : ts) tts'@(t' : ts')
  | rank t < rank t' = t : mergeHeaps ts tts'
  | rank t' < rank t = t' : mergeHeaps tts ts'
  | otherwise        = mergeHeaps [combineTrees t t'] (mergeHeaps ts ts')

insert :: Ord a => a -> BinHeap a -> BinHeap a
insert v
  = mergeHeaps [Node v 0 []]

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin h
  = mergeHeaps (children minTree) rest
  where
    minKey  = extractMin h
    minTree = head (filter (\t -> key t == minKey) h)
    rest    = filter (/= minTree) h

remove :: Eq a => a -> BinHeap a -> BinHeap a
remove
  = undefined

removeMin :: Ord a => BinHeap a -> (BinTree a, BinHeap a)
removeMin
  = undefined

binSort :: Ord a => [a] -> [a]
binSort xs
  = binSort' (foldr insert [] xs)
  where
    binSort' []
      = []
    binSort' h
      = extractMin h : binSort' (deleteMin h)

--------------------------------------------------------------
-- PART III

toBinary :: BinHeap a -> [Int]
toBinary []
  = []
toBinary h
  = reverse [if i `elem` ranks then 1 else 0 | i <- [0..maximum ranks]]
  where
    ranks = map rank h

binarySum :: [Int] -> [Int] -> [Int]
binarySum x y
  = dropWhile (== 0) (reverse (binarySum' x' y' 0))
  where
    binarySum' [] [] cOut
      = [cOut]
    binarySum' (x : xs) (y : ys) cIn
      = sum : binarySum' xs ys cOut
      where
        (sum, cOut) = sumDigit x y cIn
    numDigits = max (length x) (length y)
    padAndReverse n
      = reverse (replicate (numDigits - length n + 1) 0 ++ n) -- Extra 0 for overflow
    x' = padAndReverse x
    y' = padAndReverse y
    sumDigit b1 b2 cIn
      = (sum, cOut)
      where
        (cOut, sum) = quotRem (b1 + b2 + cIn) 2


------------------------------------------------------
-- Some sample trees...

t1, t2, t3, t4, t5, t6, t7, t8 :: BinTree Int
-- Note: t7 is the result of merging t5 and t6

-- t1 to t4 appear in Figure 1...
t1 = Node 4 0 []
t2 = Node 1 1 [Node 5 0 []]
t3 = Node 2 2 [Node 8 1 [Node 9 0 []], 
               Node 7 0 []]
t4 = Node 2 3 [Node 3 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []],
               Node 8 1 [Node 9 0 []],
               Node 7 0 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 4 2 [Node 6 1 [Node 8 0 []], 
                         Node 10 0 []]
t6 = Node 2 2 [Node 8 1 [Node 9 0 []], Node 7 0 []]
t7 = Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []], Node 10 0 []],
               Node 8 1 [Node 9 0 []], 
               Node 7 0 []]

-- An additional tree...
t8 = Node 12 1 [Node 16 0 []]

------------------------------------------------------
-- Some sample heaps...

h1, h2, h3, h4, h5, h6, h7 :: BinHeap Int
-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 1 2 [Node 12 1 [Node 16 0 []],
                Node 5 0 []],
      Node 2 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 8 1 [Node 9 0 []],
                Node 7 0 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 4 0 [],
      Node 1 3 [Node 4 2 [Node 6 1 [Node 8 0 []],
                          Node 10 0 []],
                Node 12 1 [Node 16 0 []],
                Node 5 0 []]]

-- h7 is shown in Figure 5...
h7 = [Node 4 3 [Node 4 2 [Node 12 1 [Node 16 0 []],
                          Node 5 0 []],
                Node 6 1 [Node 8 0 []],
                Node 10 0 []]]



