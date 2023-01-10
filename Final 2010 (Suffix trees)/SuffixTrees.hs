module SuffixTrees where

data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix s s'
  = s == take (length s) s'

removePrefix :: String -> String -> String
removePrefix s
--Pre: s is a prefix of s'
  = drop (length s)

suffixes :: [a] -> [[a]]
suffixes []
  = []
suffixes xs
  = xs : suffixes (tail xs)

isSubstring :: String -> String -> Bool
isSubstring s s'
  = null s || any (isPrefix s) (suffixes s')

findSubstrings :: String -> String -> [Int]
findSubstrings s s'
  = [i | (suf, i) <- zip (suffixes s') [0..], isPrefix s suf]

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf i)
  = [i]
getIndices (Node children)
  = concatMap (getIndices . snd) children

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition [] ys
  = ([], [], ys)
partition xs []
  = ([], xs, [])
partition xxs@(x : xs) yys@(y : ys)
  | x == y    = (x : pre, xs', ys')
  | otherwise = ([], xxs, yys)
  where
    (pre, xs', ys') = partition xs ys

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' "" (Leaf i)
  = [i]
findSubstrings' s (Leaf i)
  = []
findSubstrings' s (Node ats)
  = concatMap (search s) ats
  where
    search :: String -> (String, SuffixTree) -> [Int]
    search s (a, t)
      | null s'   = getIndices t
      | null a'   = findSubstrings' s' t
      | otherwise = []
      where
        (_, s', a') = partition s a

------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (s, n) t@(Node ats)
  = Node (insert' (s, n) ats)
  where
    insert' (s, n) []
      = [(s, Leaf n)]
    insert' (s, n) ((a, t) : ats)
      | null p    = (a, t) : insert' (s, n) ats
      | null a'   = (a, insert (s', n) t) : ats
      | otherwise = (p, Node [(s', Leaf n), (a', t)]) : ats
      where
        (p, s', a') = partition s a

-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring (Leaf _)
  = ""
longestRepeatedSubstring (Node ats)
  = (snd . maximum) (map (\sub -> (length sub, sub)) candidates)
  where
    candidates = [a ++ longestRepeatedSubstring t 
                 | (a, t) <- ats, length (getIndices t) > 1] ++ [""]

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]


