module DecisionTrees where

import Data.Maybe
import Data.List

type AttName = String

type AttValue = String

type Attribute = (AttName, [AttValue])

type Header = [Attribute]

type Row = [AttValue]

type DataSet = (Header, [Row])

data DecisionTree = Null |
                    Leaf AttValue | 
                    Node AttName [(AttValue, DecisionTree)]
                  deriving (Eq, Show)

type Partition = [(AttValue, DataSet)]

type AttSelector = DataSet -> Attribute -> Attribute

xlogx :: Double -> Double
xlogx p
  | p <= 1e-100 = 0.0
  | otherwise   = p * log2 p 
  where
    log2 x = log x / log 2

lookUp :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
lookUp x table
  = fromMaybe (error ("lookUp error - no binding for " ++ show x ++ 
                      " in table: " ++ show table))
              (lookup x table)

--------------------------------------------------------------------
-- PART I
--------------------------------------------------------------------

allSame :: Eq a => [a] -> Bool
allSame []
  = True
allSame (x : xs)
  = all (== x) xs

remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove x
  = filter (\(x', _) -> x /= x')

zipHeaderRow :: Header -> Row -> [(AttName, AttValue)]
zipHeaderRow
  = zipWith (\(name', _) v -> (name', v))

lookUpAtt :: AttName -> Header -> Row -> AttValue
--Pre: The attribute name is present in the given header.
lookUpAtt name h r
  = lookUp name (zipHeaderRow h r)

removeAtt :: AttName -> Header -> Row -> Row
removeAtt name h r
  = map snd (remove name (zipHeaderRow h r))

addToMapping :: Eq a => (a, b) -> [(a, [b])] -> [(a, [b])]
addToMapping (x, v) []
  = [(x, [v])]
addToMapping (x, v) ((x', v') : xvs)
  | x == x'   = (x, v : v') : xvs
  | otherwise = (x', v') : addToMapping (x, v) xvs

buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
--Pre: Each row of the data set contains an instance of the attribute
buildFrequencyTable (name, vs) (h, rs)
  = map (\v -> (v, length (filter (v ==) (map (lookUpAtt name h) rs)))) vs

--------------------------------------------------------------------
-- PART II
--------------------------------------------------------------------

nodes :: DecisionTree -> Int
nodes Null
  = 0
nodes (Leaf _)
  = 1
nodes (Node _ children)
  = 1 + sum (map (nodes . snd) children)

evalTree :: DecisionTree -> Header -> Row -> AttValue
evalTree Null _ _
  = ""
evalTree (Leaf v) _ _
  = v
evalTree (Node name children) h r
  = evalTree (lookUp (lookUpAtt name h r) children) h r

--------------------------------------------------------------------
-- PART III
--------------------------------------------------------------------

--
-- Given...
-- In this simple case, the attribute selected is the first input attribute 
-- in the header. Note that the classifier attribute may appear in any column,
-- so we must exclude it as a candidate.
--
nextAtt :: AttSelector
--Pre: The header contains at least one input attribute
nextAtt (header, _) (classifierName, _)
  = head (filter ((/= classifierName) . fst) header)

partitionData :: DataSet -> Attribute -> Partition
partitionData (h, rs) (name, vs)
  = map (\v -> (v, (remove name h, [removeAtt name h r 
                                   | r <- rs, v == lookUpAtt name h r]))) vs

buildTree :: DataSet -> Attribute -> AttSelector -> DecisionTree 
buildTree d@(h, rs) cl@(clName, _) f
  | null rs    = Null
  | allSame vs = Leaf (head vs)
  | otherwise  = Node name (map (\(v, d') -> (v, buildTree d' cl f)) p)
  where
    a@(name, _) = f d cl
    vs          = map (lookUpAtt clName h) rs
    p           = partitionData d a

--------------------------------------------------------------------
-- PART IV
--------------------------------------------------------------------

prob :: DataSet -> Attribute -> AttValue -> Double
prob d a v
  = fromIntegral (lookUp v freqs) / total
  where
    freqs = buildFrequencyTable a d
    total = fromIntegral (sum (map snd freqs))

entropy :: DataSet -> Attribute -> Double
entropy (_, []) _
  = 0.0

-- Suppose there are n values for the given attribute
-- I think this is O(n):
entropy d a
  = sum (map (\(_, freq) -> negate (xlogx (fromIntegral freq / total))) freqs)
  where
    freqs = buildFrequencyTable a d
    total = fromIntegral (sum (map snd freqs))

-- And this is O(n ^ 2) (but looks cleaner and more semantic):
entropy d a@(_, vs)
  = sum (map (negate . xlogx . prob d a) vs)

gain :: DataSet -> Attribute -> Attribute -> Double
gain d a cl
  = entropy d cl - sum (map (\(v, d') -> prob d a v * entropy d' cl) p)
  where
    p = partitionData d a

bestGainAtt :: AttSelector
bestGainAtt d@(h, _) cl
  = fst (maximumBy compareGains (zip h' (map (\a -> gain d a cl) h')))
  where
    h' = delete cl h
    compareGains x y
      = compare (snd x) (snd y)

--------------------------------------------------------------------

outlook :: Attribute
outlook 
  = ("outlook", ["sunny", "overcast", "rainy"])

temp :: Attribute 
temp 
  = ("temp", ["hot", "mild", "cool"])

humidity :: Attribute 
humidity 
  = ("humidity", ["high", "normal"])

wind :: Attribute 
wind 
  = ("wind", ["windy", "calm"])

result :: Attribute 
result
  = ("result", ["good", "bad"])

fishingData :: DataSet
fishingData
  = (header, table)

header :: Header
table  :: [Row]
header 
  =  [outlook,    temp,   humidity, wind,    result] 
table 
  = [["sunny",    "hot",  "high",   "calm",  "bad" ],
     ["sunny",    "hot",  "high",   "windy", "bad" ],
     ["overcast", "hot",  "high",   "calm",  "good"],
     ["rainy",    "mild", "high",   "calm",  "good"],
     ["rainy",    "cool", "normal", "calm",  "good"],
     ["rainy",    "cool", "normal", "windy", "bad" ],
     ["overcast", "cool", "normal", "windy", "good"],
     ["sunny",    "mild", "high",   "calm",  "bad" ],
     ["sunny",    "cool", "normal", "calm",  "good"],
     ["rainy",    "mild", "normal", "calm",  "good"],
     ["sunny",    "mild", "normal", "windy", "good"],
     ["overcast", "mild", "high",   "windy", "good"],
     ["overcast", "hot",  "normal", "calm",  "good"],
     ["rainy",    "mild", "high",   "windy", "bad" ]]

--
-- This is the same as the above table, but with the result in the second 
-- column...
--
fishingData' :: DataSet
fishingData'
  = (header', table')

header' :: Header
table'  :: [Row]
header' 
  =  [outlook,    result, temp,   humidity, wind] 
table' 
  = [["sunny",    "bad",  "hot",  "high",   "calm"],
     ["sunny",    "bad",  "hot",  "high",   "windy"],
     ["overcast", "good", "hot",  "high",   "calm"],
     ["rainy",    "good", "mild", "high",   "calm"],
     ["rainy",    "good", "cool", "normal", "calm"],
     ["rainy",    "bad",  "cool", "normal", "windy"],
     ["overcast", "good", "cool", "normal", "windy"],
     ["sunny",    "bad",  "mild", "high",   "calm"],
     ["sunny",    "good", "cool", "normal", "calm"],
     ["rainy",    "good", "mild", "normal", "calm"],
     ["sunny",    "good", "mild", "normal", "windy"],
     ["overcast", "good", "mild", "high",   "windy"],
     ["overcast", "good", "hot",  "normal", "calm"],
     ["rainy",    "bad",  "mild", "high",   "windy"]]

fig1 :: DecisionTree
fig1
  = Node "outlook" 
         [("sunny", Node "temp" 
                         [("hot", Leaf "bad"),
                          ("mild",Node "humidity" 
                                       [("high",   Leaf "bad"),
                                        ("normal", Leaf "good")]),
                          ("cool", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "temp" 
                         [("hot", Null),
                          ("mild", Node "humidity" 
                                        [("high",Node "wind" 
                                                      [("windy",  Leaf "bad"),
                                                       ("calm", Leaf "good")]),
                                         ("normal", Leaf "good")]),
                          ("cool", Node "humidity" 
                                        [("high", Null),
                                         ("normal", Node "wind" 
                                                         [("windy",  Leaf "bad"),
                                                          ("calm", Leaf "good")])])])]

fig2 :: DecisionTree
fig2
  = Node "outlook" 
         [("sunny", Node "humidity" 
                         [("high", Leaf "bad"),
                          ("normal", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "wind" 
                         [("windy", Leaf "bad"),
                          ("calm", Leaf "good")])]


outlookPartition :: Partition
outlookPartition
  = [("sunny",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","bad"],["hot","high","windy","bad"],
                   ["mild","high","calm","bad"],["cool","normal","calm","good"],
                   ["mild","normal","windy","good"]])),
     ("overcast",([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","good"],["cool","normal","windy","good"],
                   ["mild","high","windy","good"],["hot","normal","calm","good"]])),
     ("rainy",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["mild","high","calm","good"],["cool","normal","calm","good"],
                   ["cool","normal","windy","bad"],["mild","normal","calm","good"],
                   ["mild","high","windy","bad"]]))]
