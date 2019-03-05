module EdgeGeographyGame where


{- A quicker state representing a state of a game. 
  
 -}
data State Integer =  (Integer) (Map Integer [Integer])
    deriving (Eq, Show)

-- You may import useful modules here.

{- The input is a list of adjacency lists, e.g.,
   [ (0, [1, 2]) , (1, [0]) , (2, [1]), (3, []) ]
   means 0->1, 0->2, 1->0, 2->1, 3 has no outgoing edges.

   goodFirstVertices takes this input and computes the choices for the first
   vertex so the first player is destined to win.
-}

goodFirstVertices :: [(Integer, [Integer])] -> [Integer]
goodFirstVertices [(a, [])] = [a]
goodFirstVertices inp = matchVertexEdges vertices edges
  where 
    vertices = allVertices inp
    edges = inputToState inp

matchVertexEdges :: [Integer]->[(Integer, Integer)]->[Integer]
matchVertexEdges [] edges = []
matchVertexEdges vertex [] = vertex
matchVertexEdges (vertex:rest) edges 
  | isWinningState (vertex, edges) == True =  vertex:(matchVertexEdges rest edges)
  | otherwise = matchVertexEdges rest edges

-- A function to detect if a state is winning state.
isWinningState :: (Integer, [(Integer, Integer)])-> Bool
isWinningState (value, []) = True
isWinningState curS@(v, x:xs) 
  | children == [] = True
  | otherwise =  hasWinningState child && allTrue (fmap hasWinningState rest)
  where  
    child:rest = childStates curS

hasWinningState :: (Integer, [(Integer, Integer)])-> Bool
hasWinningState x = hasTrue (fmap isWinningState children)
  where 
    children = childStates x


-- Helper function to detect if all the elements are true.
allTrue :: [Bool]-> Bool
allTrue [] = False
allTrue [value] = value
allTrue (x:xs) = x && allTrue xs

-- Helper function to detect if there exist a true.
hasTrue :: [Bool]-> Bool
hasTrue [] = False
hasTrue [value] = value
hasTrue (x:xs) = x || hasTrue xs

-- A function to translate list of adjacency lists Integero a state.
merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

turnIntegeroState :: (Integer, [Integer])-> [(Integer, Integer)]
turnIntegeroState (v, []) = []
turnIntegeroState (v, x:xs) = (v, x): y
  where 
    y = turnIntegeroState (v, xs)

inputToState :: [(Integer, [Integer])] ->  [(Integer, Integer)]
inputToState [] = []
inputToState (x:xs) = merge (turnIntegeroState x) (inputToState xs)

allVertices :: [(Integer, [Integer])] ->  [Integer]
allVertices [] = []
allVertices (x:xs) = value: (allVertices xs)
  where 
    (value, siblings) = x

-- Return all the children of current state.
-- A helper function that remove an edge from available edges and return a new state.
-- 
appendState :: (Integer, Integer)->[(Integer, [(Integer, Integer)])]-> [(Integer, [(Integer, Integer)])]
appendState  edge [] = []
appendState  edge (x:xs)  = (vertex, edge:edges):appendState edge xs
  where 
    (vertex, edges) = x

childStates ::(Integer, [(Integer, Integer)]) -> [(Integer, [(Integer, Integer)])]
childStates (a, []) = []
childStates (a, (x:xs)) 
  | a == from = (to, xs):appendState x (childStates (a, xs))
  | otherwise = appendState x (childStates (a, xs))  
  where 
      (from, to) = x
  

  
-- Test cases.
input1= [ (0, [1, 2]) , (1, [0]) , (2, [1]), (3, []) ]
input2 = [(0,[1,2]), (1, [3,4]), (2,[0]), (3,[4]), (4,[])]
input3 = [(0,[2,5]),(1,[0,3,4]),(2,[0,1,4,5]), (3,[1,4,5]),(4,[0,1,2,5]),(5,[1,2,4])]
input4 = [(0,[10,11,12]),(1,[3,8,20]),(2,[9,15]),(3,[2,5,10,11]),(4,[5,11,21]),(5,[6,18,20]),(6,[3,12,14,18,19]),(7,[19]),(8,[2,13,22]),(9,[1,13,23]),(10,[5,12,16]),(11,[12,18]),(12,[7]),(13,[4,15,19,23,24]),(14,[5,6,7,15,20,23]),(15,[20,23]),(16,[9]),(17,[12,18]),(18,[6,19]),(19,[6,11,12,24]),(20,[]),(21,[1,10,14,16]),(22,[5,8,11,16]),(23,[14]),(24,[1,11])]
test1 = [(0, 2), (0,1), (1,4), (1,3), (3,4), (2,0)]
testState = (0, [(0,1), (1,2)]) -- True
testState2 = (0, [(0,1), (1,0)]) -- True
testState3 = (0, [(0,1), (1,0), (0,2)]) -- False

