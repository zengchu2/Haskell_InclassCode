module EdgeGeographyGame where
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data MapState =  State Integer (Map Integer [Integer])
    deriving (Eq, Show)

combineVerticeEdge [] edges = []
combineVerticeEdge (v1:vs) edges = (State v1 edges):combineVerticeEdge vs edges

turnIntoState :: [(Integer, [Integer])]->[MapState]
turnIntoState [] = []
turnIntoState (x:xs) = combineVerticeEdge vertices edges
  where
    edges = Map.fromList (x:xs)
    vertices = Map.keys edges

children :: MapState -> [MapState]
children s@(State vertex (map))
  | lst == [] = []
  | otherwise = (State x (Map.insert vertex xs map)):(addVertex x vertex (children (State vertex (Map.insert vertex xs map))))
  where 
    Just lst = Map.lookup vertex map
    (x:xs) =  lst

addVertex :: Integer -> Integer -> [MapState]->[MapState]
addVertex vertex cur [] = []
addVertex vertex cur (x:xs) = (State next (Map.insert cur (vertex:lst) map)):(addVertex vertex cur xs)
  where 
    (State next map) = x
    Just lst = Map.lookup cur map

-- The above is a list of children.
isWinningState :: MapState-> Bool
isWinningState curS@(State vertex map) 
  | childrenStates == [] = True
  | otherwise =  hasWinningState child && allTrue (fmap hasWinningState rest)
  where  
    childrenStates = children curS
    child:rest = childrenStates

hasWinningState :: MapState -> Bool
hasWinningState x 
  | childStates == [] = False
  | otherwise =  isWinningState child1 || hasTrue (fmap isWinningState rest)
  where 
    childStates = children x
    child1:rest = childStates


-- Helper function to detect if all the elements are true.
allTrue :: [Bool]-> Bool
allTrue [] = True
allTrue [value] = value
allTrue (x:xs) = x && allTrue xs

-- Helper function to detect if there exist a true.
hasTrue :: [Bool]-> Bool
hasTrue [] = False
hasTrue [value] = value
hasTrue (x:xs) = x || hasTrue xs

-- You may import useful modules here.

{- The input is a list of adjacency lists, e.g.,
   [ (0, [1, 2]) , (1, [0]) , (2, [1]), (3, []) ]
   means 0->1, 0->2, 1->0, 2->1, 3 has no outgoing edges.

   goodFirstVertices takes this input and computes the choices for the first
   vertex so the first player is destined to win.
-}

goodFirstVertices :: [(Integer, [Integer])] -> [Integer]
goodFirstVertices [(a, [])] = [a]
goodFirstVertices inp = matchVertexEdges (turnIntoState inp)

matchVertexEdges :: [MapState]->[Integer]
matchVertexEdges [] = []
matchVertexEdges (s1:rest) 
  | isWinningState s1 == True =  vertex:(matchVertexEdges rest)
  | otherwise = matchVertexEdges rest
  where 
    (State vertex map) = s1

	
	
-- Test cases.
input1= [ (0, [1, 2]) , (1, [0]) , (2, [1]), (3, []) ]
states = turnIntoState input1
s1:ss = states
input2 = [(0,[1,2]), (1, [3,4]), (2,[0]), (3,[4]), (4,[])]
input3 = [(0,[2,5]),(1,[0,3,4]),(2,[0,1,4,5]), (3,[1,4,5]),(4,[0,1,2,5]),(5,[1,2,4])]
input4 = [(0,[10,11,12]),(1,[3,8,20]),(2,[9,15]),(3,[2,5,10,11]),(4,[5,11,21]),(5,[6,18,20]),(6,[3,12,14,18,19]),(7,[19]),(8,[2,13,22]),(9,[1,13,23]),(10,[5,12,16]),(11,[12,18]),(12,[7]),(13,[4,15,19,23,24]),(14,[5,6,7,15,20,23]),(15,[20,23]),(16,[9]),(17,[12,18]),(18,[6,19]),(19,[6,11,12,24]),(20,[]),(21,[1,10,14,16]),(22,[5,8,11,16]),(23,[14]),(24,[1,11])]

testState = (0, [(0,1), (1,2)]) -- True
testState2 = (0, [(0,1), (1,0)]) -- True
testState3 = (0, [(0,1), (1,0), (0,2)]) -- False

