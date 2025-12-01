module DijkstraSimple where

{- required packages:
   containers, unordered-containers, hashable
-}

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.Heap (MinPrioHeap)
import Data.Heap qualified as H
import Data.Maybe (fromMaybe)

data Distance a = Dist a | Infinity
  deriving (Show, Eq)

instance (Ord a) => Ord (Distance a) where
  Infinity <= Infinity = True
  Infinity <= Dist x = False
  Dist x <= Infinity = True
  Dist x <= Dist y = x <= y

addDist :: (Num a) => Distance a -> Distance a -> Distance a
addDist (Dist x) (Dist y) = Dist (x + y)
addDist _ _ = Infinity

(!??) :: (Hashable k, Eq k) => HashMap k (Distance d) -> k -> Distance d
(!??) distanceMap key = fromMaybe Infinity (HM.lookup key distanceMap)

newtype Graph = Graph
  {edges :: HashMap String [(String, Int)]}

data DijkstraState = DijkstraState
  { visitedSet :: HashSet String,
    distanceMap :: HashMap String (Distance Int),
    nodeQueue :: MinPrioHeap (Distance Int) String
  }

findShortestDistance :: Graph -> String -> String -> Distance Int
findShortestDistance graph src dest = processQueue initialState !?? dest
  where
    initialVisited = HS.empty
    initialDistances = HM.singleton src (Dist 0)
    initialQueue = H.fromList [(Dist 0, src)]
    initialState = DijkstraState initialVisited initialDistances initialQueue

    processQueue :: DijkstraState -> HashMap String (Distance Int)
    processQueue ds@(DijkstraState v0 d0 q0) = case H.view q0 of
      Nothing -> d0
      Just ((minDist, node), q1) ->
        if node == dest
          then d0
          else
            if HS.member node v0
              then processQueue (ds {nodeQueue = q1})
              else
                -- Update the visited set
                let v1 = HS.insert node v0
                    -- Get all unvisited neighbors of our current node
                    allNeighbors = fromMaybe [] (HM.lookup node (edges graph))
                    unvisitedNeighbors = filter (\(n, _) -> not (HS.member n v1)) allNeighbors
                 in -- Fold each neighbor and recursively process the queue
                    processQueue $ foldl (foldNeighbor node) (DijkstraState v1 d0 q1) unvisitedNeighbors
    foldNeighbor current ds@(DijkstraState v1 d0 q1) (neighborNode, cost) =
      let altDistance = addDist (d0 !?? current) (Dist cost)
       in if altDistance < d0 !?? neighborNode
            then DijkstraState v1 (HM.insert neighborNode altDistance d0) (H.insert (altDistance, neighborNode) q1)
            else ds

graph1 :: Graph
graph1 =
  Graph $
    HM.fromList
      [ ("A", [("B", 4), ("C", 5)]),
        ("B", [("D", 7)]),
        ("C", [("B", 3), ("E", 4)]),
        ("D", [("C", 2), ("F", 1)]),
        ("E", [("D", 1), ("F", 3)]),
        ("G", [("H", 5)]),
        ("H", [("G", 5)])
      ]

main :: IO ()
main = do
  print (findShortestDistance graph1 "A" "F")