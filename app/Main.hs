{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad.Random (MonadRandom, getRandomR, evalRandIO)
import Data.Graph.Inductive.Graph (mkGraph, Node)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (spLength)
import Data.List (elemIndex, nub)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Vector as V

type Distance = Double
type Path = V.Vector Node
type LazyDistanceMap = M.Map (Node, Node) Distance

-- Syöte
parseGraph :: String -> (Gr () Distance, M.Map Node String)
parseGraph input = (mkGraph graphNodes graphEdges, idToName)
  where
    rawPairs = filter (not . null) $ map (filter (not . (`elem` "\r\n\t "))) (splitOn ";" input)

    toTriplet s = case splitOn "," s of
                    [u, v, d] -> Just (u, v, d)
                    _         -> Nothing
    triplets = mapMaybe toTriplet rawPairs

    allNames = nub $ concat [ [u, v] | (u, v, _) <- triplets ]

    nameToId name = fromMaybe 0 (elemIndex name allNames)
    idToName = M.fromList $ zip [0..] allNames

    graphNodes = map (, ()) [0 .. length allNames - 1]
    graphEdges = [ (nameToId u, nameToId v, read d) | (u, v, d) <- triplets ]


-- Dijkstra
getDistance :: Gr () Distance -> Node -> Node -> LazyDistanceMap -> (Distance, LazyDistanceMap)
getDistance graph u v cache =
  case M.lookup (u, v) cache of
    Just dist -> (dist, cache)
    Nothing   ->
      let dist = fromMaybe (1/0) (spLength u v graph)
          newCache = M.insert (u, v) dist cache
      in (dist, newCache)


fullPathCost :: Gr () Distance -> Path -> LazyDistanceMap -> (Distance, LazyDistanceMap)
fullPathCost graph path cache = V.foldl sumCosts (0, cache) indices
  where
    len = V.length path
    indices = V.generate len id
    sumCosts (accCost, currentCache) i =
      let u = path V.! i
          v = path V.! ((i + 1) `mod` len)
          (d, nextCache) = getDistance graph u v currentCache
      in (accCost + d, nextCache)


mutateInsertion :: MonadRandom m => Path -> m (Path, Int, Int)
mutateInsertion path = do
  let len = V.length path
  if len < 3
    then return (path, 0, 0)
  else do
    fromIdx <- getRandomR (0, len - 1)
    toIdx   <- getRandomR (0, len - 1)
    if fromIdx == toIdx
      then return (path, fromIdx, toIdx)
    else do
      let target = path V.! fromIdx
          withoutTarget = V.slice 0 fromIdx path V.++ V.slice (fromIdx + 1) (len - fromIdx - 1) path
          newPath = V.slice 0 toIdx withoutTarget V.++ V.singleton target V.++ V.slice toIdx (V.length withoutTarget - toIdx) withoutTarget
      return (newPath, fromIdx, toIdx)


calculateDelta
  :: Gr () Distance
  -> Path
  -> Path
  -> Int
  -> Int
  -> LazyDistanceMap
  -> (Distance, LazyDistanceMap)

calculateDelta graph oldPath _ fromIdx toIdx cache
  | fromIdx == toIdx =
      (0, cache)
  | otherwise =
      let
          n = V.length oldPath
          x = oldPath V.! fromIdx
          a = oldPath V.! ((fromIdx - 1 + n) `mod` n)
          b = oldPath V.! ((fromIdx + 1) `mod` n)
          reduced =
            V.slice 0 fromIdx oldPath
              V.++ V.slice (fromIdx + 1) (n - fromIdx - 1) oldPath

          reducedLen = n - 1
          c = reduced V.! ((toIdx - 1 + reducedLen) `mod` reducedLen)
          d = reduced V.! (toIdx `mod` reducedLen)

          removedEdges =
            [ (a, x)
            , (x, b)
            , (c, d)
            ]

          addedEdges =
            [ (a, b)
            , (c, x)
            , (x, d)
            ]

          sumEdges (total, c0) (u, v) =
            let (dist, c1) = getDistance graph u v c0
            in (total + dist, c1)

          (oldCost, cache1) = foldl sumEdges (0, cache) removedEdges
          (newCost, cache2) = foldl sumEdges (0, cache1) addedEdges
      in
        (newCost - oldCost, cache2)


anneal
  :: MonadRandom m
  => Gr () Distance
  -> Path              -- current path
  -> Distance          -- current cost
  -> Path              -- best path
  -> Distance          -- best cost
  -> LazyDistanceMap
  -> Int
  -> Double
  -> m (Path, Distance)

anneal _ _ _ bestPath bestCost _ 0 _ =
  return (bestPath, bestCost)

anneal graph currentPath currentCost bestPath bestCost cache steps temp = do
  (candidatePath, fromIdx, toIdx) <- mutateInsertion currentPath

  let (delta, nextCache) =
        calculateDelta
          graph
          currentPath
          candidatePath
          fromIdx
          toIdx
          cache

      candidateCost = currentCost + delta

  accept <-
    if delta < 0
      then return True
    else do
      r <- getRandomR (0.0, 1.0)
      return (r < exp( -(delta / temp) ))

  let (nextPath, nextCost) =
        if accept
          then (candidatePath, candidateCost)
        else (currentPath, currentCost)

      (nextBestPath, nextBestCost) =
        if nextCost < bestCost
          then (nextPath, nextCost)
        else (bestPath, bestCost)

      nextTemp = temp * 0.99995

  anneal
    graph
    nextPath
    nextCost
    nextBestPath
    nextBestCost
    nextCache
    (steps - 1)
    nextTemp


main :: IO ()
main = do
  inputStr <- getContents

  if all (`elem` "\r\n\t ") inputStr
    then putStrLn "Virhe: Syöte on tyhjä."
    else do
      let (graph, idToName) = parseGraph inputStr
          initialPath = V.fromList [0 .. M.size idToName - 1]

      if V.null initialPath
        then putStrLn "Virhe: Graafista ei löytynyt valideja kaaria."
        else do
          let (initCost, initCache) = fullPathCost graph initialPath M.empty
          putStrLn $ "Solmuja yhteensä: " ++ show (V.length initialPath)
          putStrLn $ "Alkukustannus: " ++ show initCost

          (finalPath, finalCost) <- evalRandIO $
            anneal
              graph
              initialPath
              initCost
              initialPath
              initCost
              initCache
              500000
              200.0

          let finalPathNames = map (\nodeId -> fromMaybe (show nodeId) (M.lookup nodeId idToName)) (V.toList finalPath)

          putStrLn "\n--- TULOKSET ---"
          putStrLn $ "Paras löydetty reitti:\n" ++ show finalPathNames
          putStrLn $ "Kustannus: " ++ show finalCost
