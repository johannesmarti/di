module ProgramToGraph (
  programToGraph
) where

import Control.Monad.State.Strict
import Data.Map.Strict as Map
import Data.Set as Set

import Program
import Graph

type FreshIntState x = State Int x

freshInt :: FreshIntState Int
freshInt = do
  i <- get
  put (i + 1)
  return i

-- this might be a shitty function. not sure how to make this better
assignInts :: Ord r => Set r -> FreshIntState (Map r Int)
assignInts set = do
  let makePair predicate = do i <- freshInt
                              return (predicate, i)
  ascList <- traverse makePair (Set.toAscList set)
  return (Map.fromDistinctAscList ascList)

programToGraphWithTrace :: Ord r => Program r v -> (Graph r v Int, Map r Int)
programToGraphWithTrace = undefined
 -- initialize the map of all predicates
 -- work through each element in the map and take care of the remaining nodes.


--  in do
--    map <- assignInts predicates
--    

programToGraph :: Ord r => Program r v -> Graph r v Int
programToGraph = fst . programToGraphWithTrace
