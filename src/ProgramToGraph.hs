module ProgramToGraph (
  programToGraph
) where

import Control.Monad.State.Strict
import Data.Map.Strict as Map
import Data.Set as Set

import Atom as At
import Program
import Graph
import Schema

data DefinedVariable r v = PredicateVariable r Int |
                           QuantifiedVariable v
  deriving (Show, Eq, Ord)

outputVariables :: (Ord r, Ord v) => r -> Int -> Set (DefinedVariable r v)
outputVariables predicate arity =
  Set.fromList [PredicateVariable predicate pos | pos <- [0..(arity - 1)]]

-- This is a partial input to the fromTupleList function that constructs a Graph
type NodeDefinitions r v =
  [(Int, Unfolding r (DefinedVariable r v) Int, Set (DefinedVariable r v))]


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

nodesForRule :: Ord r => (r -> Int) -> Rule r v
                         -> FreshIntState (Int, NodeDefinitions r v)
nodesForRule predicateToNode rule = undefined

rulesForPredicate :: Eq r => Program r v -> r -> [Rule r v]
rulesForPredicate program predicate =
  Prelude.filter ((== predicate) . At.predicateSymbol . headAtom) program

nodesForDefinedPredicate :: (Ord r, Ord v) =>
                            (r -> Int) -> Program r v -> (r, Int)
                            -> FreshIntState (NodeDefinitions r v)
nodesForDefinedPredicate predicateToNode program (predicate, arity) = do
  -- TODO: adapt this to also make sense of the head vars.
  let rules = rulesForPredicate program predicate
  l <- mapM (nodesForRule predicateToNode) rules
  let topLevelNodes = Prelude.map fst l
  let allDefinitions = concat (Prelude.map snd l)
  headNode <- freshInt
  let outVars = outputVariables predicate arity
  let topLevelDefinition = (headNode, And (Set.fromList topLevelNodes), outVars)
  return $ topLevelDefinition : allDefinitions

programToGraphWithTrace :: (Ord r, Ord v) => Schema r -> Program r v
                            -> (Graph r (DefinedVariable r v) Int, Map r Int)
programToGraphWithTrace schema program =
  evalState computation 0 where
    definedPredicates = Map.keysSet schema
    computation = do
      definedMap <- assignInts definedPredicates
      let predicateToNode p = definedMap ! p
      let mapper = nodesForDefinedPredicate predicateToNode program
      allNodeDefinitions <- fmap concat $ mapM mapper (Map.toList schema)
      let graph = Graph.fromTupleList allNodeDefinitions
      return (graph, definedMap)

programToGraph :: (Ord r, Ord v) => Schema r -> Program r v
                                    -> Graph r (DefinedVariable r v) Int
programToGraph schema program =
  fst $ programToGraphWithTrace schema program
