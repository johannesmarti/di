module ProgramToGraph (
  programToGraph
) where

import Control.Exception (assert)

import Control.Monad.State.Strict
import Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set as Set

import Atom as At
import Graph
import Pretty
import Program
import Schema

data DefinedVariable r v = PredicateVariable r Int |
                           QuantifiedVariable v
  deriving (Eq, Ord)

instance (Pretty r, Pretty v) => Pretty (DefinedVariable r v) where
  pretty (PredicateVariable r i) = pretty r ++ show i
  pretty (QuantifiedVariable x) = pretty x

argumentListForPredicate :: r -> Int -> [DefinedVariable r v]
argumentListForPredicate predicate arity = 
  [PredicateVariable predicate pos | pos <- [0..(arity - 1)]]

outputVariablesForPredicate :: (Ord r, Ord v) => r -> Int
                                                 -> Set (DefinedVariable r v)
outputVariablesForPredicate predicate arity =
  Set.fromList $ argumentListForPredicate predicate arity

-- This is a partial input to the fromTupleList function that constructs a Graph
type NodeDefinitions r v =
  [(Int, Unfolding r (DefinedVariable r v) Int, Set (DefinedVariable r v))]

data PartialGraphData r v = PartialGraphData {
  outputVarMap :: Map Int (Set v),
  unfoldingMap :: Map Int (Unfolding r v Int)
}

intoGraph :: Ord v => PartialGraphData r v -> Graph r v Int
intoGraph pgd = let
    ovs = outputVarMap pgd
    ufs = unfoldingMap pgd
    dom = assert (keysSet ovs == keysSet ufs) $ keysSet ovs
    f n = (ufs ! n, ovs ! n)
  in Graph.fromSet dom f

type GraphConstructor r v x = State (PartialGraphData r v) x

freshInt :: GraphConstructor r v Int
freshInt = do
  cs <- get
  return $ fromMaybe 0 (fmap ((+ 1) . fst) (Map.lookupMax (outputVarMap cs)))

addNode :: Set v -> GraphConstructor r v Int
addNode outputVariables = do
  n <- freshInt
  cs <- get
  put $ cs { outputVarMap = Map.insert n outputVariables (outputVarMap cs) }
  return n

setNode :: Ord v => Int -> Unfolding r v Int -> GraphConstructor r v ()
setNode n uf = do
  PartialGraphData outVarMap ufMap <- get
  put $ assert (n `Map.member` outVarMap) $
        assert (not (n `Map.member` ufMap)) $
        assert (outVarMap Map.! n == outputVariablesFromUnfolding
                                        (outVarMap Map.!) uf) $
        PartialGraphData outVarMap (Map.insert n uf ufMap)

constructNode :: Ord v => Unfolding r v Int -> GraphConstructor r v Int
constructNode uf = do
  PartialGraphData outVarMap _ <- get
  let outVars = outputVariablesFromUnfolding (outVarMap Map.!) uf
  n <- addNode outVars
  setNode n uf
  return n

{-

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

generateAtomNode :: (Ord r, Ord v) => (r -> Int) -> (r, Int)
                    -> ((Int, Unfolding r (DefinedVariable r v) Int, Set (DefinedVariable r v)))
generateAtomNode assignInt (predicate, arity) =
  (assignInt predicate, Graph.Atom atom, outVars) where
    argList = argumentListForPredicate predicate arity
    atom = At.Atom predicate argList
    outVars = Set.fromList argList

project :: Ord v => [v] -> Int -> FreshIntState (Int, [(Int, Unfolding r v Int, Set v)])
project varsToProject continuationNode = undefined
  -- How to handle the outputVariables here?

embedEquality :: Ord v => Set v -> (v, v)
                 -> FreshIntState (Int, [(Int, Unfolding r v Int, Set v)])
embedEquality context (a, b) = assert (a /= b) $
                               assert (a `Set.member` context) $
                               assert (b `Set.member` context) $ let
    worker (n,ctx) accum [] = return (n,accum)
    worker (n,ctx) accum (nextVar:rest) = do
      let outVars = Set.insert nextVar ctx
      newNode <- freshInt
      let newDef = (newNode, Project nextVar n, outVars)
      worker (newNode, outVars) (newDef:accum) rest
    startCtx = Set.fromList [a, b]
    varsToProject = Set.toList . Set.delete a . Set.delete b $ context
  in do eqNode <- freshInt
        worker (eqNode, startCtx) [(eqNode, Equality a b, startCtx)]
               varsToProject

bodyAtomWorker :: (Ord r, Ord v) =>
                    (r, Int) -> Int -> [(v, Int)] -> Map v Int
                    -> FreshIntState (Int, Set (DefinedVariable r v),
                                      NodeDefinitions r v, Set v)
bodyAtomWorker (predicate, arity) innerNode [] env =
  return (innerNode, outputVariablesForPredicate predicate arity,
          [], keysSet env)
bodyAtomWorker predInfo innerNode ((var, pos):rest) env =
  case Map.lookup var env of
    Just pp -> do
      let removedVar = PredicateVariable (fst predInfo) pos
      let prevVar = PredicateVariable (fst predInfo) pp
      (nextNode, nextOut, nextDefs, used) <- bodyAtomWorker predInfo
                                                   innerNode rest env
      (equality, eqDefs) <- embedEquality nextOut (prevVar, removedVar)
      conjunction <- freshInt
      let conDef = (conjunction, And (Set.fromList [equality, nextNode]),
                    nextOut)
      let outVars = Set.delete removedVar nextOut
      newNode <- freshInt
      let newDef = (newNode, Exists removedVar conjunction, outVars)
      let allDefs = newDef:conDef:(eqDefs ++ nextDefs)
      return (newNode, outVars, allDefs, used)
    Nothing -> do
      let removedVar = PredicateVariable (fst predInfo) pos
      let newVar = QuantifiedVariable var
      (nextNode, nextOut, nextDefs, used) <- bodyAtomWorker predInfo
                                                   innerNode rest
                                                   (Map.insert var pos env)
      newNode <- freshInt
      let outVars = Set.insert newVar (Set.delete removedVar nextOut)
      let newDefinition = (newNode, Assign newVar removedVar nextNode, outVars)
      return (newNode, outVars, newDefinition : nextDefs, used)

nodesForBodyAtom :: (Ord r, Ord v) => (r -> Int) -> Atom r v
                                      -> FreshIntState (Int,
                                                        NodeDefinitions r v,
                                                        Set v)
nodesForBodyAtom nodeMap (At.Atom predicate args) = let
    projectOut (a,b,c,d) = (a,c,d)
    arity = length args
    node = nodeMap predicate
    posArgs = zip args [0..]
  in fmap projectOut (bodyAtomWorker (predicate, arity) node posArgs Map.empty)

someFunction :: (r -> Int) -> FreshIntState ()
someFunction nodeMap = do
  let mapper atom = nodesForBodyAtom nodeMap atom
  tupleList <- mapM mapper (bodyAtoms rule)
  let usedVariables = Prelude.foldl Set.union (Prelude.map (\(_,_,s) -> s) tupleList)
  return ()

-- TODO
processHeadArgumentList :: (Ord r, Ord v) =>
                           Atom r v
                           -> (Map v (DefinedVariable r v), [(r, v)])
processHeadArgumentList = undefined
 -- this is going to be applied to argument list of the head and
 -- defines a mapping from some of the variables to DefinedVariables. Plus
 -- we are going to get the equalities that need to be estaplished in for
 -- instance R x y :- which becomes R r0 r1 :- r0 = r1


nodesForRule :: (Ord r, Ord v) => (r -> Int) -> Rule r v
                                  -> FreshIntState (Int, NodeDefinitions r v)
nodesForRule predicateToNode rule = do
  let nodeDefinitions = undefined
  let ha = headAtom rule
  let headPredicate = predicateSymbol ha
  let arity = length $ arguments ha
  let outVars = outputVariablesForPredicate headPredicate arity
  headNode <- freshInt
  let topLevelDefinition = (headNode, And Set.empty, outVars)
  return (headNode, nodeDefinitions)

rulesForPredicate :: Eq r => Program r v -> r -> [Rule r v]
rulesForPredicate program predicate =
  Prelude.filter ((== predicate) . At.predicateSymbol . headAtom) program

nodesForDefinedPredicate :: (Ord r, Ord v) =>
                            (r -> Int) -> Program r v -> (r, Int)
                            -> FreshIntState (NodeDefinitions r v)
nodesForDefinedPredicate predicateToNode program (predicate, arity) = do
  let rules = rulesForPredicate program predicate
  l <- mapM (nodesForRule predicateToNode) rules
  let topLevelNodes = Prelude.map fst l
  let allDefinitions = concat (Prelude.map snd l)
  let headNode = predicateToNode predicate
  let outVars = outputVariablesForPredicate predicate arity
  let topLevelDefinition = (headNode, Or (Set.fromList topLevelNodes), outVars)
  return $ topLevelDefinition : allDefinitions

programToGraphWithTrace :: (Ord r, Ord v) => (Schema r, Schema r) -> Program r v
                            -> (Graph r (DefinedVariable r v) Int, Map r Int)
programToGraphWithTrace (definedSchema, baseSchema) program =
  evalState computation 0 where
    basePredicates = Map.keysSet baseSchema
    definedPredicates = Map.keysSet definedSchema
    computation = do
      baseMap <- assignInts basePredicates
      let baseDefinitions = Prelude.map (generateAtomNode (\p -> baseMap ! p))
                                        (Map.toList baseSchema)
      definedMap <- assignInts definedPredicates
      let combinedMap = definedMap `Map.union` baseMap
      let predicateToNode p = combinedMap ! p
      let mapper = nodesForDefinedPredicate predicateToNode program
      allNodeDefinitions <- fmap concat $ mapM mapper (Map.toList definedSchema)
      let graph = Graph.fromTupleList allNodeDefinitions
      return (graph, combinedMap)

programToGraph :: (Ord r, Ord v) => (Schema r, Schema r) -> Program r v
                                    -> Graph r (DefinedVariable r v) Int
programToGraph schema program =
  fst $ programToGraphWithTrace schema program

-}
