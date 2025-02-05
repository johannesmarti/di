module ProgramToGraph (
  programToGraph
) where

import Control.Exception (assert)

import Data.Foldable (foldlM)
import Control.Monad.State.Strict
import Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set as Set

import Atom as At
import Graph
import Pretty
import Program
import Schema

type Node = Int

data PartialGraphData r v = PartialGraphData {
  outputVarMap :: Map Int (Set v),
  unfoldingMap :: Map Int (Unfolding r v Int)
}

emptyGraphData :: PartialGraphData r v
emptyGraphData = PartialGraphData Map.empty Map.empty

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

outVarsOfNode :: Int -> GraphConstructor r v (Set v)
outVarsOfNode n = do
  PartialGraphData outVarMap _ <- get
  return (outVarMap ! n)

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
        assert (outVarMap ! n == outputVariablesFromUnfolding (outVarMap !) uf) $
        PartialGraphData outVarMap (Map.insert n uf ufMap)

constructNode :: Ord v => Unfolding r v Int -> GraphConstructor r v Int
constructNode uf = do
  PartialGraphData outVarMap _ <- get
  let outVars = outputVariablesFromUnfolding (outVarMap !) uf
  n <- addNode outVars
  setNode n uf
  return n

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

type Constructor r v x = GraphConstructor r (DefinedVariable r v) x

constructBasePredicates :: (Ord r, Ord v) => Schema r
                                          -> Constructor r v (Map r Int)
constructBasePredicates schema = traverseWithKey mapper schema where
  mapper predicate arity = let
      argList = argumentListForPredicate predicate arity
      atom = At.Atom predicate argList
    in constructNode (Graph.Atom atom)

addDefinedPredicates :: (Ord r, Ord v) => Schema r
                                          -> Constructor r v (Map r Int)
addDefinedPredicates schema = traverseWithKey mapper schema where
  mapper predicate arity = addNode (outputVariablesForPredicate predicate arity)

projectSimpleRec :: Ord v => [v] -> Int -> GraphConstructor r v Int
projectSimpleRec [] continuationNode = return continuationNode
projectSimpleRec (nextVar:rest) continuationNode = do
  inner <- project rest continuationNode
  constructNode (Project nextVar inner)

projectTailRec :: Ord v => [v] -> Int -> GraphConstructor r v Int
projectTailRec [] continuationNode = return continuationNode
projectTailRec (nextVar:rest) continuationNode = do
  inner <- constructNode (Project nextVar continuationNode)
  project rest inner
  
project :: Ord v => [v] -> Int -> GraphConstructor r v Int
project = projectTailRec

embedEquality :: Ord v => Set v -> (v, v)
                 -> GraphConstructor r v Int
embedEquality context (a, b) = assert (a /= b) $
                               assert (a `Set.member` context) $
                               assert (b `Set.member` context) $ let
    varsToProject = Set.toList . Set.delete a . Set.delete b $ context
  in do eqNode <- constructNode (Equality a b)
        project varsToProject eqNode

constructEquality :: Ord v => Node -> (v, v) -> GraphConstructor r v Node
constructEquality baseNode (keptVar, removedVar) = do
  nextOutVars <- outVarsOfNode baseNode
  equality <- embedEquality nextOutVars (keptVar, removedVar)
  conjunction <- constructNode (And (Set.fromList [equality, baseNode]))
  constructNode (Exists removedVar conjunction)

constructAssign :: Ord v => Node -> (v, v) -> GraphConstructor r v Node
constructAssign baseNode (assignee, assigned) = do
  constructNode (Assign assignee assigned baseNode)

processArgumentListWorker :: Ord v => [(v,Int)] -> Map v Int -> [(Int,Int)]
                                      -> (Map v Int, [(Int,Int)])
processArgumentListWorker [] env eqs = (env, eqs)
processArgumentListWorker ((var, pos) : rest) env eqs = let
  (env', eqs') = case Map.lookup var env of
                   Just prevPos -> (env, (prevPos,pos):eqs)
                   Nothing      -> (Map.insert var pos env, eqs)
  in processArgumentListWorker rest env' eqs'

processArgumentList :: Ord v => [v] -> (Map v Int, [(Int, Int)])
processArgumentList args =
  processArgumentListWorker (zip args [0..]) Map.empty []

nodesForBodyAtom :: (Ord r, Ord v) => (r -> Int) -> Atom r v
                                      -> Constructor r v (Int, Set v)
nodesForBodyAtom nodeMap (At.Atom predicate args) = let
    node = nodeMap predicate
    (env, nakedEqs) = processArgumentList args
    e = PredicateVariable predicate
    eqs = fmap (\(a,b) -> (e a, e b)) nakedEqs
    assigns = fmap (\(a,b) -> (QuantifiedVariable a, e b)) . Map.toList $ env
  in do topLevelEquality <- foldlM constructEquality node eqs
        topLevelAssign <- foldlM constructAssign topLevelEquality assigns
        return (topLevelAssign, keysSet env)

embed :: (Ord r, Ord v) => Set v -> Set v -> Int -> Constructor r v Int
embed outerSet innerSet = let
    diff = outerSet `Set.difference` innerSet
    varsToProject = Prelude.map QuantifiedVariable . Set.toList $ diff
  in project varsToProject

bodyConjunction :: (Ord r, Ord v) => (r -> Int) -> [Atom r v]
                                     -> Constructor r v (Int, Set v)
bodyConjunction nodeMap bodyAtoms = do
  let mapper atom = nodesForBodyAtom nodeMap atom
  tupleList <- mapM mapper bodyAtoms
  let usedVariables = Prelude.foldl Set.union Set.empty
                                    (Prelude.map snd tupleList)
  let embedder = uncurry (flip (embed usedVariables))
  embedded <- traverse embedder tupleList
  conjunction <- constructNode (And (Set.fromList embedded))
  return (conjunction, usedVariables)


constructNodesForRule :: (Ord r, Ord v) => (r -> Int) -> Rule r v
                                           -> Constructor r v Int
constructNodesForRule predicateToNode rule = undefined
{-
constructNodesForRule predicateToNode rule = do
  let nodeDefinitions = undefined
  let ha = headAtom rule
  let headPredicate = predicateSymbol ha
  let arity = length $ arguments ha
  let outVars = outputVariablesForPredicate headPredicate arity
  headNode <- freshInt
  let topLevelDefinition = (headNode, And Set.empty, outVars)
  return (headNode, nodeDefinitions)
-}

nodesForDefinedPredicate :: (Ord r, Ord v) =>
                            (r -> Int) -> Program r v -> r -> Constructor r v ()
nodesForDefinedPredicate predicateToNode program predicate = do
  let rules = rulesForPredicate program predicate
  topLevelNodes <- mapM (constructNodesForRule predicateToNode) rules
  setNode (predicateToNode predicate) (Or (Set.fromList topLevelNodes))

constructGraphForProgram :: (Ord r, Ord v) =>
                                (Schema r, Schema r) -> Program r v
                                -> Constructor r v (Map r Int)
constructGraphForProgram (definedSchema, baseSchema) program = do
  baseMap <- constructBasePredicates baseSchema
  definedMap <- addDefinedPredicates definedSchema
  let combinedMap = definedMap `Map.union` baseMap
  let mapper = nodesForDefinedPredicate (combinedMap !) program
  _ <- traverse mapper (keys definedSchema)
  return combinedMap

programToGraphWithTrace :: (Ord r, Ord v) =>
                           (Schema r, Schema r) -> Program r v
                           -> (Graph r (DefinedVariable r v) Int, Map r Int)
programToGraphWithTrace schemas program = (intoGraph graphData, m) where
  (m, graphData) = runState (constructGraphForProgram schemas program)
                            emptyGraphData

programToGraph :: (Ord r, Ord v) => (Schema r, Schema r) -> Program r v
                                    -> Graph r (DefinedVariable r v) Int
programToGraph schemas program = fst $ programToGraphWithTrace schemas program
