module ProgramToGraph (
  Node,
  programToGraph
) where

import Control.Exception (assert)

import Data.Foldable (foldlM)
import Control.Monad.State.Strict
import Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set as Set
import Data.Tuple (swap)

import Atom as At
import OperatorGraph
import Pretty
import Program
import Schema

type Node = Int

data PartialGraphData r v = PartialGraphData {
  outputVarMap :: Map Node (Set v),
  unfoldingMap :: Map Node (Operator r v Node)
}

emptyGraphData :: PartialGraphData r v
emptyGraphData = PartialGraphData Map.empty Map.empty

intoGraph :: Ord v => PartialGraphData r v -> Graph r v Node
intoGraph pgd = let
    ovs = outputVarMap pgd
    ufs = unfoldingMap pgd
    dom = assert (keysSet ovs == keysSet ufs) $ keysSet ovs
    f n = (ufs ! n, ovs ! n)
  in OperatorGraph.fromSet dom f

type GraphConstructor r v x = State (PartialGraphData r v) x

freshNode :: GraphConstructor r v Node
freshNode = do
  cs <- get
  return $ fromMaybe 0 (fmap ((+ 1) . fst) (Map.lookupMax (outputVarMap cs)))

outVarsOfNode :: Node -> GraphConstructor r v (Set v)
outVarsOfNode n = do
  PartialGraphData outVarMap _ <- get
  return (outVarMap ! n)

addNode :: Set v -> GraphConstructor r v Node
addNode outputVariables = do
  n <- freshNode
  cs <- get
  put $ cs { outputVarMap = Map.insert n outputVariables (outputVarMap cs) }
  return n

setNode :: Ord v => Node -> Operator r v Node -> GraphConstructor r v ()
setNode n uf = do
  PartialGraphData outVarMap ufMap <- get
  put $ assert (n `Map.member` outVarMap) $
        assert (not (n `Map.member` ufMap)) $
        assert (outVarMap ! n == outputVariablesOfOperator (outVarMap !) uf) $
        PartialGraphData outVarMap (Map.insert n uf ufMap)

constructNode :: Ord v => Operator r v Node -> GraphConstructor r v Node
constructNode uf = do
  PartialGraphData outVarMap _ <- get
  let outVars = outputVariablesOfOperator (outVarMap !) uf
  n <- addNode outVars
  setNode n uf
  return n

data DefinedVariable r v = PredicateVariable r Int | RuleVariable v
  deriving (Eq, Ord)

instance (Pretty r, Pretty v) => Pretty (DefinedVariable r v) where
  pretty (PredicateVariable r i) = pretty r ++ show i
  pretty (RuleVariable x) = pretty x

argumentListForPredicate :: r -> Int -> [DefinedVariable r v]
argumentListForPredicate predicate arity = 
  [PredicateVariable predicate pos | pos <- [0..(arity - 1)]]

outputVariablesForPredicate :: (Ord r, Ord v) => r -> Int
                                                 -> Set (DefinedVariable r v)
outputVariablesForPredicate predicate arity =
  Set.fromList $ argumentListForPredicate predicate arity

type Constructor r v x = GraphConstructor r (DefinedVariable r v) x

constructBasePredicates :: (Ord r, Ord v) => Schema r
                                          -> Constructor r v (Map r Node)
constructBasePredicates schema = traverseWithKey mapper schema where
  mapper predicate arity = let
      argList = argumentListForPredicate predicate arity
      atom = At.Atom predicate argList
    in constructNode (OperatorGraph.Atom atom)

addDefinedPredicates :: (Ord r, Ord v) =>
  Schema r -> Constructor r v (Map r Node)
addDefinedPredicates schema = traverseWithKey mapper schema where
  mapper predicate arity = addNode (outputVariablesForPredicate predicate arity)

constructSwitch :: Ord v => Node -> Switch v -> GraphConstructor r v Node
constructSwitch baseNode sw = do
  constructNode (VariableSwitch sw baseNode)

pairsToSuccessorMap :: (Ord a, Ord b) => [(a,b)] -> Map a (Set b)
pairsToSuccessorMap pairs = worker pairs Map.empty where
  worker [] m = m
  worker ((x,y):ps) m = worker ps (Map.alter (ins y) x m)
  ins y Nothing = Just (Set.singleton y)
  ins y (Just set) = Just (Set.insert y set)

processArgumentList :: Ord v => [v] -> [(v,Int)]
processArgumentList args = zip args [0..]

nodesForBodyAtom :: (Ord r, Ord v) => (r -> Node) -> Atom r v
                                      -> Constructor r v (Node, Set v)
nodesForBodyAtom nodeMap (At.Atom predicate args) = let
    node = nodeMap predicate
    env = pairsToSuccessorMap . processArgumentList $ args
    toSwitch (v, posSet) = Switch (Set.singleton (RuleVariable v))
                                  (Set.map (PredicateVariable predicate) posSet)
    switches = Prelude.map toSwitch (Map.toList env)
  in do topLevelSwitch <- foldlM constructSwitch node switches
        return (topLevelSwitch, keysSet env)

quantifyAway :: Ord v => Node -> v -> GraphConstructor r v Node
quantifyAway baseNode variable =
  constructNode (existsOperator variable baseNode)

projectAway :: Ord v => Node -> v -> GraphConstructor r v Node
projectAway baseNode variable =
  constructNode (projectOperator variable baseNode)

embed :: (Ord r, Ord v) => Set v -> Set v -> Node -> Constructor r v Node
embed outerSet innerSet node = let
    diff = outerSet `Set.difference` innerSet
    varsToProject = Prelude.map RuleVariable . Set.toList $ diff
  in foldM projectAway node varsToProject

bodyConjunction :: (Ord r, Ord v) => (r -> Node) -> [Atom r v]
                                     -> Constructor r v (Node, Set v)
bodyConjunction predicateToNode bodyAtoms = do
  let mapper atom = nodesForBodyAtom predicateToNode atom
  tupleList <- mapM mapper bodyAtoms
  let usedVariables = Prelude.foldl Set.union Set.empty
                                    (Prelude.map snd tupleList)
  let embedder = uncurry (flip (embed usedVariables))
  embedded <- traverse embedder tupleList
  conjunction <- constructNode (And (Set.fromList embedded))
  return (conjunction, usedVariables)

constructNodesForRule :: (Ord r, Ord v) => (r -> Node) -> Rule r v
                                           -> Constructor r v Node
constructNodesForRule predicateToNode (Rule headAtom bodyAtoms) = let
    At.Atom headPredicate headArgs = headAtom
    answerVars = Set.fromList headArgs
    env = pairsToSuccessorMap (Prelude.map swap (processArgumentList headArgs))
    e = PredicateVariable headPredicate
    toSwitch (pos, varSet) = Switch (Set.singleton (e pos))
                                    (Set.map RuleVariable varSet)
    switches = Prelude.map toSwitch (Map.toList env)
  in do
    (bodyNode, usedVars) <- bodyConjunction predicateToNode bodyAtoms
    let diff = usedVars `Set.difference` answerVars
    let toQuantifyAway = Prelude.map RuleVariable (Set.toList diff)
    quantifiedNode <- foldM quantifyAway bodyNode toQuantifyAway
    foldM constructSwitch quantifiedNode switches

nodesForDefinedPredicate :: (Ord r, Ord v) =>
  (r -> Node) -> Program r v -> r -> Constructor r v ()
nodesForDefinedPredicate predicateToNode program predicate = do
  let rules = rulesForPredicate program predicate
  topLevelNodes <- mapM (constructNodesForRule predicateToNode) rules
  setNode (predicateToNode predicate) (Or (Set.fromList topLevelNodes))

constructGraphForProgram :: (Ord r, Ord v) =>
  (Schema r, Schema r) -> Program r v -> Constructor r v (Map r Node)
constructGraphForProgram (definedSchema, baseSchema) program = do
  baseMap <- constructBasePredicates baseSchema
  definedMap <- addDefinedPredicates definedSchema
  let combinedMap = definedMap `Map.union` baseMap
  let mapper = nodesForDefinedPredicate (combinedMap !) program
  _ <- traverse mapper (keys definedSchema)
  return combinedMap

programToGraphWithTrace :: (Ord r, Ord v) =>
  (Schema r, Schema r) -> Program r v
  -> (Graph r (DefinedVariable r v) Node, Map r Node)
programToGraphWithTrace schemas program = (intoGraph graphData, m) where
  (m, graphData) = runState (constructGraphForProgram schemas program)
                            emptyGraphData

programToGraph :: (Ord r, Ord v) => (Schema r, Schema r) -> Program r v
                                    -> Graph r (DefinedVariable r v) Node
programToGraph schemas program = fst $ programToGraphWithTrace schemas program
