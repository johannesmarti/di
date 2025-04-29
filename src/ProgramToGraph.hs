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
        assert (outVarMap ! n == outputVariablesFromOperator (outVarMap !) uf) $
        PartialGraphData outVarMap (Map.insert n uf ufMap)

constructNode :: Ord v => Operator r v Node -> GraphConstructor r v Node
constructNode uf = do
  PartialGraphData outVarMap _ <- get
  let outVars = outputVariablesFromOperator (outVarMap !) uf
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

addDefinedPredicates :: (Ord r, Ord v) => Schema r
                                          -> Constructor r v (Map r Node)
addDefinedPredicates schema = traverseWithKey mapper schema where
  mapper predicate arity = addNode (outputVariablesForPredicate predicate arity)

projectSimpleRec :: Ord v => [v] -> Node -> GraphConstructor r v Node
projectSimpleRec [] continuationNode = return continuationNode
projectSimpleRec (nextVar:rest) continuationNode = do
  inner <- project rest continuationNode
  constructNode (Project nextVar inner)

projectTailRec :: Ord v => [v] -> Node -> GraphConstructor r v Node
projectTailRec [] continuationNode = return continuationNode
projectTailRec (nextVar:rest) continuationNode = do
  inner <- constructNode (Project nextVar continuationNode)
  project rest inner
  
project :: Ord v => [v] -> Node -> GraphConstructor r v Node
project = projectTailRec

embedEquality :: Ord v => Set v -> (v, v)
                 -> GraphConstructor r v Node
embedEquality context (a, b) = assert (a /= b) $
                               assert (a `Set.member` context) $
                               assert (b `Set.member` context) $ let
    varsToProject = Set.toList . Set.delete a . Set.delete b $ context
  in do eqNode <- constructNode (Equality a b)
        project varsToProject eqNode

quantifyAway :: Ord v => Node -> v -> GraphConstructor r v Node
quantifyAway baseNode variable = constructNode (Exists variable baseNode)

constructAssign :: Ord v => Node -> (v, v) -> GraphConstructor r v Node
constructAssign baseNode (assignee, assigned) = do
  constructNode (Assign assignee assigned baseNode)

constructExiEquality :: Ord v => Node -> (v, v) -> GraphConstructor r v Node
constructExiEquality baseNode (keptVar, removedVar) = do
  nextOutVars <- outVarsOfNode baseNode
  equality <- embedEquality nextOutVars (keptVar, removedVar)
  conjunction <- constructNode (And (Set.fromList [equality, baseNode]))
  quantifyAway conjunction removedVar

constructProjEquality :: Ord v => Node -> (v, v) -> GraphConstructor r v Node
constructProjEquality baseNode (keptVar, projectedVar) = do
  inner <- constructNode (Project projectedVar baseNode)
  nextOutVars <- outVarsOfNode inner
  equality <- embedEquality nextOutVars (keptVar, projectedVar)
  constructNode (And (Set.fromList [equality, inner]))

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

nodesForBodyAtom :: (Ord r, Ord v) => (r -> Node) -> Atom r v
                                      -> Constructor r v (Node, Set v)
nodesForBodyAtom nodeMap (At.Atom predicate args) = let
    node = nodeMap predicate
    (env, nakedEqs) = processArgumentList args
    e = PredicateVariable predicate
    eqs = Prelude.map (\(a,b) -> (e a, e b)) nakedEqs
    assigns = Prelude.map (\(a,b) -> (RuleVariable a, e b)) . Map.toList $ env
  in do topLevelEquality <- foldlM constructExiEquality node eqs
        topLevelAssign <- foldlM constructAssign topLevelEquality assigns
        return (topLevelAssign, keysSet env)

embed :: (Ord r, Ord v) => Set v -> Set v -> Node -> Constructor r v Node
embed outerSet innerSet = let
    diff = outerSet `Set.difference` innerSet
    varsToProject = Prelude.map RuleVariable . Set.toList $ diff
  in project varsToProject

bodyConjunction :: (Ord r, Ord v) => (r -> Node) -> [Atom r v]
                                     -> Constructor r v (Node, Set v)
bodyConjunction nodeMap bodyAtoms = do
  let mapper atom = nodesForBodyAtom nodeMap atom
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
    (At.Atom headPredicate headArgs) = headAtom
    (env, nakedEqs) = processArgumentList headArgs
    answerVars = Map.keysSet env
    e = PredicateVariable headPredicate
    eqs = Prelude.map (\(a,b) -> (e a, e b)) nakedEqs
    assigns = Prelude.map (\(a,b) -> (e b, RuleVariable a)) . Map.toList $ env
  in do
    (bodyNode, usedVars) <- bodyConjunction predicateToNode bodyAtoms
    let diff = usedVars `Set.difference` answerVars
    let toQuantifyAway = Prelude.map RuleVariable (Set.toList diff)
    quantifiedNode <- foldM quantifyAway bodyNode toQuantifyAway
    topLevelAssign <- foldM constructAssign quantifiedNode assigns
    foldM constructProjEquality topLevelAssign eqs

nodesForDefinedPredicate :: (Ord r, Ord v) =>
                            (r -> Node) -> Program r v -> r -> Constructor r v ()
nodesForDefinedPredicate predicateToNode program predicate = do
  let rules = rulesForPredicate program predicate
  topLevelNodes <- mapM (constructNodesForRule predicateToNode) rules
  setNode (predicateToNode predicate) (Or (Set.fromList topLevelNodes))

constructGraphForProgram :: (Ord r, Ord v) =>
                                (Schema r, Schema r) -> Program r v
                                -> Constructor r v (Map r Node)
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
