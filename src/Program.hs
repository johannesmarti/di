module Program (
  Rule(Rule),
  headAtom,
  bodyAtoms,
  rulesForPredicate,
  Program,
  prettyProgram,
  showProgram,
) where

import Data.List (intercalate)

import Atom
import Pretty

instance (Show r, Show v) => Show (Atom r v) where
  show = prettyAtom show show

data Rule r v = Rule {
  headAtom :: Atom r v,
  bodyAtoms :: [Atom r v]
}

rulesForPredicate :: Eq r => Program r v -> r -> [Rule r v]
rulesForPredicate program predicate =
  filter ((== predicate) . predicateSymbol . headAtom) program

prettyRule :: (r -> String) -> (v -> String) -> Rule r v -> String
prettyRule pRel pVar (Rule h b) = let pAt = prettyAtom pRel pVar
  in pAt h ++ " <= " ++ intercalate ", " (map pAt b)

instance (Show r, Show v) => Show (Rule r v) where
  show = prettyRule show show

type Program r v = [Rule r v]

prettyProgramPrinter :: (r -> String) -> (v -> String) -> Program r v -> String
prettyProgramPrinter pRel pVar rules = let pRule r = prettyRule pRel pVar r ++ "\n"
  in concatMap pRule rules

showProgram :: (Show r, Show v) => Program r v -> String
showProgram = prettyProgramPrinter show show

prettyProgram :: (Pretty r, Pretty v) => Program r v -> String
prettyProgram = prettyProgramPrinter pretty pretty
