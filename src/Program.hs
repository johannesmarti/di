{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
-- All of these pragmas need are needed to make String an instance of a type class. lol

module Program (
  Atom(Atom),
  predicateSymbol,
  arguments,
  Rule(Rule),
  headAtom,
  bodyAtoms,
  Program,
  prettyAtom,
  prettyProgram,
  showProgram,
  Pretty,
  pretty,
) where

import Data.List (intercalate)

data Atom r v = Atom {
  predicateSymbol :: r,
  arguments       :: [v]
}

prettyAtom :: (r -> String) -> (v -> String) -> Atom r v -> String
prettyAtom pRel pVar (Atom rs args) = pRel rs ++ " " ++
                                                intercalate " " (map pVar args)

instance (Show r, Show v) => Show (Atom r v) where
  show = prettyAtom show show

data Rule r v = Rule {
  headAtom :: Atom r v,
  bodyAtoms :: [Atom r v]
}

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

class Pretty p where
  pretty :: p -> String

instance Pretty Char where
  pretty c = [c]

instance Pretty String where
  pretty = id

instance Pretty Int where
  pretty = show
