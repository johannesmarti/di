module Atom (
  Atom(Atom),
  predicateSymbol,
  arguments,
  prettyAtom,
) where

import Data.List (intercalate)

data Atom r v = Atom {
  predicateSymbol :: r,
  arguments       :: [v]
}

prettyAtom :: (r -> String) -> (v -> String) -> Atom r v -> String
prettyAtom pRel pVar (Atom rs args) = pRel rs ++ " " ++
                                                intercalate " " (map pVar args)

