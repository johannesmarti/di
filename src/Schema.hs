module Schema (
  Schema,
--  predicateSet,
) where

import Data.Map.Strict as Map
--import Data.Set as Set

type Schema r = Map r Int

--predicateSet :: Schema r -> Set r
--predicateSet = keysSet
