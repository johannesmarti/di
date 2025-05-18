module Data.AbstractTypeSignature (
  AbstractTypeSignature,
  AbstractTypedSchema,
) where

import qualified Data.Map.Strict as M

type AbstractTypeSignature t = [t]
type AbstractTypedSchema r t = M.Map r (AbstractTypeSignature t)

