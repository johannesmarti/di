{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
-- All of these pragmas need are needed to make String an instance of a type class. lol

module Pretty (
  Pretty,
  pretty
) where

class Pretty p where
  pretty :: p -> String

instance Pretty Char where
  pretty c = [c]

instance Pretty String where
  pretty = id

instance Pretty Int where
  pretty = show
