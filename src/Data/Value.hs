module Data.Value (
  Type(..),
  TypeSignature,
  TypedSchema,
  Value(..),
  Tuple,
  tupleType,
  prettyTuple
) where

import Data.AbstractTypeSignature

data Type = IntType
          | StringType
          | FloatType
  deriving Eq

instance Show Type where
  show IntType = "int"
  show StringType = "string"
  show FloatType = "float"

type TypeSignature = AbstractTypeSignature Type
type TypedSchema p = AbstractTypedSchema p Type

data Value = IntValue Int
           | StringValue String
           | FloatValue Float
  deriving (Eq, Ord, Show)

valueType :: Value -> Type
valueType (IntValue _) = IntType
valueType (StringValue _) = StringType
valueType (FloatValue _) = FloatType

type Tuple = [Value]

tupleType :: Tuple -> TypeSignature
tupleType = map valueType

prettyTuple :: Tuple -> String
prettyTuple [] = ""
prettyTuple ((IntValue v):vs) = show v ++ "\t" ++ prettyTuple vs
prettyTuple ((StringValue v):vs) = v ++ "\t" ++ prettyTuple vs
prettyTuple ((FloatValue v):vs) = show v ++ "\t" ++ prettyTuple vs

