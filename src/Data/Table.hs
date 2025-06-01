module Data.Table (
  Table,
  empty,
  isEmpty,
  singleton,
  insert,
  loadTupleList,
  toTupleList,
  fromTupleList,
  merge,
  contains,
  typeSignature,
  toLeapFrog,
) where

import Control.Exception (assert)

import qualified Data.Map.Strict as Map

import Data.Value
import LeapFrog hiding (End, toTupleList)
import qualified LeapFrog as LF

-- TODO: should have a function that populates a table directely from a leap frog
-- TODO: it might be better to have the LeapFrog related code in the LeapFrog module and not this Table Module

data TypedMap x = IntMap (Map.Map Int x)
                | StringMap (Map.Map String x)
                | FloatMap (Map.Map Float x)
  deriving Eq

data NonEmptyTable = End | Layer (TypedMap NonEmptyTable)
  deriving Eq
data Table = Empty TypeSignature | NonEmpty NonEmptyTable
  deriving Eq

empty :: TypeSignature -> Table
empty = Empty

isEmpty :: Table -> Bool
isEmpty (Empty _) = True
isEmpty (NonEmpty _) = False

singleton' :: Tuple -> NonEmptyTable
singleton' [] = End
singleton' ((IntValue v):vs) = Layer (IntMap (Map.singleton v (singleton' vs)))
singleton' ((StringValue v):vs) = Layer (StringMap (Map.singleton v (singleton' vs)))
singleton' ((FloatValue v):vs) = Layer (FloatMap (Map.singleton v (singleton' vs)))

singleton :: Tuple -> Table
singleton t = NonEmpty (singleton' t)

toTupleList :: Table -> [Tuple]
toTupleList (Empty _) = []
toTupleList (NonEmpty table) = worker table where
  worker End = [[]]
  worker (Layer (IntMap m)) =
    concatMap (\(k, v) -> map (IntValue k:) (worker v)) (Map.toList m)
  worker (Layer (StringMap m)) =
    concatMap (\(k, v) -> map (StringValue k:) (worker v)) (Map.toList m)
  worker (Layer (FloatMap m)) =
    concatMap (\(k, v) -> map (FloatValue k:) (worker v)) (Map.toList m)

-- At the moment insert accepts mismatching tuple types if there table is
-- empty or if the mismatch happens on a branch of the trie that is still
-- empty.
insert :: Tuple -> Table -> Table
insert tuple (Empty s) = assert (tupleType tuple == s) $ singleton tuple
insert tuple (NonEmpty table) = NonEmpty (worker tuple table) where
  worker [] End = End
  worker [] (Layer _) = error "insert: tuple is shorter than table schema"
  worker ((IntValue v):vs) (Layer (IntMap m)) =
    Layer (IntMap (Map.insertWith (\_ -> worker vs) v (singleton' vs) m))
  worker ((StringValue v):vs) (Layer (StringMap m)) =
    Layer (StringMap (Map.insertWith (\_ -> worker vs) v (singleton' vs) m))
  worker ((FloatValue v):vs) (Layer (FloatMap m)) =
    Layer (FloatMap (Map.insertWith (\_ -> worker vs) v (singleton' vs) m))
  worker _ End = error "insert: tuple is longer than table schema"
  worker _ _ = error "insert: tuple and table types mismatch"

loadTupleList :: [Tuple] -> Table -> Table
loadTupleList l table = foldr insert table l

fromTupleList :: TypeSignature -> [Tuple] -> Table
fromTupleList signature = flip loadTupleList (empty signature)

merge :: Table -> Table -> Table
merge (Empty _) t = t
merge t (Empty _) = t
merge (NonEmpty t1) (NonEmpty t2) = NonEmpty (merge' t1 t2) where
  merge' End End = End
  merge' (Layer (IntMap m1)) (Layer (IntMap m2)) =
    Layer (IntMap (Map.unionWith merge' m1 m2))
  merge' (Layer (StringMap m1)) (Layer (StringMap m2)) =
    Layer (StringMap (Map.unionWith merge' m1 m2))
  merge' (Layer (FloatMap m1)) (Layer (FloatMap m2)) =
    Layer (FloatMap (Map.unionWith merge' m1 m2))
  merge' _ _ = error "merge: table type schemas mismatch"

contains :: Table -> Tuple -> Bool
contains (Empty _) _ = False
contains (NonEmpty table) tuple = contains' table tuple where
  contains' End [] = True
  contains' (Layer (IntMap m)) ((IntValue v):vs) =
    case Map.lookup v m of
      Just t -> contains' t vs
      Nothing -> False
  contains' (Layer (StringMap m)) ((StringValue v):vs) =
    case Map.lookup v m of
      Just t -> contains' t vs
      Nothing -> False
  contains' (Layer (FloatMap m)) ((FloatValue v):vs) =
    case Map.lookup v m of
      Just t -> contains' t vs
      Nothing -> False
  contains' _ _ = error "contains: table and tuple type signatures mismatch"

typeSignature' :: NonEmptyTable -> TypeSignature
typeSignature' End = []
typeSignature' (Layer (IntMap m)) = IntType : typeSignature' (snd (Map.findMin m))
typeSignature' (Layer (StringMap m)) = StringType : typeSignature' (snd (Map.findMin m))
typeSignature' (Layer (FloatMap m)) = FloatType : typeSignature' (snd (Map.findMin m))

typeSignature :: Table -> TypeSignature
typeSignature (Empty sig) = sig
typeSignature (NonEmpty table) = typeSignature' table

prettyTupleList :: [Tuple] -> String
prettyTupleList = unlines . map prettyTuple

instance Show Table where
  show = prettyTupleList . toTupleList

nextOnMap :: Map.Map k v -> Maybe (Map.Map k v)
nextOnMap m = let advanced = Map.deleteMin m
  in if Map.null advanced then Nothing else Just advanced

seekOnMap :: Ord k => k -> Map.Map k a -> Maybe (Map.Map k a)
seekOnMap k m = let (_, maybeValue, rest) = Map.splitLookup k m
               in case maybeValue of
                    Just value -> Just $ Map.insert k value rest
                    Nothing -> if Map.null rest then Nothing else Just rest

seekTypeWorker :: Value -> (TypedMap NonEmptyTable)
                  -> Maybe (TypedMap NonEmptyTable)
seekTypeWorker (IntValue v) (IntMap m) = fmap IntMap (seekOnMap v m)
seekTypeWorker (StringValue v) (StringMap m) = fmap StringMap (seekOnMap v m)
seekTypeWorker (FloatValue v) (FloatMap m) = fmap FloatMap (seekOnMap v m)
seekTypeWorker _ _ = error "seek: type mismatch"

typedMapToLeapFrog :: TypedMap NonEmptyTable -> LeapFrog Value
typedMapToLeapFrog typedMap = let
    minKey = fst . Map.findMin
    definedCurrent = case typedMap of
                       IntMap    m -> IntValue    (minKey m)
                       StringMap m -> StringValue (minKey m)
                       FloatMap  m -> FloatValue  (minKey m)
    lfFromTMap = nonEmptyToLeapFrog . Layer
    definedNext = case typedMap of
           IntMap    m -> fmap (typedMapToLeapFrog . IntMap   ) $ nextOnMap m
           StringMap m -> fmap (typedMapToLeapFrog . StringMap) $ nextOnMap m
           FloatMap  m -> fmap (typedMapToLeapFrog . FloatMap ) $ nextOnMap m
    definedSeek value = fmap typedMapToLeapFrog (seekTypeWorker value typedMap)
    downOnMap = Just . nonEmptyToLeapFrog . snd . Map.findMin
    definedDown = case typedMap of
                    IntMap    m -> downOnMap m
                    StringMap m -> downOnMap m
                    FloatMap  m -> downOnMap m
  in LeapFrog definedCurrent definedNext definedSeek definedDown

nonEmptyToLeapFrog :: NonEmptyTable -> FrogOrEnd Value
nonEmptyToLeapFrog End = LF.End
nonEmptyToLeapFrog (Layer typedMap) = Frog (typedMapToLeapFrog typedMap)

toLeapFrog :: Table -> Maybe (FrogOrEnd Value)
toLeapFrog (Empty _) = Nothing
toLeapFrog (NonEmpty t) = Just (nonEmptyToLeapFrog t)
