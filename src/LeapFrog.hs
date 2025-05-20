module LeapFrog (
  LeapFrog(..),
  toTupleList,
  conjunction,
  disjunction,
) where

import Data.Maybe
import Data.List (sortBy)
import Data.Ord (comparing)

data LeapFrog a = LeapFrog {
  current :: a,
  next :: Maybe (LeapFrog a),
  seek :: a -> Maybe (LeapFrog a),
  down :: Maybe (LeapFrog a)
}

-- Maybe it would be better to make this tail recursive
toTupleList :: Int -> Maybe (LeapFrog a) -> [[a]]
toTupleList 0 _ = [[]]
toTupleList _ Nothing = []
toTupleList n (Just frog) = let
    first = current frog
    firstTail = toTupleList (n - 1) (down frog)
    firsts = map (first:) firstTail
    rest = toTupleList n (next frog)
  in firsts ++ rest

conjunctionFromStableList :: Ord a => [LeapFrog a] -> LeapFrog a
conjunctionFromStableList frogs = let
    currentValue = current . head $ frogs
    definedNext = fromOperationOnFirst next frogs
    definedSeek value = fromOperationOnFirst (\f -> seek f value) frogs
    definedDown = do dFrogs <- traverse down frogs
                     conjunction dFrogs
  in LeapFrog currentValue definedNext definedSeek definedDown

stabilizeWorker :: Ord a => a -> [LeapFrog a] -> [LeapFrog a]
                              -> Maybe [LeapFrog a]
stabilizeWorker _ onValue [] = Just onValue
stabilizeWorker value onValue (f:fs) =
  case seek f value of
    Nothing -> Nothing
    Just f' -> let newValue = current f' in
        if newValue == value
          then stabilizeWorker value (f':onValue) fs
          else stabilizeWorker newValue [f'] (fs ++ onValue)

stabilize :: Ord a => LeapFrog a -> [LeapFrog a] -> Maybe [LeapFrog a]
stabilize target others = stabilizeWorker (current target) [target] others

fromOperationOnFirst :: Ord a => (LeapFrog a -> Maybe (LeapFrog a))
                                 -> [LeapFrog a] -> Maybe (LeapFrog a)
fromOperationOnFirst _ [] = error "not going to construct conjunction leapfrog for empty conjunction"
fromOperationOnFirst o (first:rest) = do
  first' <- o first
  stableList <- stabilize first' rest
  return $ conjunctionFromStableList stableList

conjunction :: Ord a => [LeapFrog a] -> Maybe (LeapFrog a)
conjunction frogs = fromOperationOnFirst Just frogs

disjunctionFromNonEmptyOrderedList :: Ord a => [LeapFrog a] -> LeapFrog a
disjunctionFromNonEmptyOrderedList orderedList = let
    -- here we could have better error reporting for the emptyList
    currentValue = current . head $ orderedList
    sameValue f = current f == currentValue
    disjunctionBehindPredicate predicate = let
        work [] = []
        work (f:fs) = if predicate f
                        then let fs' = case next f of
                                         Nothing -> fs
                                         Just f' -> insertOrderedFrog f' fs
                               in work fs'
                        else f:fs
      in disjunctionFromOrderedList (work orderedList)
    definedNext = disjunctionBehindPredicate sameValue
    definedSeek value = disjunctionBehindPredicate (\f -> current f < value)
    definedDown = disjunctionFromOrderedList . catMaybes . map down $
                    takeWhile sameValue orderedList
  in LeapFrog currentValue definedNext definedSeek definedDown

disjunctionFromOrderedList :: Ord a => [LeapFrog a] -> Maybe (LeapFrog a)
disjunctionFromOrderedList [] = Nothing
disjunctionFromOrderedList fs = Just $ disjunctionFromNonEmptyOrderedList fs

disjunction :: Ord a => [LeapFrog a] -> Maybe (LeapFrog a)
disjunction = disjunctionFromOrderedList . sortBy (comparing current)

-- TODO: is there a library for this?
insertOrderedBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertOrderedBy cmp value [] = [value]
insertOrderedBy cmp value (a:as) = if a `cmp` value == LT
                               then a : (insertOrderedBy cmp value as)
                               else value : (a:as)

insertOrderedFrog :: Ord a => LeapFrog a -> [LeapFrog a] -> [LeapFrog a]
insertOrderedFrog = insertOrderedBy (comparing current)


existential :: Ord a => Int -> LeapFrog a -> Maybe (LeapFrog a)
existential position frog = undefined


merge :: Ord a => Int -> Int -> LeapFrog a -> Maybe (LeapFrog a)
merge mergeFrom mergeTo frog = undefined

split :: Ord a => Int -> Int -> LeapFrog a -> Maybe (LeapFrog a)
split splitFrom splitTo frog = undefined

