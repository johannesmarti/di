module LeapFrog (
  FrogOrEnd(..),
  LeapFrog(..),
  maybeFrogOrEndToTupleList,
  frogToTupleList,
  forceFrog,
  toTupleList,
  conjunction,
  disjunction,
  existential,
  existentialOnLast,
  merge,
) where

import Data.Filtrable (mapEither)
import Data.Maybe
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Exception (assert)

data LeapFrog a = LeapFrog {
  current :: a,
  next :: Maybe (LeapFrog a),
  -- I am assuing that the value that we are seeking for is always
  -- bigger than the current value. Is this sane?
  seek :: a -> Maybe (LeapFrog a),
  down :: Maybe (FrogOrEnd a)
}

data FrogOrEnd a = End | Frog (LeapFrog a)

forceFrog :: String -> FrogOrEnd a -> LeapFrog a
forceFrog operationName End = error ("unexpected end in " ++ operationName)
forceFrog operationName (Frog f) = f

pushPastEnd :: [FrogOrEnd a] -> Maybe [LeapFrog a]
pushPastEnd endOrFrogs = let
    f End = Left ()
    f (Frog frog) = Right frog
    (ends,frogs) = mapEither f endOrFrogs
  in if null ends
       then Just frogs
       else assert (null frogs) $ Nothing

-- Maybe it would be better to make this tail recursive
maybeFrogOrEndToTupleList :: Maybe (FrogOrEnd a) -> [[a]]
maybeFrogOrEndToTupleList Nothing = []
maybeFrogOrEndToTupleList (Just End) = [[]]
maybeFrogOrEndToTupleList (Just (Frog frog)) = let
    first = current frog
    firstTail = maybeFrogOrEndToTupleList (down frog)
    firsts = map (first:) firstTail
    rest = maybeFrogToTupleList (next frog)
  in firsts ++ rest

maybeFrogToTupleList :: Maybe (LeapFrog a) -> [[a]]
maybeFrogToTupleList Nothing = []
maybeFrogToTupleList (Just frog) = let
    first = current frog
    firstTail = maybeFrogOrEndToTupleList (down frog)
    firsts = map (first:) firstTail
    rest = maybeFrogToTupleList (next frog)
  in firsts ++ rest

toTupleList :: Maybe (LeapFrog a) -> [[a]]
toTupleList = maybeFrogToTupleList

frogToTupleList :: LeapFrog a -> [[a]]
frogToTupleList = toTupleList . Just

conjunctionFromStableList :: Ord a => [LeapFrog a] -> LeapFrog a
conjunctionFromStableList frogs = let
    currentValue = current . head $ frogs
    definedNext = fromOperationOnFirst next frogs
    definedSeek value = fromOperationOnFirst (\f -> seek f value) frogs
    definedDown = do dEndOrFrogs <- traverse down frogs
                     case pushPastEnd dEndOrFrogs of
                        Nothing -> Just End
                        Just dFrogs -> fmap Frog (conjunction dFrogs)
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

disjunctionDown :: Ord a => [LeapFrog a] -> Maybe (FrogOrEnd a)
disjunctionDown upperFrogs = let
    downFrogsOrEnd = catMaybes . map down $ upperFrogs
  in case pushPastEnd downFrogsOrEnd of
       Nothing -> Just End
       Just dFrogs -> fmap Frog (disjunction dFrogs)

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
    definedDown = disjunctionDown (takeWhile sameValue orderedList)
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

-- TODO: should also implement and existential that operates on the last variable
existential :: Ord a => Int -> LeapFrog a -> Maybe (FrogOrEnd a)
existential 0 frog = disjunctionDown (valueList frog) where
  valueList f = f : case next f of
                      Nothing -> []
                      Just f' -> valueList f'
existential position frog = let
    definedNext = do nextFrog <- next frog
                     fmap (forceFrog "existenial next")
                       $ existential position nextFrog
    definedSeek value = do seekFrog <- seek frog value
                           fmap (forceFrog "existential seek") $
                             existential position seekFrog
    definedDown = do downFrogOrEnd <- down frog
                     case downFrogOrEnd of
                       End -> error "hit end in existential before reaching quantified variable"
                       Frog downFrog -> existential (position - 1) downFrog
  in Just . Frog $ LeapFrog (current frog) definedNext definedSeek definedDown

isAtLastPosition :: LeapFrog a -> Bool
isAtLastPosition frog = case down frog of
                          Nothing         -> True
                          (Just End)      -> True
                          (Just (Frog _)) -> False

existentialOnLast :: Ord a => Int -> LeapFrog a -> Maybe (FrogOrEnd a)
existentialOnLast 0 frog = assert (isAtLastPosition frog) $ Just End
existentialOnLast indexOfLastPosition frog = let
    definedNext = do nextFrog <- next frog
                     fmap (forceFrog "existenial next")
                       $ existentialOnLast indexOfLastPosition nextFrog
    definedSeek value = do seekFrog <- seek frog value
                           fmap (forceFrog "existential seek") $
                             existentialOnLast indexOfLastPosition seekFrog
    definedDown = do downFrogOrEnd <- down frog
                     case downFrogOrEnd of
                       End -> error "hit end in existential before reaching quantified variable"
                       Frog downFrog -> existentialOnLast
                                          (indexOfLastPosition - 1) downFrog
  in Just . Frog $ LeapFrog (current frog) definedNext definedSeek definedDown

duplicateAtMergeFrom :: Ord a => a -> Int -> FrogOrEnd a -> LeapFrog a
duplicateAtMergeFrom fixedValue 0 continuation = duplicationFrog where
  definedNext = Nothing
  definedSeek value = if value <= fixedValue then Just duplicationFrog
                                             else Nothing
  definedDown = Just continuation
  duplicationFrog = LeapFrog fixedValue definedNext definedSeek definedDown
duplicateAtMergeFrom fixedValue mergeFrom End =
  error "hit end in merge before reaching merge to variable"
duplicateAtMergeFrom fixedValue mergeFrom (Frog f) = worker f where
  worker frog = let 
      definedNext = fmap worker (next frog)
      definedSeek value = fmap worker (seek frog value)
      definedDown = case down frog of
                      Nothing -> Nothing
                      Just continuation -> Just . Frog $
                            duplicateAtMergeFrom fixedValue (mergeFrom - 1)                                                      continuation
    in LeapFrog (current frog) definedNext definedSeek definedDown

findMergeTo :: Ord a => Int -> Int -> LeapFrog a -> LeapFrog a
findMergeTo mergeTo mergeFrom frog = let
    recurseOnSameLevel = findMergeTo mergeTo mergeFrom
    definedNext = fmap recurseOnSameLevel (next frog)
    definedSeek value = fmap recurseOnSameLevel (seek frog value)
    operate continuation = Frog $
            if mergeTo == 0
              then duplicateAtMergeFrom (current frog) (mergeFrom - 1) continuation
              else case continuation of
                     End -> error "hit end in merge before reaching merge to variable"
                     (Frog downFrog) ->
                           findMergeTo (mergeTo - 1) (mergeFrom - 1) downFrog
    definedDown = fmap operate (down frog)
  in LeapFrog (current frog) definedNext definedSeek definedDown

merge :: Ord a => Int -> Int -> LeapFrog a -> LeapFrog a
merge mergeTo mergeFrom frog = assert (mergeTo < mergeFrom) $
  findMergeTo mergeTo mergeFrom frog

split :: Ord a => Int -> Int -> LeapFrog a -> Maybe (LeapFrog a)
split splitFrom splitTo frog = undefined

