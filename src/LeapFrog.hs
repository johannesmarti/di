module LeapFrog (

) where

import Data.List (sortBy)
import Data.Ord (comparing)

data LeapFrog a = LeapFrog {
  current :: a,
  next :: Maybe (LeapFrog a),
  seek :: a ->  Maybe (LeapFrog a),
  down :: Maybe (LeapFrog a)
}


conjunction :: Ord a => [LeapFrog a] -> Maybe (LeapFrog a)
conjunction frogs = fromOperationOnFirst Just frogs
  
conjunctionFromStableList :: Ord a => [LeapFrog a] -> LeapFrog a
conjunctionFromStableList frogs = let 
    currentValue = current . head $ frogs
    definedNext = fromOperationOnFirst next frogs
    definedSeek value = fromOperationOnFirst (\f -> seek f value) frogs
    definedDown = do dFrogs <- traverse down frogs
                     conjunction dFrogs
  in LeapFrog currentValue definedNext definedSeek definedDown

stabelizeWorker :: Ord a => a -> [LeapFrog a] -> [LeapFrog a]
                              -> Maybe [LeapFrog a]
stabelizeWorker _ onValue [] = Just onValue
stabelizeWorker value onValue (f:fs) =
  case seek f value of
    Nothing -> Nothing
    Just f' -> let newValue = current f' in
        if newValue == value
          then stabelizeWorker value (f':onValue) fs
          else stabelizeWorker newValue [f'] (fs ++ onValue)

stabelize :: Ord a => LeapFrog a -> [LeapFrog a] -> Maybe [LeapFrog a]
stabelize target others = stabelizeWorker (current target) [target] others

fromOperationOnFirst :: Ord a => (LeapFrog a -> Maybe (LeapFrog a))
                                 -> [LeapFrog a] -> Maybe (LeapFrog a)
fromOperationOnFirst _ [] = error "not going to construct conjunction leapfrog for empty conjunction"
fromOperationOnFirst o (first:rest) = do
  first' <- o first
  stableList <- stabelize first' rest
  return $ conjunctionFromStableList stableList


disjunction :: Ord a => [LeapFrog a] -> LeapFrog a
disjunction = disjunctionFromOrderedList . sortBy (comparing current)

disjunctionFromOrderedList :: Ord a => [LeapFrog a] -> LeapFrog a
disjunctionFromOrderedList orderedList = let
    -- here we could have better error reporting for the emptyList
    currentValue = current . head $ orderedList
    sameValue f = current f == currentValue
    -- TODO: take out the common pattern in the following two functions
    definedNext = let
        work [] = Nothing
        work (f:fs) = if sameValue f
                        then let fs' = case next f of
                                         Nothing -> fs
                                         Just f' -> insertOrderedFrog f' fs
                               in work fs'
                        else Just (f:fs)
      in fmap disjunctionFromOrderedList (work orderedList)
    definedSeek value = let
        work [] = Nothing
        work (f:fs) = if current f < value
                        then let fs' = case seek f value of
                                         Nothing -> fs
                                         Just f' -> insertOrderedFrog f' fs
                               in work fs'
                        else Just (f:fs)
      in fmap disjunctionFromOrderedList (work orderedList)
    definedDown = Just $ disjunction (takeWhile sameValue orderedList)
  in LeapFrog currentValue definedNext definedSeek definedDown

-- TODO: is there a library for this?
insertOrderedBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertOrderedBy cmp value [] = [value]
insertOrderedBy cmp value (a:as) = if a `cmp` value == LT
                               then a : (insertOrderedBy cmp value as)
                               else value : (a:as)

insertOrderedFrog :: Ord a => LeapFrog a -> [LeapFrog a] -> [LeapFrog a]
insertOrderedFrog = insertOrderedBy (comparing current)
