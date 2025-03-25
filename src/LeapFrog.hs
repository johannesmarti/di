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
          then stabelizeWorker value (f':onValue) rest
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
    currentValue = current . head $ orderedList
    definedNext = let
        work [] = Nothing
        work (f:fs) = if currentValue == current f
                        then let fs' = case next f of
                                         Nothing -> fs
                                         Just f' -> insertOrdered f' fs
                               in work fs
                        else (f:fs)
      in work orderedList
    definedSeek value = let
        work [] = Nothing
        work (f:fs) = if value < current f
                        then let fs' = case seek value f of
                                         Nothing -> fs
                                         Just f' -> insertOrdered f' fs
                               in work fs
                        else (f:fs)
      in work orderedList
    definedDown = undefined
  in LeapFrog currentValue definedNext definedSeek definedDown

-- TODO: is there a library for this?
insertOrdered :: Ord a => a -> [a] -> [a]
insertOrdered value [] = [value]
insertOrdered value (a:as) = if value <= a
                               then value : (a:as)
                               else a : (insertOrdered value as)
