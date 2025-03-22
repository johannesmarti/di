module LeapFrog (

) where

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
stabelizeWorker value onValue (next:rest) =
  case seek next value of
    Nothing -> Nothing
    Just n' -> let newValue = current n' in
        if newValue == value
          then stabelizeWorker value (n':onValue) rest
          else stabelizeWorker newValue [n'] (rest ++ onValue)

stabelize :: Ord a => LeapFrog a -> [LeapFrog a] -> Maybe [LeapFrog a]
stabelize target others = stabelizeWorker (current target) [target] others

fromOperationOnFirst :: Ord a => (LeapFrog a -> Maybe (LeapFrog a))
                                 -> [LeapFrog a] -> Maybe (LeapFrog a)
fromOperationOnFirst _ [] = error "not going to construct conjunction leapfrog for empty conjunction"
fromOperationOnFirst o (first:rest) = do
  first' <- o first
  stableList <- stabelize first' rest
  return $ conjunctionFromStableList stableList
