module Main where

import Data.Maybe

import Data.Value
import Data.Table
import LeapFrog

table1 = fromTupleList [IntType,IntType]
            [[IntValue 1, IntValue 1],
             [IntValue 1, IntValue 3],
             [IntValue 2, IntValue 1],
             [IntValue 4, IntValue 2],
             [IntValue 4, IntValue 3],
             [IntValue 4, IntValue 5]]

table2 = fromTupleList [IntType,IntType]
            [[IntValue 1, IntValue 1],
             [IntValue 2, IntValue 2],
             [IntValue 3, IntValue 2],
             [IntValue 4, IntValue 1],
             [IntValue 4, IntValue 4],
             [IntValue 4, IntValue 5]]

{-
table1 = fromTupleList [IntType]
            [[IntValue 1],
             [IntValue 3],
             [IntValue 4],
             [IntValue 5],
             [IntValue 6],
             [IntValue 8]]

table2 = fromTupleList [IntType]
            [[IntValue 2],
             [IntValue 3],
             [IntValue 8],
             [IntValue 9]]
-}

frog1 = fromJust . toLeapFrog $ table1
frog2 = fromJust . toLeapFrog $ table2

main :: IO ()
main = do
  --let out = Just (LeapFrog.disjunction [frog1, frog2])
  let out = LeapFrog.conjunction [frog1, frog2]
  --let out = toLeapFrog table2
  let outTuples = LeapFrog.toTupleList 2 out
  putStrLn . unlines . (map prettyTuple) $ outTuples
