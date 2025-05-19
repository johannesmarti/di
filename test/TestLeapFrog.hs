module TestLeapFrog (spec) where

import Test.Hspec

import Data.Maybe

import Data.Value
import Data.Table as Table
import LeapFrog

type1 = [IntType]
type2 = [IntType, IntType]

tableEmpty1 = Table.empty type1
tableEmpty2 = Table.empty type2

table1a = fromTupleList type1
            [[IntValue 1],
             [IntValue 3],
             [IntValue 4],
             [IntValue 5],
             [IntValue 6],
             [IntValue 8]]

table1b = fromTupleList type1
            [[IntValue 2],
             [IntValue 3],
             [IntValue 8],
             [IntValue 9]]

tableCon1 = fromTupleList type1
            [[IntValue 3],
             [IntValue 8]]

tableDis1 = fromTupleList type1
            [[IntValue 1],
             [IntValue 2],
             [IntValue 3],
             [IntValue 4],
             [IntValue 5],
             [IntValue 6],
             [IntValue 9],
             [IntValue 8]]

table2a = fromTupleList type2
            [[IntValue 1, IntValue 1],
             [IntValue 1, IntValue 3],
             [IntValue 2, IntValue 1],
             [IntValue 4, IntValue 2],
             [IntValue 4, IntValue 3],
             [IntValue 4, IntValue 5]]

table2b = fromTupleList type2
            [[IntValue 1, IntValue 1],
             [IntValue 2, IntValue 2],
             [IntValue 3, IntValue 2],
             [IntValue 4, IntValue 1],
             [IntValue 4, IntValue 4],
             [IntValue 4, IntValue 5]]

tableCon2 = fromTupleList type2
            [[IntValue 1, IntValue 1],
             [IntValue 4, IntValue 5]]

tableDis2 = fromTupleList type2
            [[IntValue 1, IntValue 1],
             [IntValue 1, IntValue 3],
             [IntValue 2, IntValue 1],
             [IntValue 2, IntValue 2],
             [IntValue 3, IntValue 2],
             [IntValue 4, IntValue 1],
             [IntValue 4, IntValue 2],
             [IntValue 4, IntValue 3],
             [IntValue 4, IntValue 4],
             [IntValue 4, IntValue 5]]


frog1a = fromJust . toLeapFrog $ table1a
frog1b = fromJust . toLeapFrog $ table1b

frog2a = fromJust . toLeapFrog $ table2a
frog2b = fromJust . toLeapFrog $ table2b

spec :: Spec
spec = do
  describe "idempotence" $ do
    let out = Table.fromTupleList type1 $ LeapFrog.toTupleList 1 $
                LeapFrog.conjunction [frog1a, frog1a]
    it "conjunction frog1a" $
      out `shouldBe` table1a

    let out = Table.fromTupleList type2 $ LeapFrog.toTupleList 2 $
                LeapFrog.conjunction [frog2b, frog2b]
    it "conjunction frog2b" $
      out `shouldBe` table2b

    let out = Table.fromTupleList type1 $ LeapFrog.toTupleList 1 $
                Just (LeapFrog.disjunction [frog1b, frog1b])
    it "disjuction frog1b" $
      out `shouldBe` table1b

    let out = Table.fromTupleList type2 $ LeapFrog.toTupleList 2 $
                Just (LeapFrog.disjunction [frog2a, frog2a])
    it "disjuction frog2a" $
      out `shouldBe` table2a

  describe "prepared conjunctions" $ do
    let out = Table.fromTupleList type1 $ LeapFrog.toTupleList 1 $
                LeapFrog.conjunction [frog1a, frog1b]
    it "tableCon1" $
      out `shouldBe` tableCon1

    let out = Table.fromTupleList type2 $ LeapFrog.toTupleList 2 $
                LeapFrog.conjunction [frog2a, frog2b]
    it "tableCon2" $
      out `shouldBe` tableCon2

  describe "prepared disjunctions" $ do
    let out = Table.fromTupleList type1 $ LeapFrog.toTupleList 1 $
                Just (LeapFrog.disjunction [frog1a, frog1b])
    it "tableDis1" $
      out `shouldBe` tableDis1

    let out = Table.fromTupleList type2 $ LeapFrog.toTupleList 2 $
                Just (LeapFrog.disjunction [frog2a, frog2b])
    it "tableDis2" $
      out `shouldBe` tableDis2

