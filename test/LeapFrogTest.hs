module LeapFrogTest (spec) where

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

table1Con = fromTupleList type1
            [[IntValue 3],
             [IntValue 8]]

table1Dis = fromTupleList type1
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

table2Con = fromTupleList type2
            [[IntValue 1, IntValue 1],
             [IntValue 4, IntValue 5]]

table2Dis = fromTupleList type2
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

table2c = fromTupleList type2
            [[IntValue 1, IntValue 1]]

table2Difficult = fromTupleList type2
            [[IntValue 2, IntValue 1]]

table2aExistential1 = fromTupleList type1
            [[IntValue 1],
             [IntValue 2],
             [IntValue 4]]

table2aExistential0 = fromTupleList type1
            [[IntValue 1],
             [IntValue 2],
             [IntValue 3],
             [IntValue 5]]

tableConExistential1 = fromTupleList type1
            [[IntValue 1],
             [IntValue 4]]


frog1a = fromJust . toLeapFrog $ table1a
frog1b = fromJust . toLeapFrog $ table1b

frog2a = fromJust . toLeapFrog $ table2a
frog2b = fromJust . toLeapFrog $ table2b
frog2c = fromJust . toLeapFrog $ table2c

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
                LeapFrog.disjunction [frog1b, frog1b]
    it "disjuction frog1b" $
      out `shouldBe` table1b

    let out = Table.fromTupleList type2 $ LeapFrog.toTupleList 2 $
                LeapFrog.disjunction [frog2a, frog2a]
    it "disjuction frog2a" $
      out `shouldBe` table2a

  describe "prepared conjunctions" $ do
    let out = Table.fromTupleList type1 $ LeapFrog.toTupleList 1 $
                LeapFrog.conjunction [frog1a, frog1b]
    it "table1Con" $
      out `shouldBe` table1Con

    let out = Table.fromTupleList type2 $ LeapFrog.toTupleList 2 $
                LeapFrog.conjunction [frog2a, frog2b]
    it "table2Con" $
      out `shouldBe` table2Con

  describe "prepared disjunctions" $ do
    let out = Table.fromTupleList type1 $ LeapFrog.toTupleList 1 $
                LeapFrog.disjunction [frog1a, frog1b]
    it "table1Dis" $
      out `shouldBe` table1Dis

    let out = Table.fromTupleList type2 $ LeapFrog.toTupleList 2 $
                LeapFrog.disjunction [frog2a, frog2b]
    it "table2Dis" $
      out `shouldBe` table2Dis

    let out = Table.fromTupleList type2 $ LeapFrog.toTupleList 2 $
                LeapFrog.disjunction [frog2c,
                    fromJust (LeapFrog.conjunction [frog2a, frog2b])]
    it "disjunction with empty continuations" $
      out `shouldBe` table2Con

  describe "existential" $ do
    let out = Table.fromTupleList type1 $ LeapFrog.toTupleList 1 $
                LeapFrog.existential 1 frog2a
    it "table2a existential on 1" $
      out `shouldBe` table2aExistential1

    let out = Table.fromTupleList type1 $ LeapFrog.toTupleList 1 $
                LeapFrog.existential 0 frog2a
    it "table2a existential on 0" $
      out `shouldBe` table2aExistential0

    let out = Table.fromTupleList type1 $ LeapFrog.toTupleList 1 $
                LeapFrog.existential 1 (fromJust (LeapFrog.conjunction
                                                          [frog2a, frog2b]))
    it "existential on conjunction of 2a and 2b" $
      out `shouldBe` tableConExistential1

