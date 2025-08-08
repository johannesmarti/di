module LeapFrogTest (spec) where

import Test.Hspec

import Data.Maybe

import Data.Value
import Data.Table as Table
import LeapFrog


type0 = []
type1 = [IntType]
type2 = [IntType, IntType]
type3 = [IntType, IntType, IntType]

tableEmpty0 = Table.empty type0
tableEmpty1 = Table.empty type1
tableEmpty2 = Table.empty type2

tableFull0 = fromTupleList type0 [[]]

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

table1ConMerge = fromTupleList type2
            [[IntValue 3, IntValue 3],
             [IntValue 8, IntValue 8]]

table2aMerge01 = fromTupleList type3
            [[IntValue 1, IntValue 1, IntValue 1],
             [IntValue 1, IntValue 1, IntValue 3],
             [IntValue 2, IntValue 2, IntValue 1],
             [IntValue 4, IntValue 4, IntValue 2],
             [IntValue 4, IntValue 4, IntValue 3],
             [IntValue 4, IntValue 4, IntValue 5]]

table2aMerge02 = fromTupleList type3
            [[IntValue 1, IntValue 1, IntValue 1],
             [IntValue 1, IntValue 3, IntValue 1],
             [IntValue 2, IntValue 1, IntValue 2],
             [IntValue 4, IntValue 2, IntValue 4],
             [IntValue 4, IntValue 3, IntValue 4],
             [IntValue 4, IntValue 5, IntValue 4]]

table2aMerge12 = fromTupleList type3
            [[IntValue 1, IntValue 1, IntValue 1],
             [IntValue 1, IntValue 3, IntValue 3],
             [IntValue 2, IntValue 1, IntValue 1],
             [IntValue 4, IntValue 2, IntValue 2],
             [IntValue 4, IntValue 3, IntValue 3],
             [IntValue 4, IntValue 5, IntValue 5]]

table2DisSplit = fromTupleList type1
            [[IntValue 1],
             [IntValue 2],
             [IntValue 4]]

tableToFrog = forceFrog "testdata frog" . fromJust . toLeapFrog

frog1a = tableToFrog table1a
frog1b = tableToFrog table1b

frog2a = tableToFrog table2a
frog2b = tableToFrog table2b
frog2c = tableToFrog table2c

frog2Difficult = tableToFrog table2Difficult

spec :: Spec
spec = do
  describe "idempotence" $ do
    it "conjunction frog1a" $ do
      let out = Table.fromTupleList type1 $ LeapFrog.toTupleList $
                  LeapFrog.conjunction [frog1a, frog1a]
      out `shouldBe` table1a

    it "conjunction frog2b" $ do
      let out = Table.fromTupleList type2 $ LeapFrog.toTupleList $
                  LeapFrog.conjunction [frog2b, frog2b]
      out `shouldBe` table2b

    it "disjuction frog1b" $ do
      let out = Table.fromTupleList type1 $ LeapFrog.toTupleList $
                  LeapFrog.disjunction [frog1b, frog1b]
      out `shouldBe` table1b

    it "disjuction frog2a" $ do
      let out = Table.fromTupleList type2 $ LeapFrog.toTupleList $
                  LeapFrog.disjunction [frog2a, frog2a]
      out `shouldBe` table2a

  describe "prepared conjunctions" $ do
    it "table1Con" $ do
      let out = Table.fromTupleList type1 $ LeapFrog.toTupleList $
                  LeapFrog.conjunction [frog1a, frog1b]
      out `shouldBe` table1Con

    it "table2Con" $ do
      let out = Table.fromTupleList type2 $ LeapFrog.toTupleList $
                  LeapFrog.conjunction [frog2a, frog2b]
      out `shouldBe` table2Con

  describe "prepared disjunctions" $ do
    it "table1Dis" $ do
      let out = Table.fromTupleList type1 $ LeapFrog.toTupleList $
                  LeapFrog.disjunction [frog1a, frog1b]
      out `shouldBe` table1Dis

    it "table2Dis" $ do
      let out = Table.fromTupleList type2 $ LeapFrog.toTupleList $
                  LeapFrog.disjunction [frog2a, frog2b]
      out `shouldBe` table2Dis

    it "disjunction with empty continuations" $ do
      let out = Table.fromTupleList type2 $ LeapFrog.toTupleList $
                  LeapFrog.disjunction [frog2c,
                      fromJust (LeapFrog.conjunction [frog2a, frog2b])]
      out `shouldBe` table2Con

  describe "existential" $ do
    it "table2a existential on 1" $ do
      let out = Table.fromTupleList type1 $ maybeFrogOrEndToTupleList $
                  LeapFrog.existential 1 frog2a
      out `shouldBe` table2aExistential1

    it "table2a existential on 0" $ do
      let out = Table.fromTupleList type1 $ maybeFrogOrEndToTupleList $
                  LeapFrog.existential 0 frog2a
      out `shouldBe` table2aExistential0

    it "existential on conjunction of 2a and 2b" $ do
      let out = Table.fromTupleList type1 $ maybeFrogOrEndToTupleList $
                  LeapFrog.existential 1 (fromJust (LeapFrog.conjunction
                                                            [frog2a, frog2b]))
      out `shouldBe` tableConExistential1

  describe "existentialOnLast" $ do
    it "table2a existential on 1" $ do
      let out = Table.fromTupleList type1 $ maybeFrogOrEndToTupleList $
                  LeapFrog.existentialOnLast 1 frog2a
      out `shouldBe` table2aExistential1

    it "existential on conjunction of 2a and 2b" $ do
      let out = Table.fromTupleList type1 $ maybeFrogOrEndToTupleList $
                  LeapFrog.existentialOnLast 1 (fromJust (LeapFrog.conjunction
                                                            [frog2a, frog2b]))
      out `shouldBe` tableConExistential1

  describe "merge" $ do
    it "over conjunction a and b" $ do
      let con = fromJust (LeapFrog.conjunction [frog1a, frog1b])
      let out = Table.fromTupleList type2 $ frogToTupleList $
                  LeapFrog.merge 0 1 con
      out `shouldBe` table1ConMerge

    it "over table2a 0 1" $ do
      let out = Table.fromTupleList type3 $ frogToTupleList $
                  LeapFrog.merge 0 1 frog2a
      out `shouldBe` table2aMerge01

    it "over table2a 0 2" $ do
      let out = Table.fromTupleList type3 $ frogToTupleList $
                  LeapFrog.merge 0 2 frog2a
      out `shouldBe` table2aMerge02

    it "over table2a 1 2" $ do
      let out = Table.fromTupleList type3 $ frogToTupleList $
                  LeapFrog.merge 1 2 frog2a
      out `shouldBe` table2aMerge12

  describe "split" $ do
    it "for table2Difficult" $ do
      let out = Table.fromTupleList type1 $ frogToTupleList $
                  LeapFrog.split 0 1 frog2Difficult
      out `shouldBe` (Table.fromTupleList type1 [])

    let splitOverDis = LeapFrog.split 0 1 . fromJust $
                         LeapFrog.disjunction [frog2a, frog2b]
    it "over disjunction frog2a frog2b" $ do
      let out = Table.fromTupleList type1 (frogToTupleList splitOverDis)
      out `shouldBe` table2DisSplit

    it "disjunction frog2a frog2b emedded in conjunction with table1b" $ do
      let out = Table.fromTupleList type1 $ LeapFrog.toTupleList $
                 (LeapFrog.conjunction [frog1b, splitOverDis])
      out `shouldBe` (Table.fromTupleList type1 [[IntValue 2]])
