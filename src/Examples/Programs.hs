{-# LANGUAGE TypeSynonymInstances #-}

module Examples.Programs (
  transitive, transitiveSchema,
  manages, managesSchema,
  fp, fpSchema,
  top, topSchema,
  chain, chainSchema,
  Examples.Programs.sum, sumSchema,
  borrow, borrowSchema,
  chainOfSpiders, chainOfSpidersSchema,
  cup, cupSchema,
  unify, unifySchema,
  generatedEquivalence, generatedEquivalenceSchema,
) where

import qualified Data.Map.Strict as M

import Atom
import Program

type AbstractTypeSignature t = [t]
type AbstractTypedSchema r t = M.Map r (AbstractTypeSignature t)


transitive :: Program Char Char
transitive = [
    Rule (Atom 'T' ['x', 'y']) [Atom 'R' ['x', 'y']],
    Rule (Atom 'T' ['x', 'y']) [Atom 'R' ['x', 'z'], Atom 'T' ['z', 'y']] ]

transitiveSchema :: AbstractTypedSchema Char String
transitiveSchema = M.fromList [('E', ["int", "int"])]


manages :: Program Char Char
manages = [
    Rule (Atom 'U' ['x']) [Atom 'B' ['b'], Atom 'M' ['b', 'x']],
    Rule (Atom 'U' ['x']) [Atom 'U' ['u'], Atom 'M' ['u', 'x']],
    Rule (Atom 'F' ['t', 'n']) [Atom 'U' ['t'], Atom 'P' ['t', 'n', 'b']] 
    ]

managesSchema :: AbstractTypedSchema Char String
managesSchema = M.fromList [('B', ["id"]), ('M', ["id", "id"]),
                            ('P', ["id", "str", "date"])]

fp :: Program Char Char
fp = [
    Rule (Atom 'R' ['x']) [Atom 'R' ['x']]
    ]

fpSchema :: AbstractTypedSchema Char String
fpSchema = M.empty


top :: Program Char Char
top = [
    Rule (Atom 'T' ['x']) [],
    Rule (Atom 'T' ['x']) [Atom 'B' ['x']]
    ]

topSchema :: AbstractTypedSchema Char String
topSchema = M.fromList [('B', ["id"])]

chain :: Program String Char
chain = [
    Rule (Atom "Diamond" ['x', 'v', 'y']) [Atom "Up" ['x', '1', 'u'], Atom "Down" ['x', '2', 'd'],
                                           Atom "BackDown" ['u', '3', 'y'], Atom "BackUp" ['d', '4', 'y'],
                                           Atom "Add" ['1', '2', '8'],
                                           Atom "Add" ['3', '4', '9'],
                                           Atom "Min" ['8', '9', 'v']  ],
    Rule (Atom "Chain" ['x', 'v', 'y']) [Atom "Diamond" ['x', 'v', 'y']],
    Rule (Atom "Chain" ['x', 'd', 'y']) [Atom "Diamond" ['x', 'v', 'z'], Atom "Chain" ['z', 'w', 'y'], Atom "Add" ['v', 'w', 'd']]
    ]

chainSchema :: AbstractTypedSchema String String
chainSchema = M.fromList [("Up", ["id", "dist", "id"]), ("Down", ["id", "dist", "id"]),
                          ("BackUp", ["id", "dist", "id"]), ("BackDown", ["id", "dist", "id"]),
                          ("Min", ["dist", "dist", "dist"]),
                          ("Add", ["dist", "dist", "dist"]) ]

sum :: Program String Char
sum = [
    Rule (Atom "Diamond" ['x', 'v', 'y']) [Atom "Up" ['x', '1', 'u'], Atom "Down" ['x', '2', 'd'],
                                           Atom "BackDown" ['u', '3', 'y'], Atom "BackUp" ['d', '4', 'y'],
                                           Atom "Add" ['1', '2', '8'],
                                           Atom "Add" ['3', '4', '9'],
                                           Atom "Min" ['8', '9', 'v']  ],
    Rule (Atom "Chain" ['x', 'v', 'y']) [Atom "Diamond" ['x', 'v', 'y']],
    Rule (Atom "Sum" ['x']) [Atom "Diamond" ['x', 'v', 'z'], Atom "Chain" ['z', 'w', 'y'], Atom "Add" ['v', 'w', 'd']]
    ]

sumSchema :: AbstractTypedSchema String String
sumSchema = M.fromList [("R", ["id", "int"]),
                          ("Min", ["int", "int", "int"]),
                          ("Add", ["int", "int", "int"]) ]


borrow :: Program String Char
borrow = [
    Rule (Atom "subset" ['1', '2', 'p']) [Atom "outlives" ['1', '2', 'p']],
    Rule (Atom "subset" ['1', '3', 'p']) [Atom "subset" ['1', '2', 'p'],
                                          Atom "subset" ['2', '3', 'p']],
    Rule (Atom "subset" ['1', '2', 'q']) [Atom "subset" ['1', '2', 'p'], Atom "cfg_edge" ['p', 'q'],
                                          Atom "region_live_at" ['1', 'q'],
                                          Atom "region_live_at" ['2', 'q']],
    Rule (Atom "requires" ['r', 'b', 'p']) [Atom "borrow_region" ['r', 'b', 'p']],
    Rule (Atom "requires" ['2', 'b', 'p']) [Atom "requires" ['1', 'b', 'p'],
                                            Atom "subset" ['1', '2', 'p']],
    Rule (Atom "requires" ['r', 'b', 'q']) [Atom "requires" ['r', 'b', 'p'],
                                            Atom "not_killed" ['b', 'p'],
                                            Atom "cfg_edge" ['p', 'q'],
                                            Atom "region_live_at" ['r', 'q']],
    Rule (Atom "borrow_live_at" ['b', 'p']) [Atom "requires" ['r', 'b', 'p'],
                                             Atom "region_live_at" ['r', 'p']]
    ]

borrowSchema :: AbstractTypedSchema String String
borrowSchema = M.fromList [("outlives", ["set", "set", "id"]),
                           ("cfg_edge", ["id", "id"]),
                           ("region_live_at", ["set", "id"]),
                           ("borrow_region", ["set", "point", "id"]),
                           ("not_killed", ["point", "id"])]

chainOfSpiders :: Program String Char
chainOfSpiders = [
    Rule (Atom "in_chain" ['s', 'l', 'r']) [Atom "first_spider" ['s'],
                                            Atom "left_leg" ['s', 'l'],
                                            Atom "right_leg" ['s', 'r']],
    Rule (Atom "in_chain" ['s', 'l', 'r']) [Atom "chain" ['p', 's'],
                                            Atom "in_chain" ['p', '1', '2'],
                                            Atom "in_chain" ['f', 'l', 'r']]
    ]

chainOfSpidersSchema :: AbstractTypedSchema String String
chainOfSpidersSchema = M.fromList [("chain", ["spider", "spider"]),
                                   ("first_spider", ["spider"]),
                                   ("left_leg", ["spider", "left_leg"]),
                                   ("right_leg", ["spider", "right_leg"])]

cup :: Program String String
cup = [
    Rule (Atom "Match" ["f", "s"]) [Atom "First" ["f"],
                                    Atom "Succ" ["f", "s"]],
    Rule (Atom "Match" ["w", "l"]) [Atom "Match" ["w'", "w"],
                                    Atom "First" ["w'"],
                                    Atom "Succ" ["w", "l"]],
    Rule (Atom "Match" ["w", "l"]) [Atom "Match" ["w'", "l'"],
                                    Atom "Succ" ["w", "w'"],
                                    Atom "Succ" ["l'", "l"]]
    ]

cupSchema :: AbstractTypedSchema String String
cupSchema = M.fromList [("First", ["player"]),
                        ("Succ", ["player", "player"])]


unify :: Program String String
unify = [
    Rule (Atom "Unify" ["p", "ap1", "p", "ap2"])
                [Atom "Head_Atom" ["rule", "atom"],
                 Atom "Linked" ["atom", "p", "ap1", "atom", "p", "ap2"]],
    Rule (Atom "Unify" ["p1", "ap1", "p2", "ap2"])
                [Atom "Head_Atom" ["rule", "atom1"],
                 Atom "Body_Atom" ["rule", "atom2"],
                 Atom "Linked" ["atom1", "p1", "ap1", "atom2", "p2", "ap2"]],
    Rule (Atom "Unify" ["p1", "ap1", "p2", "ap2"])
                [Atom "Body_Atom" ["rule", "atom1"],
                 Atom "Body_Atom" ["rule", "atom2"],
                 Atom "Linked" ["atom1", "p1", "ap1", "atom2", "p2", "ap2"]],
    Rule (Atom "Unify" ["p1", "ap1", "p2", "ap2"])
                [Atom "Unify" ["p2", "ap2", "p1", "ap1"]],
    Rule (Atom "Unify" ["p1", "ap1", "p3", "ap3"])
                [Atom "Unify" ["p1", "ap1", "p2", "ap2"],
                 Atom "Unify" ["p2", "ap2", "p3", "ap3"]],
    Rule (Atom "Linked" ["a1", "p1", "ap1", "a2", "p2", "ap2"])
                [Atom "Insident" ["a1", "ap1", "variable"],
                 Atom "Insident" ["a2", "ap2", "variable"],
                 Atom "Predicate" ["a1", "p1"],
                 Atom "Predicate" ["a2", "p2"]]
    ]

unifySchema :: AbstractTypedSchema String String
unifySchema = M.fromList [("Head_Atom", ["rule", "atom"]),
                          ("Body_Atom", ["rule", "atom"]),
                          ("Predicate", ["atom", "predicate"]),
                          ("Insident", ["atom", "argument_position", "variable"])]

generatedEquivalence :: Program Char Char
generatedEquivalence = [
    Rule (Atom 'E' ['x', 'x']) [],
    Rule (Atom 'E' ['x', 'y']) [Atom 'R' ['x', 'y']],
    Rule (Atom 'E' ['x', 'y']) [Atom 'E' ['x', 'z'], Atom 'E' ['z', 'y']] ]

generatedEquivalenceSchema :: AbstractTypedSchema Char String
generatedEquivalenceSchema = M.fromList [('R', ["x", "x"])]
