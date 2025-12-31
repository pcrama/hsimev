{-# LANGUAGE Trustworthy #-}

module PriorityMapTests (tests) where

import Data.List (nub)
import Data.Text (Text)
import PriorityMap
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

tests =
  [ testListContents,
    testInsert,
    testSplitAfterFirst,
    testRemove,
    testLookupKey,
    testFromListContentsSpecialCases
  ]

safeFromListContents :: (Ord p, Eq k) => [(p, k, v)] -> PriorityMap p k v
safeFromListContents psksvs
  | not $ sortedByP psksvs = error "Invalid input for safeFromListContents: should be sorted in ascending p order"
  | not uniqueKs = error "Invalid input for safeFromListContents: key values should be unique"
  | otherwise = fromListContents psksvs
  where
    sortedByP [] = True
    sortedByP [_] = True
    sortedByP ((p1, _, _) : pkv@(p2, _, _) : tl) = p1 <= p2 && sortedByP (pkv : tl)
    uniqueKs = length psksvs == length (nub $ map (\(_, k, _) -> k) psksvs)

testListContents :: TestTree
testListContents =
  testGroup
    "listContents"
    [ testCase "empty" $ listContents (empty :: PriorityMap Int String Char) @?= [],
      testCase "from sorted input with unique keys (if this test passes, the usage of safeFromListContents is valid)" $
        listContents (fromListContents [(1 :: Int, "One", 'a'), (2 :: Int, "Two", 'b')]) @?= [(1, "One", 'a'), (2, "Two", 'b')]
    ]

testInsert :: TestTree
testInsert =
  testGroup
    "insert"
    [ testCase "into empty" $
        listContents (insert empty 1 "One" 'A') @?= [(1 :: Int, "One", 'A')],
      testCase "in front (unique key)" $
        listContents (insert oneElt 0 "Zero" 'z') @?= [(0, "Zero", 'z'), (1, "One", 'A')],
      testCase "in front (duplicated key)" $
        listContents (insert oneElt 0 "One" 'o') @?= [(0, "One", 'o')],
      testCase "at the end (unique key)" $
        listContents (insert threeElts 9 'i' "Nine")
          @?= [(1, 'a', "one"), (2.72, 'b', "two"), (3.14, 'c', "three"), (9, 'i', "Nine")],
      testCase "at the end (larger priority than duplicated key)" $
        listContents (insert threeElts 9 'c' "CCC")
          @?= [(1, 'a', "one"), (2.72, 'b', "two"), (3.14, 'c', "three")],
      testCase "at the end (smaller priority than duplicated key)" $
        listContents (insert threeElts 3 'c' "CCC")
          @?= [(1, 'a', "one"), (2.72, 'b', "two"), (3, 'c', "CCC")],
      testCase "after first (unique key)" $
        listContents (insert threeElts 1.5 '?' "?")
          @?= [(1, 'a', "one"), (1.5, '?', "?"), (2.72, 'b', "two"), (3.14, 'c', "three")],
      testCase "in the middle (larger priority than duplicated key)" $
        listContents (insert threeElts 3 'b' "BBB")
          @?= [(1, 'a', "one"), (2.72, 'b', "two"), (3.14, 'c', "three")],
      testCase "in the middle (smaller priority than duplicated key)" $
        listContents (insert threeElts 2 'b' "BBB")
          @?= [(1, 'a', "one"), (2, 'b', "BBB"), (3.14, 'c', "three")]
    ]
  where
    oneElt :: PriorityMap Int String Char
    oneElt = safeFromListContents [(1, "One", 'A')]
    threeElts :: PriorityMap Double Char String
    threeElts = safeFromListContents [(1, 'a', "one"), (2.72, 'b', "two"), (3.14, 'c', "three")]

testRemove :: TestTree
testRemove =
  testGroup
    "remove"
    [ testCase "from empty" $
        fmap listContents <$> remove empty () @?= (Nothing :: Maybe ((Int, Char), [(Int, (), Char)])),
      testCase "existing key from 1 elt" $
        fmap listContents <$> remove oneElt "One" @?= Just ((1, 'A'), []),
      testCase "unknown key from 1 elt" $
        fmap listContents <$> remove oneElt "ZZZ" @?= (Nothing :: Maybe ((Int, Char), [(Int, String, Char)])),
      testCase "unknown key from 3 elts" $
        fmap listContents <$> remove threeElts 'z' @?= (Nothing :: Maybe ((Double, String), [(Double, Char, String)])),
      testCase "first element of 3" $
        fmap listContents <$> remove threeElts 'a' @?= Just ((1, "one"), [(2.72, 'b', "two"), (3.14, 'c', "three")]),
      testCase "second element of 3" $
        fmap listContents <$> remove threeElts 'b' @?= Just ((2.72, "two"), [(1, 'a', "one"), (3.14, 'c', "three")]),
      testCase "third element of 3" $
        fmap listContents <$> remove threeElts 'c' @?= Just ((3.14, "three"), [(1, 'a', "one"), (2.72, 'b', "two")])
    ]
  where
    oneElt :: PriorityMap Int String Char
    oneElt = safeFromListContents [(1, "One", 'A')]
    threeElts :: PriorityMap Double Char String
    threeElts = safeFromListContents [(1, 'a', "one"), (2.72, 'b', "two"), (3.14, 'c', "three")]

testSplitAfterFirst :: TestTree
testSplitAfterFirst =
  testGroup
    "splitAfterFirst & lookupFirst"
    [ testCase "splitAfterFirst empty" $
        fmap (fmap listContents) (splitAfterFirst empty) @?= (Nothing :: Maybe ((Int, Char, String), [(Int, Char, String)])),
      testCase "lookupFirst empty" $ lookupFirst empty @?= (Nothing :: Maybe (Int, Char, String)),
      testCase "splitAfterFirst (1 elt)" $ do
        let Just (pkv, emptyPM) = splitAfterFirst oneElt
        pkv @?= (1, "One", 'A')
        listContents emptyPM @?= [],
      testCase "lookupFirst (1 elt)" $ lookupFirst oneElt @?= Just (1, "One", 'A'),
      testCase "splitAfterFirst (3 elts)" $ do
        let Just (pkv, emptyPM) = splitAfterFirst threeElts
        pkv @?= (1, 'a', "one")
        listContents emptyPM @?= [(2.72, 'b', "two"), (3.14, 'c', "three")],
      testCase "lookupFirst (3 elts)" $ lookupFirst threeElts @?= Just (1, 'a', "one")
    ]
  where
    oneElt :: PriorityMap Int String Char
    oneElt = safeFromListContents [(1, "One", 'A')]
    threeElts :: PriorityMap Double Char String
    threeElts = safeFromListContents [(1, 'a', "one"), (2.72, 'b', "two"), (3.14, 'c', "three")]

testLookupKey :: TestTree
testLookupKey =
  testGroup
    "lookupKey"
    [ testCase "empty" $ lookupKey empty "a" @?= (Nothing :: Maybe (Int, Char)),
      testCase "unknown key from 1 elt" $ lookupKey oneElt "ZZZ" @?= Nothing,
      testCase "existing key from 1 elt" $ lookupKey oneElt "One" @?= Just (1, 'A'),
      testCase "unknown key from 3 elts" $ lookupKey threeElts "zzz" @?= Nothing,
      testCase "existing key (1st from 3 elts)" $ lookupKey threeElts "9a" @?= Just (1, "one"),
      testCase "existing key (2nd from 3 elts)" $ lookupKey threeElts "8b" @?= Just (2.72, "two"),
      testCase "existing key (3rd from 3 elts)" $ lookupKey threeElts "7c" @?= Just (3.14, "three")
    ]
  where
    oneElt :: PriorityMap Int String Char
    oneElt = safeFromListContents [(1, "One", 'A')]
    threeElts :: PriorityMap Double String String
    threeElts = safeFromListContents [(1, "9a", "one"), (2.72, "8b", "two"), (3.14, "7c", "three")]

testFromListContentsSpecialCases :: TestTree
testFromListContentsSpecialCases =
  testGroup
    "fromListContents"
    [ testCase (show psks <> " -> " <> show expectedResult) $
        listContents (fromListContents $ map (\(p, k) -> (p, k, ())) psks)
          @?= map (\(p, k) -> (p, k, ())) expectedResult
    | (psks, expectedResult) <-
        [ ([(3, '3'), (4, '4'), (5, '3')], [(3, '3'), (4, '4')]),
          ([(3, '3'), (4, '3'), (5, '3')], [(3, '3')]),
          ([(99, '3'), (4, '4'), (5, '3')], [(4, '4'), (5, '3')]),
          ([(99, 'z'), (4, '4'), (5, '5'), (9, '9'), (7, '7')], [(4, '4'), (5, '5'), (7, '7'), (9, '9'), (99, 'z')])
        ]
    ]
