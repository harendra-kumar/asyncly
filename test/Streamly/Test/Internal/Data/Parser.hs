module Main (main) where

import Streamly.Internal.Data.Parser as P
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL

import Test.Hspec(hspec, describe)
import Test.Hspec.QuickCheck
import Test.QuickCheck (forAll, chooseInt, Property, property, listOf, vectorOf, (.&&.))

-- Accumulator Tests

testFromFold :: Property
testFromFold =
    forAll (listOf $ chooseInt (0, 10000)) $ \ls ->
        case (==) <$> (S.parse (fromFold FL.sum) (S.fromList ls)) <*> (S.fold FL.sum (S.fromList ls)) of
            Right is_equal -> is_equal
            Left _ -> False

testAny :: Property
testAny =
    forAll (listOf $ chooseInt (0, 10000)) $ \ls ->
        case S.parse (P.any (> 5000)) (S.fromList ls) of
            Right r -> r == (Prelude.any (> 5000) ls)
            Left _ -> False

testAll :: Property
testAll =
    forAll (listOf $ chooseInt (0, 10000)) $ \ls ->
        case S.parse (P.all (> 5000)) (S.fromList ls) of
            Right r -> r == (Prelude.all (> 5000) ls)
            Left _ -> False

testYield :: Property
testYield = 
    forAll (chooseInt (0, 10000)) $ \x ->
        case S.parse (yield x) (S.fromList [1 :: Int]) of
            Right r -> r == x
            Left _ -> False

testYieldM :: Property
testYieldM =
    forAll (chooseInt (0, 10000)) $ \x ->
        case S.parse (yieldM $ return x) (S.fromList [1 :: Int]) of
            Right r -> r == x
            Left _ -> False

testDie :: Property
testDie =
    property $
    case S.parse (die "die test") (S.fromList [0 :: Int]) of
        Right _ -> False
        Left _ -> True

testDieM :: Property
testDieM =
    property $
    case S.parse (dieM (Right "die test")) (S.fromList [0 :: Int]) of
        Right _ -> False
        Left _ -> True

-- Element Parser Tests

testPeek :: Property
testPeek = 
    forAll (chooseInt (1, 100)) $ \list_length ->
        forAll (vectorOf list_length (chooseInt (0, 10000))) $ \ls ->
            case S.parse peek (S.fromList ls) of
                Right head_value -> case ls of
                    head_ls : _ -> head_value == head_ls
                    _ -> False
                Left _ -> False
    .&&.
    property (case S.parse peek (S.fromList []) of
        Right _ -> False
        Left _ -> True)

testEof :: Property
testEof = 
    forAll (chooseInt (1, 100)) $ \list_length ->
        forAll (vectorOf list_length (chooseInt (0, 10000))) $ \ls ->
            case S.parse eof (S.fromList ls) of
                Right _ -> False
                Left _ -> True
    .&&.
    property (case S.parse eof (S.fromList []) of
        Right _ -> True
        Left _ -> False)

testSatisfy :: Property
testSatisfy = 
    forAll (listOf (chooseInt (0, 10000))) $ \ls ->
        case S.parse (satisfy predicate) (S.fromList ls) of
            Right r -> case ls of
                [] -> False
                (x : _) -> predicate x && (r == x)
            Left _ -> case ls of
                [] -> True
                (x : _) -> not $ predicate x
        where
            predicate = (>= 5000)

-- Sequence Parsers Tests

testTake :: Property
testTake = 
    forAll (chooseInt (0, 10000)) $ \n ->
        forAll (listOf (chooseInt (0, 10000))) $ \ls ->
            case S.parse (P.take n FL.toList) (S.fromList ls) of
                Right parsed_list -> parsed_list == Prelude.take n ls
                Left _ -> False

testTakeEQ :: Property
testTakeEQ =
    forAll (chooseInt (0, 10000)) $ \n ->
        forAll (listOf (chooseInt (0, 10000))) $ \ls ->
            let 
                list_length = Prelude.length ls
            in
                case S.parse (P.takeEQ n FL.toList) (S.fromList ls) of
                    Right parsed_list -> (n <= list_length) && (parsed_list == Prelude.take n ls)
                    Left _ -> n > list_length

testTakeGE :: Property
testTakeGE =
    forAll (chooseInt (0, 10000)) $ \n ->
        forAll (listOf (chooseInt (0, 10000))) $ \ls ->
            let 
                list_length = Prelude.length ls
            in
                case S.parse (P.takeGE n FL.toList) (S.fromList ls) of
                    Right parsed_list -> (n <= list_length) && (parsed_list == ls)
                    Left _ -> n > list_length

testLookAhead :: Property
testLookAhead =
    forAll (chooseInt (0, 10000)) $ \n -> 
        let
            takeWithoutConsume = lookAhead $ P.take n FL.toList
            parseTwice = do
                parsed_list_1 <- takeWithoutConsume
                parsed_list_2 <- takeWithoutConsume
                return (parsed_list_1, parsed_list_2)
        in
            forAll (listOf (chooseInt (0, 10000))) $ \ls ->
                case S.parse parseTwice (S.fromList ls) of
                    Right (ls_1, ls_2) -> (ls_1 == ls_2) && (ls_1 == Prelude.take n ls)
                    Left _ -> (list_length < n) || (list_length == n && n == 0)
                        where
                            list_length = Prelude.length ls

testTakeWhile :: Property
testTakeWhile =
    forAll (listOf (chooseInt (0, 1))) $ \ ls ->
        case S.parse (P.takeWhile predicate  FL.toList) (S.fromList ls) of
            Right parsed_list -> parsed_list == Prelude.takeWhile predicate ls
            Left _ -> False
        where
            predicate = (== 0)

testTakeWhile1 :: Property
testTakeWhile1 =
    forAll (listOf (chooseInt (0, 1))) $ \ ls ->
        case S.parse (P.takeWhile1 predicate  FL.toList) (S.fromList ls) of
            Right parsed_list -> case ls of
                [] -> False
                (x : _) -> predicate x && (parsed_list == Prelude.takeWhile predicate ls)
            Left _ -> case ls of
                [] -> True
                (x : _) -> not $ predicate x
        where
            predicate = (== 0)
    
testSliceSepBy :: Property
testSliceSepBy =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case S.parse (sliceSepBy predicate FL.toList) (S.fromList ls) of
            Right parsed_list -> parsed_list == Prelude.takeWhile (not . predicate) ls
            Left _ -> False
        where
            predicate = (== 1)

main :: IO ()
main = hspec $ do
    describe "test for accumulator" $ do
        prop "test fromFold function" testFromFold
        prop "test any function" testAny
        prop "test all function" testAll
        prop "test yield function" testYield
        prop "test yieldM function" testYieldM
        prop "test die function" testDie
        prop "test dieM function" testDieM
    
    describe "test for element parser" $ do
        prop "test for peek function" testPeek
        prop "test for eof function" testEof
        prop "test for satisfy function" testSatisfy

    describe "test for sequence parser" $ do
        prop "test for take function" testTake
        prop "test for takeEq function" testTakeEQ
        prop "test for takeGE function" testTakeGE
        prop "test for LookAhead function" testLookAhead
        prop "test for takeWhile function" testTakeWhile
        prop "test for takeWhile1 function" testTakeWhile1
        prop "test for sliceSepBy function" testSliceSepBy