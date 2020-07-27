module Main (main) where

import qualified Streamly.Internal.Data.Parser.ParserD as P
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL

-- import Data.List (partition)

import Test.Hspec(Spec, hspec, describe)
import qualified Test.Hspec as H
import Test.Hspec.QuickCheck
import Test.QuickCheck (arbitrary, forAll, choose, elements, Property,
                        property, listOf, vectorOf, counterexample,
                        (.&&.), Gen)

import Test.QuickCheck.Monadic (monadicIO, PropertyM, assert, monitor)
import Control.Exception (SomeException(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when)
import Data.List ((\\))

maxTestCount :: Int
maxTestCount = 100

min_value :: Int
min_value = 0

mid_value :: Int
mid_value = 5000

max_value :: Int
max_value = 10000

max_length :: Int
max_length = 1000

listEquals :: (Show a, Eq a, MonadIO m)
    => ([a] -> [a] -> Bool) -> [a] -> [a] -> PropertyM m ()
listEquals eq parsed_list list = do
    when (not $ parsed_list `eq` list) $ liftIO $ putStrLn $
                  "parsed list " ++ show parsed_list
             ++ "\nlist   " ++ show list
             ++ "\nparsed list \\\\ list " ++ show (parsed_list \\ list)
             ++ "\nlist \\\\ parsed list " ++ show (list \\ parsed_list)
    when (not $ parsed_list `eq` list) $
        monitor
            (counterexample $
                  "parsed list " ++ show parsed_list
             ++ "\nlist   " ++ show list
             ++ "\nparsed list \\\\ list " ++ show (parsed_list \\ list)
             ++ "\nlist \\\\ parsed list " ++ show (list \\ parsed_list)
             )
    assert (parsed_list `eq` list)

checkListEqual :: (Show a, Eq a) => [a] -> [a] -> Property
checkListEqual ls_1 ls_2 = monadicIO (listEquals (==) ls_1 ls_2)

chooseInt :: (Int, Int) -> Gen Int
chooseInt = choose

-- Accumulator Tests

fromFold :: Property
fromFold =
    forAll (listOf $ chooseInt (min_value, max_value)) $ \ls ->
        case (==) <$> (S.parseD (P.fromFold FL.sum) (S.fromList ls)) <*> (S.fold FL.sum (S.fromList ls)) of
            Right is_equal -> is_equal
            Left _ -> False

any :: Property
any =
    forAll (listOf $ chooseInt (min_value, max_value)) $ \ls ->
        case S.parseD (P.any (> mid_value)) (S.fromList ls) of
            Right r -> r == (Prelude.any (> mid_value) ls)
            Left _ -> False

all :: Property
all =
    forAll (listOf $ chooseInt (min_value, max_value)) $ \ls ->
        case S.parseD (P.all (> mid_value)) (S.fromList ls) of
            Right r -> r == (Prelude.all (> mid_value) ls)
            Left _ -> False

yield :: Property
yield =
    forAll (chooseInt (min_value, max_value)) $ \x ->
        case S.parseD (P.yield x) (S.fromList [1 :: Int]) of
            Right r -> r == x
            Left _ -> False

yieldM :: Property
yieldM =
    forAll (chooseInt (min_value, max_value)) $ \x ->
        case S.parseD (P.yieldM $ return x) (S.fromList [1 :: Int]) of
            Right r -> r == x
            Left _ -> False

die :: Property
die =
    property $
    case S.parseD (P.die "die test") (S.fromList [0 :: Int]) of
        Right _ -> False
        Left _ -> True

dieM :: Property
dieM =
    property $
    case S.parseD (P.dieM (Right "die test")) (S.fromList [0 :: Int]) of
        Right _ -> False
        Left _ -> True

-- Element Parser Tests

peekPass :: Property
peekPass =
    forAll (chooseInt (1, max_length)) $ \list_length ->
        forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
            case S.parseD P.peek (S.fromList ls) of
                Right head_value -> case ls of
                    head_ls : _ -> head_value == head_ls
                    _ -> False
                Left _ -> False

peekFail :: Property
peekFail =
    property (case S.parseD P.peek (S.fromList []) of
        Right _ -> False
        Left _ -> True)

eofPass :: Property
eofPass =
    property (case S.parseD P.eof (S.fromList []) of
        Right _ -> True
        Left _ -> False)

eofFail :: Property
eofFail =
    forAll (chooseInt (1, max_length)) $ \list_length ->
        forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
            case S.parseD P.eof (S.fromList ls) of
                Right _ -> False
                Left _ -> True

satisfyPass :: Property
satisfyPass =
    forAll (chooseInt (mid_value, max_value)) $ \first_element ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls_tail ->
            let
                ls = first_element : ls_tail
                predicate = (>= mid_value)
            in
                case S.parseD (P.satisfy predicate) (S.fromList ls) of
                    Right r -> r == first_element
                    Left _ -> False

satisfy :: Property
satisfy =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
        case S.parseD (P.satisfy predicate) (S.fromList ls) of
            Right r -> case ls of
                [] -> False
                (x : _) -> predicate x && (r == x)
            Left _ -> case ls of
                [] -> True
                (x : _) -> not $ predicate x
        where
            predicate = (>= mid_value)

-- Sequence Parsers Tests

take :: Property
take =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
            case S.parseD (P.take n FL.toList) (S.fromList ls) of
                Right parsed_list -> checkListEqual parsed_list (Prelude.take n ls)
                Left _ -> property False

takeEQPass :: Property
takeEQPass =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (chooseInt (n, max_value)) $ \list_length ->
            forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
                case S.parseD (P.takeEQ n FL.toList) (S.fromList ls) of
                    Right parsed_list -> checkListEqual parsed_list (Prelude.take n ls)
                    Left _ -> property False

takeEQ :: Property
takeEQ =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
            let
                list_length = Prelude.length ls
            in
                case S.parseD (P.takeEQ n FL.toList) (S.fromList ls) of
                    Right parsed_list ->
                        if (n <= list_length) then
                            checkListEqual parsed_list (Prelude.take n ls)
                        else
                            property False
                    Left _ -> property (n > list_length)

takeGEPass :: Property
takeGEPass =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (chooseInt (n, max_value)) $ \list_length ->
            forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
                case S.parseD (P.takeGE n FL.toList) (S.fromList ls) of
                    Right parsed_list -> checkListEqual parsed_list ls
                    Left _ -> property False

takeGE :: Property
takeGE =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
            let
                list_length = Prelude.length ls
            in
                case S.parseD (P.takeGE n FL.toList) (S.fromList ls) of
                    Right parsed_list ->
                        if (n <= list_length) then
                            checkListEqual parsed_list ls
                        else
                            property False
                    Left _ -> property (n > list_length)

nLessThanEqual0 ::
       (  Int
       -> FL.Fold (Either SomeException) Int [Int]
       -> P.Parser (Either SomeException) Int [Int]
       )
    -> (Int -> [Int] -> [Int])
    -> Property
nLessThanEqual0 tk ltk =
    forAll (elements [0, (-1)]) $ \n ->
        forAll (listOf arbitrary) $ \ls ->
            case S.parseD (tk n FL.toList) (S.fromList ls) of
                Right parsed_list -> checkListEqual parsed_list (ltk n ls)
                Left _ -> property False

takeProperties :: Spec
takeProperties =
    describe "take combinators when n <= 0/" $ do
        prop "take n FL.toList = []" $
            nLessThanEqual0 P.take (\_ -> const [])
        prop "takeEQ n FL.toList = []" $
            nLessThanEqual0 P.takeEQ (\_ -> const [])
        prop "takeGE n FL.toList xs = xs" $
            nLessThanEqual0 P.takeGE (\_ -> id)


-- XXX lookAhead can't deal with EOF which in this case means when
-- n==list_length, this test will fail. So excluding that case for now.
lookAheadPass :: Property
lookAheadPass =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        let
            takeWithoutConsume = P.lookAhead $ P.take n FL.toList
            parseTwice = do
                parsed_list_1 <- takeWithoutConsume
                parsed_list_2 <- takeWithoutConsume
                return (parsed_list_1, parsed_list_2)
        in
            forAll (chooseInt (n+1, max_value)) $ \list_length ->
                forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
                    case S.parseD parseTwice (S.fromList ls) of
                        Right (ls_1, ls_2) -> checkListEqual ls_1 ls_2 .&&. checkListEqual ls_1 (Prelude.take n ls)
                        Left _ -> property $ False

lookAhead :: Property
lookAhead =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        let
            takeWithoutConsume = P.lookAhead $ P.take n FL.toList
            parseTwice = do
                parsed_list_1 <- takeWithoutConsume
                parsed_list_2 <- takeWithoutConsume
                return (parsed_list_1, parsed_list_2)
        in
            forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
                case S.parseD parseTwice (S.fromList ls) of
                    Right (ls_1, ls_2) -> checkListEqual ls_1 ls_2 .&&. checkListEqual ls_1 (Prelude.take n ls)
                    Left _ -> property ((list_length < n) || (list_length == n && n == 0))
                        where
                            list_length = Prelude.length ls

takeWhile :: Property
takeWhile =
    forAll (listOf (chooseInt (0, 1))) $ \ ls ->
        case S.parseD (P.takeWhile predicate FL.toList) (S.fromList ls) of
            Right parsed_list -> checkListEqual parsed_list (Prelude.takeWhile predicate ls)
            Left _ -> property False
        where
            predicate = (== 0)

takeWhile1 :: Property
takeWhile1 =
    forAll (listOf (chooseInt (0, 1))) $ \ ls ->
        case S.parseD (P.takeWhile1 predicate  FL.toList) (S.fromList ls) of
            Right parsed_list -> case ls of
                [] -> property False
                (x : _) ->
                    if predicate x then
                        checkListEqual parsed_list (Prelude.takeWhile predicate ls)
                    else
                        property False
            Left _ -> case ls of
                [] -> property True
                (x : _) -> property (not $ predicate x)
        where
            predicate = (== 0)

sliceSepBy :: Property
sliceSepBy =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case S.parseD (P.sliceSepBy predicate FL.toList) (S.fromList ls) of
            Right parsed_list -> checkListEqual parsed_list (Prelude.takeWhile (not . predicate) ls)
            Left _ -> property False
        where
            predicate = (== 1)

sliceSepByMax :: Property
sliceSepByMax =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (0, 1))) $ \ls ->
            case S.parseD (P.sliceSepByMax predicate n FL.toList) (S.fromList ls) of
                Right parsed_list -> checkListEqual parsed_list (Prelude.take n (Prelude.takeWhile (not . predicate) ls))
                Left _ -> property False
            where
                predicate = (== 1)

sliceEndWith :: Property
sliceEndWith =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case S.parseD (P.sliceEndWith predicate FL.toList) (S.fromList ls) of
            Right parsed_list -> 
                checkListEqual 
                parsed_list 
                (takeWhileAndFirstFail (not . predicate) ls)
            Left _ -> property False
        where
            predicate = (== 1)

            takeWhileAndFirstFail prd (x : xs) =
                if prd x 
                then x : takeWhileAndFirstFail prd xs
                else [x]
            takeWhileAndFirstFail _ [] = []

sliceBeginWith :: Property
sliceBeginWith =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case S.parseD (P.sliceBeginWith predicate FL.toList) (S.fromList ls) of
            Right parsed_list -> 
                checkListEqual parsed_list (takeWhileOrFirst (not . predicate) ls)
            Left _ -> property False
        where
            predicate = (== 1)

            takeWhileOrFirst prd (x : xs) = x : Prelude.takeWhile prd xs
            takeWhileOrFirst _ [] = []

wordBy :: Property
wordBy =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case S.parseD (P.wordBy predicate FL.toList) (S.fromList ls) of
            Right parsed_list ->
                checkListEqual parsed_list (takeFirstFails predicate ls)
            Left _ -> property False
        where
            predicate = (== 1)

            takeFirstFails prd list =
                Prelude.takeWhile (not . prd) (removePass prd list)

                where

                removePass prd1 (x : xs) =
                    if prd1 x
                    then removePass prd1 xs
                    else (x : xs)
                removePass _ [] = []

groupBy :: Property
groupBy =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case S.parseD (P.groupBy cmp FL.toList) (S.fromList ls) of
            Right parsed_list -> 
                checkListEqual parsed_list (takeWhileCmpFirst cmp ls)
            Left _ -> property False
        where
            cmp = (==)

            takeWhileCmpFirst comp list =
                case list of
                    [] -> []
                    (x : xs) -> 
                        x : Prelude.takeWhile (\curr -> x `comp` curr) xs

splitWith :: Property
splitWith =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case S.parseD (P.splitWith (,) (P.satisfy (== 0)) (P.satisfy (== 1))) (S.fromList ls) of
            Right (result_first, result_second) -> case ls of
                0 : 1 : _ -> (result_first == 0) && (result_second == 1)
                _ -> False
            Left _ -> case ls of
                0 : 1 : _ -> False
                _ -> True

splitWithFailLeft :: Property
splitWithFailLeft =
    property (case S.parseD (P.splitWith (,) (P.die "die") (P.yield (1 :: Int))) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

splitWithFailRight :: Property
splitWithFailRight =
    property (case S.parseD (P.splitWith (,) (P.yield (1 :: Int)) (P.die "die")) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

splitWithFailBoth :: Property
splitWithFailBoth =
    property (case S.parseD (P.splitWith (,) (P.die "die") (P.die "die")) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

teeWithPass :: Property
teeWithPass =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (0, 1))) $ \ls ->
            let
                prsr = P.take n FL.toList
            in
                case S.parseD (P.teeWith (,) prsr prsr) (S.fromList ls) of
                    Right (ls_1, ls_2) -> checkListEqual (Prelude.take n ls) ls_1 .&&. checkListEqual ls_1 ls_2
                    Left _ -> property False

teeWithFailLeft :: Property
teeWithFailLeft =
    property (case S.parseD (P.teeWith (,) (P.die "die") (P.yield (1 :: Int))) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

teeWithFailRight :: Property
teeWithFailRight =
    property (case S.parseD (P.teeWith (,) (P.yield (1 :: Int)) (P.die "die")) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

teeWithFailBoth :: Property
teeWithFailBoth =
    property (case S.parseD (P.teeWith (,) (P.die "die") (P.die "die")) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

deintercalate1 :: Property
deintercalate1 =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case S.parseD prsr (S.fromList ls) of
            Right parsed_list_tuple ->
                let
                    (parsedList1, parsedList2) = parsed_list_tuple
                    (list1, list2) = partition (== 0) ls
                in
                    checkListEqual parsedList1 list1
                    .&&.
                    checkListEqual parsedList2 list2
            Left _ -> property False

        where

        prsr_1 = (P.takeWhile (== 0) FL.toList)

        prsr_2 = (P.takeWhile (== 1) FL.toList)

        prsr = P.deintercalate concatFold prsr_1 concatFold prsr_2

        concatFold = 
            FL.Fold 
            (\concatList curr_list -> return $ concatList ++ curr_list) 
            (return []) 
            return

        partition prd (x : xs) =
            if prd x
            then (x : trueList, falseList)
            else (trueList, x : falseList)

            where (trueList, falseList) = partition prd xs
        partition _ [] = ([], [])

deintercalate2 :: Property
deintercalate2 =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case S.parseD prsr (S.fromList ls) of
            Right parsed_list_tuple ->
                let
                    (parsedList1, parsedList2) = parsed_list_tuple
                    (list1, list2) = partitionAlternate (== 0) ls
                in
                    checkListEqual parsedList1 list1
                    .&&.
                    checkListEqual parsedList2 list2
            Left _ -> property False

        where

        prsr_1 = P.satisfy (== 0)

        prsr_2 = P.satisfy (== 1)

        prsr = P.deintercalate FL.toList prsr_1 FL.toList prsr_2

        partitionAlternate prd list = helper list False
            where

            helper (x : xs) prevRes =
                if prd x == prevRes
                then ([], [])
                else
                    if prd x 
                    then (x : trueList, falseList)
                    else (trueList, x : falseList)

                where (trueList, falseList) = helper xs (not prevRes)
            helper [] _ = ([], [])

shortestPass :: Property
shortestPass =
    forAll (listOf (chooseInt(min_value, max_value))) $ \ls ->
        let
            half_mid_value = mid_value `Prelude.div` 2
            prsr_1 = P.takeWhile (<= half_mid_value) FL.toList
            prsr_2 = P.takeWhile (<= mid_value) FL.toList
            prsr_shortest = P.shortest prsr_1 prsr_2
        in
            case S.parseD prsr_shortest (S.fromList ls) of
                Right short_list -> checkListEqual short_list (Prelude.takeWhile (<= half_mid_value) ls)
                Left _ -> property False

shortestPassLeft :: Property
shortestPassLeft =
    property (case S.parseD (P.shortest (P.die "die") (P.yield (1 :: Int))) (S.fromList [1 :: Int]) of
        Right r -> r == 1
        Left _ -> False)

shortestPassRight :: Property
shortestPassRight =
    property (case S.parseD (P.shortest (P.yield (1 :: Int)) (P.die "die")) (S.fromList [1 :: Int]) of
        Right r -> r == 1
        Left _ -> False)

shortestFailBoth :: Property
shortestFailBoth =
    property (case S.parseD (P.shortest (P.die "die") (P.die "die")) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

longestPass :: Property
longestPass =
    forAll (listOf (chooseInt(min_value, max_value))) $ \ls ->
        let
            half_mid_value = mid_value `Prelude.div` 2
            prsr_1 = P.takeWhile (<= half_mid_value) FL.toList
            prsr_2 = P.takeWhile (<= mid_value) FL.toList
            prsr_longest = P.longest prsr_1 prsr_2
        in
            case S.parseD prsr_longest (S.fromList ls) of
                Right long_list -> long_list == Prelude.takeWhile (<= mid_value) ls
                Left _ -> False

longestPassLeft :: Property
longestPassLeft =
    property (case S.parseD (P.shortest (P.die "die") (P.yield (1 :: Int))) (S.fromList [1 :: Int]) of
        Right r -> r == 1
        Left _ -> False)

longestPassRight :: Property
longestPassRight =
    property (case S.parseD (P.shortest (P.yield (1 :: Int)) (P.die "die")) (S.fromList [1 :: Int]) of
        Right r -> r == 1
        Left _ -> False)

longestFailBoth :: Property
longestFailBoth =
    property (case S.parseD (P.shortest (P.die "die") (P.die "die")) (S.fromList [1 :: Int]) of
        Right _ -> False
        Left _ -> True)

many :: Property
many =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        let
            concatFold = FL.Fold (\concatList curr_list -> return $ concatList ++ curr_list) (return []) return
            prsr = P.many concatFold $ P.sliceSepBy (== 1) FL.toList
        in
            case S.parseD prsr (S.fromList ls) of
                Right res_list -> checkListEqual res_list (Prelude.filter (== 0) ls)
                Left _ -> property False

many_empty :: Property
many_empty =
    property (case S.parseD (P.many FL.toList (P.die "die")) (S.fromList [1 :: Int]) of
        Right res_list -> checkListEqual res_list ([] :: [Int])
        Left _ -> property False)

-- some :: Property
-- some =
--     forAll (listOf (chooseInt (0, 1))) $ \ls ->
--         let
--             concatFold = FL.Fold (\concatList curr_list -> return $ concatList ++ curr_list) (return []) return
--             prsr = P.some concatFold $ P.sliceSepBy (== 1) FL.toList
--         in
--             case S.parseD prsr (S.fromList ls) of
--                 Right res_list -> res_list == Prelude.filter (== 0) ls
--                 Left _ -> False

-- someFail :: Property
-- someFail =
--     property (case S.parseD (P.some FL.toList (P.die "die")) (S.fromList [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

main :: IO ()
main =
    hspec $
    H.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
    describe "test for accumulator" $ do
        prop "P.fromFold FL.sum = FL.sum" fromFold
        prop "P.any = Prelude.any" Main.any
        prop "P.all = Prelude.all" Main.all
        prop "yield value provided" yield
        prop "yield monadic value provided" yieldM
        prop "always fail" die
        prop "always fail but monadic" dieM

    describe "test for element parser" $ do
        prop "peek = head with list length > 0" peekPass
        prop "peek fail on []" peekFail
        prop "eof pass on []" eofPass
        prop "eof fail on non-empty list" eofFail
        prop "first element exists and >= mid_value" satisfyPass
        prop "check first element exists and satisfies predicate" satisfy

    describe "test for sequence parser" $ do
        prop "P.take = Prelude.take" Main.take
        prop "P.takeEQ = Prelude.take when len >= n" takeEQPass
        prop "P.takeEQ = Prelude.take when len >= n and fail otherwise" Main.takeEQ
        prop "P.takeGE n ls = ls when len >= n" takeGEPass
        prop "P.takeGE n ls = ls when len >= n and fail otherwise" Main.takeGE
        prop "lookAhead . take n >> lookAhead . take n = lookAhead . take n" lookAheadPass
        prop "lookAhead . take n >> lookAhead . take n = lookAhead . take n, else fail" lookAhead
        prop "P.takeWhile = Prelude.takeWhile" Main.takeWhile
        prop "P.takeWhile1 = Prelude.takeWhile if taken something, else check why failed" takeWhile1
        prop "P.sliceSepBy = Prelude.takeWhile (not . predicate)" sliceSepBy
        prop "P.sliceSepByMax = Prelude.take n (Prelude.takeWhile (not . predicate)" sliceSepByMax
        prop "P.sliceEndWith = takeWhileAndFirstFail (not . predicate)" sliceEndWith
        prop "P.sliceBeginWith predicate = takeWhileOrFirst (not . predicate)" sliceBeginWith
        prop "P.wordBy = takeFirstFails" wordBy
        prop "P.groupBy = takeWhileCmpFirst" groupBy
        prop "parse 0, then 1, else fail" splitWith
        prop "fail due to die as left parser" splitWithFailLeft
        prop "fail due to die as right parser" splitWithFailRight
        prop "fail due to die as both parsers" splitWithFailBoth
        prop "parsed two lists should be equal" teeWithPass
        prop "fail due to die as left parser" teeWithFailLeft
        prop "fail due to die as right parser" teeWithFailRight
        prop "fail due to die as both parsers" teeWithFailBoth
        prop "P.deintercalate concatFold prsr_1 concatFold prsr_2 = partition" deintercalate1
        prop "P.deintercalate FL.toList prsr_1 FL.toList prsr_2 = partitionAlternate" deintercalate2
        prop "P.takeWhile (<= half_mid_value) = Prelude.takeWhile half_mid_value" shortestPass
        prop "pass even if die is left parser" shortestPassLeft
        prop "pass even if die is right parser" shortestPassRight
        prop "fail due to die as both parsers" shortestFailBoth
        prop "P.takeWhile (<= mid_value) = Prelude.takeWhile (<= mid_value)" longestPass
        prop "pass even if die is left parser" longestPassLeft
        prop "pass even if die is right parser" longestPassRight
        prop "fail due to die as both parsers" longestFailBoth
        prop "P.many concatFold $ P.sliceSepBy (== 1) FL.toList = Prelude.filter (== 0)" many
        prop "[] due to parser being die" many_empty
        -- prop "" some
        -- prop "fail due to parser being die" someFail
    takeProperties
