module Main (main) where

import qualified Streamly.Internal.Data.Parser as P
import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Internal.Data.Fold as FL

-- import Data.List (partition)

import Test.Hspec(Spec, hspec, describe)
import qualified Test.Hspec as H
import Test.Hspec.QuickCheck
import Test.QuickCheck (arbitrary, forAll, choose, elements, Property,
                        property, listOf, vectorOf, counterexample, Gen, (.&&.))

import Test.QuickCheck.Monadic (monadicIO, PropertyM, assert, monitor)
import Control.Exception (SomeException(..), displayException)
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
        case (==) <$> (S.parse (P.fromFold FL.sum) (S.fromList ls)) <*> (S.fold FL.sum (S.fromList ls)) of
            Right is_equal -> is_equal
            Left _ -> False

any :: Property
any =
    forAll (listOf $ chooseInt (min_value, max_value)) $ \ls ->
        case S.parse (P.any (> mid_value)) (S.fromList ls) of
            Right r -> r == (Prelude.any (> mid_value) ls)
            Left _ -> False

all :: Property
all =
    forAll (listOf $ chooseInt (min_value, max_value)) $ \ls ->
        case S.parse (P.all (> mid_value)) (S.fromList ls) of
            Right r -> r == (Prelude.all (> mid_value) ls)
            Left _ -> False

yield :: Property
yield =
    forAll (chooseInt (min_value, max_value)) $ \x ->
        case S.parse (P.yield x) (S.fromList [1 :: Int]) of
            Right r -> r == x
            Left _ -> False

yieldM :: Property
yieldM =
    forAll (chooseInt (min_value, max_value)) $ \x ->
        case S.parse (P.yieldM $ return x) (S.fromList [1 :: Int]) of
            Right r -> r == x
            Left _ -> False

die :: Property
die =
    property $
    case S.parse (P.die "die test") (S.fromList [0 :: Int]) of
        Right _ -> False
        Left _ -> True

dieM :: Property
dieM =
    property $
    case S.parse (P.dieM (Right "die test")) (S.fromList [0 :: Int]) of
        Right _ -> False
        Left _ -> True

parserFail :: Property
parserFail =
    property $
    case S.parse (fail err) (S.fromList [0 :: Int]) of
        Right _ -> False
        Left (SomeException e) -> err == displayException e
  where
    err = "Testing MonadFail.fail."

-- Element Parser Tests

peekPass :: Property
peekPass =
    forAll (chooseInt (1, max_length)) $ \list_length ->
        forAll 
            (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
            case S.parse P.peek (S.fromList ls) of
                Right head_value -> case ls of
                    head_ls : _ -> head_value == head_ls
                    _ -> False
                Left _ -> False

peekFail :: Property
peekFail =
    property (case S.parse P.peek (S.fromList []) of
        Right _ -> False
        Left _ -> True)

eofPass :: Property
eofPass =
    property (case S.parse P.eof (S.fromList []) of
        Right _ -> True
        Left _ -> False)

eofFail :: Property
eofFail =
    forAll (chooseInt (1, max_length)) $ \list_length ->
        forAll 
            (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
            case S.parse P.eof (S.fromList ls) of
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
                case S.parse (P.satisfy predicate) (S.fromList ls) of
                    Right r -> r == first_element
                    Left _ -> False

satisfy :: Property
satisfy =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
        case S.parse (P.satisfy predicate) (S.fromList ls) of
            Right r -> case ls of
                [] -> False
                (x : _) -> predicate x && (r == x)
            Left _ -> case ls of
                [] -> True
                (x : _) -> not $ predicate x
        where
            predicate = (>= mid_value)

-- Sequence Parsers Tests

takeBetweenPass :: Property
takeBetweenPass =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
        let len = Prelude.length ls
        in
        forAll (chooseInt (min_value, len)) $ \low ->
            forAll (chooseInt (low, max_value)) $ \high ->
                case S.parse (P.takeBetween low high FL.toList) (S.fromList ls) of
                    Right parsed_list ->
                        checkListEqual parsed_list (Prelude.take high ls)
                    Left _ -> property False

takeBetween :: Property
takeBetween =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
        forAll (chooseInt (min_value, max_value)) $ \low ->
            forAll (chooseInt (low, max_value)) $ \high ->
                case S.parse (P.takeBetween low high FL.toList) (S.fromList ls) of
                    Right parsed_list ->
                        checkListEqual parsed_list (Prelude.take high ls)
                    Left _ -> property $ Prelude.length ls < low

take :: Property
take =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
            case S.parse (P.take n FL.toList) (S.fromList ls) of
                Right parsed_list -> 
                    checkListEqual parsed_list (Prelude.take n ls)
                Left _ -> property False

takeEQPass :: Property
takeEQPass =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (chooseInt (n, max_value)) $ \len ->
            forAll (vectorOf len (chooseInt (min_value, max_value))) $ \ls ->
                case S.parse (P.takeEQ n FL.toList) (S.fromList ls) of
                    Right parsed_list -> 
                        checkListEqual parsed_list (Prelude.take n ls)
                    Left _ -> property False

takeEQ :: Property
takeEQ =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
            let
                list_length = Prelude.length ls
            in
                case S.parse (P.takeEQ n FL.toList) (S.fromList ls) of
                    Right parsed_list ->
                        if (n <= list_length) then
                            checkListEqual parsed_list (Prelude.take n ls)
                        else
                            property False
                    Left _ -> property (n > list_length)

takeGEPass :: Property
takeGEPass =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (chooseInt (n, max_value)) $ \len ->
            forAll (vectorOf len (chooseInt (min_value, max_value))) $ \ls ->
                case S.parse (P.takeGE n FL.toList) (S.fromList ls) of
                    Right parsed_list -> checkListEqual parsed_list ls
                    Left _ -> property False

takeGE :: Property
takeGE =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
            let
                list_length = Prelude.length ls
            in
                case S.parse (P.takeGE n FL.toList) (S.fromList ls) of
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
            case S.parse (tk n FL.toList) (S.fromList ls) of
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

-- lookAheadPass :: Property
-- lookAheadPass =
--     forAll (chooseInt (min_value + 1, max_value)) $ \n ->
--         let
--             takeWithoutConsume = P.lookAhead $ P.take n FL.toList
--             parseTwice = do
--                 parsed_list_1 <- takeWithoutConsume
--                 parsed_list_2 <- takeWithoutConsume
--                 return (parsed_list_1, parsed_list_2)
--         in
--             forAll (chooseInt (n, max_value)) $ \list_length ->
--                 forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
--                     case S.parse parseTwice (S.fromList ls) of
--                         Right (ls_1, ls_2) -> checkListEqual ls_1 ls_2 .&&. checkListEqual ls_1 (Prelude.take n ls)
--                         Left _ -> property $ False

-- lookAheadFail :: Property
-- lookAheadFail =
--     forAll (chooseInt (min_value + 1, max_value)) $ \n ->
--         let
--             takeWithoutConsume = P.lookAhead $ P.take n FL.toList
--             parseTwice = do
--                 parsed_list_1 <- takeWithoutConsume
--                 parsed_list_2 <- takeWithoutConsume
--                 return (parsed_list_1, parsed_list_2)
--         in
--             forAll (chooseInt (min_value, n - 1)) $ \list_length ->
--                 forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
--                     case S.parse parseTwice (S.fromList ls) of
--                         Right _ -> False
--                         Left _ -> True

-- lookAhead :: Property
-- lookAhead =
--     forAll (chooseInt (min_value, max_value)) $ \n ->
--         let
--             takeWithoutConsume = P.lookAhead $ P.take n FL.toList
--             parseTwice = do
--                 parsed_list_1 <- takeWithoutConsume
--                 parsed_list_2 <- takeWithoutConsume
--                 return (parsed_list_1, parsed_list_2)
--         in
--             forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
--                 case S.parse parseTwice (S.fromList ls) of
--                     Right (ls_1, ls_2) -> checkListEqual ls_1 ls_2 .&&. checkListEqual ls_1 (Prelude.take n ls)
--                     Left _ -> property ((list_length < n) || (list_length == n && n == 0))
--                         where
--                             list_length = Prelude.length ls

takeWhileP1 :: Property
takeWhileP1 =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        forAll (chooseInt (min_value, max_value)) $ \n ->
            let
                predicate = (== 1)

                prsr = P.takeWhileP predicate $ P.take n FL.toList

                takeWhileTillLen maxLen prd list = 
                    Prelude.take maxLen $ Prelude.takeWhile prd list
            in
                case S.parse prsr (S.fromList ls) of
                    Right parsed_list -> 
                        checkListEqual 
                        parsed_list
                        (takeWhileTillLen n predicate ls)
                    Left _ -> property False

takeWhile :: Property
takeWhile =
    forAll (listOf (chooseInt (0, 1))) $ \ ls ->
        case S.parse (P.takeWhile predicate FL.toList) (S.fromList ls) of
            Right parsed_list -> 
                checkListEqual parsed_list (Prelude.takeWhile predicate ls)
            Left _ -> property False
        where
            predicate = (== 0)

takeWhile1 :: Property
takeWhile1 =
    forAll (listOf (chooseInt (0, 1))) $ \ ls ->
        case S.parse (P.takeWhile1 predicate  FL.toList) (S.fromList ls) of
            Right parsed_list -> case ls of
                [] -> property False
                (x : _) ->
                    if predicate x then
                        checkListEqual 
                        parsed_list 
                        (Prelude.takeWhile predicate ls)
                    else
                        property False
            Left _ -> case ls of
                [] -> property True
                (x : _) -> property (not $ predicate x)
        where
            predicate = (== 0)

sliceSepByP1 :: Property
sliceSepByP1 =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        forAll (chooseInt (min_value, max_value)) $ \n ->
            let
                predicate = (== 1)

                prsr = P.sliceSepByP predicate $ P.take n FL.toList

                takeWhileTillLen maxLen prd list = 
                    Prelude.take maxLen $ Prelude.takeWhile (not . prd) list
            in
                case S.parse prsr (S.fromList ls) of
                    Right parsed_list -> 
                        checkListEqual 
                        parsed_list
                        (takeWhileTillLen n predicate ls)
                    Left _ -> property False

sliceSepBy :: Property
sliceSepBy =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case S.parse (P.sliceSepBy predicate FL.toList) (S.fromList ls) of
            Right parsed_list -> 
                checkListEqual 
                parsed_list 
                (Prelude.takeWhile (not . predicate) ls)
            Left _ -> property False
        where
            predicate = (== 1)

sliceSepWith :: Property
sliceSepWith =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case S.parse (P.sliceSepWith predicate FL.toList) (S.fromList ls) of
            Right parsed_list -> 
                checkListEqual parsed_list $ takeFirstOrUntilSep predicate ls
            Left _ -> property False
        where
            predicate = (== 1)

            takeFirstOrUntilSep prd (x : xs) =
                if prd x
                then [x]
                else x : Prelude.takeWhile (not . prd) xs
            takeFirstOrUntilSep _ [] = []

-- sliceSepByMax :: Property
-- sliceSepByMax =
--     forAll (chooseInt (min_value, max_value)) $ \n ->
--         forAll (listOf (chooseInt (0, 1))) $ \ls ->
--             case S.parse (P.sliceSepByMax predicate n FL.toList) (S.fromList ls) of
--                 Right parsed_list -> checkListEqual parsed_list (Prelude.take n (Prelude.takeWhile (not . predicate) ls))
--                 Left _ -> property False
--             where
--                 predicate = (== 1)

sliceEndWith1 :: Property
sliceEndWith1 =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case S.parse (P.sliceEndWith predicate FL.toList) (S.fromList ls) of
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

sliceEndWith2 :: Property
sliceEndWith2 =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        let
            strm = S.fromList ls

            predicate = (==0)

            eitherParsedList = 
                S.toList $ 
                    S.parseMany (P.sliceEndWith predicate FL.toList) strm

            eitherSplitList =
                case ls of
                    [] -> return [[]]
                    _ ->
                        if last ls == 0
                        then S.toList $ S.append strm1 (S.fromList [[]])
                        else S.toList strm1

                        where 

                        strm1 = S.splitWithSuffix predicate FL.toList strm
        in
            case eitherParsedList of
                Left _ -> property False
                Right parsedList ->
                    case eitherSplitList of
                        Left _ -> property False
                        Right splitList -> checkListEqual parsedList splitList

sliceBeginWith :: Property
sliceBeginWith =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case S.parse (P.sliceBeginWith predicate FL.toList) (S.fromList ls) of
            Right parsed_list -> 
                checkListEqual parsed_list (takeWhileOrFirst (not . predicate) ls)
            Left _ -> property False
        where
            predicate = (== 1)

            takeWhileOrFirst prd (x : xs) = x : Prelude.takeWhile prd xs
            takeWhileOrFirst _ [] = []

escapedSliceSepBy :: Property
escapedSliceSepBy =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
        let
            isSep = even

            isEsc x = x `mod` 6 == 0

            prsr = P.escapedSliceSepBy isSep isEsc FL.toList

            escapeSep maybePrevEsc [] =
                case maybePrevEsc of
                    Nothing -> []
                    Just prevEsc -> [prevEsc]
            escapeSep maybePrevEsc (x : xs) =
                case maybePrevEsc of
                    Nothing ->
                        if isEsc x
                        then escapeSep (Just x) xs
                        else
                            if isSep x
                            then []
                            else x : escapeSep Nothing xs
                    Just prevEsc ->
                        if isSep x || isEsc x
                        then x : escapeSep Nothing xs
                        else
                            if isSep prevEsc
                            then []
                            else prevEsc : x : escapeSep Nothing xs
        in
            case S.parse prsr (S.fromList ls) of
                Right parsed_list -> checkListEqual parsed_list $ escapeSep Nothing ls
                _ -> property False

escapedFrameBy :: Property
escapedFrameBy =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
        let
            isBegin = (== 0)

            isEnd = (== 1)

            isEsc = (== 2)

            prsr = P.escapedFrameBy isBegin isEnd isEsc FL.toList

            checkPass (x : xs) maybePrevEsc openMinusClose =
                case maybePrevEsc of
                    Nothing ->
                        if isEsc x
                        then checkPass xs (Just x) openMinusClose
                        else
                            if isBegin x
                            then checkPass xs Nothing (openMinusClose + 1)
                            else
                                if isEnd x
                                then
                                    case openMinusClose of
                                        0 -> False
                                        1 -> True
                                        _ -> 
                                            checkPass 
                                            xs 
                                            Nothing
                                            (openMinusClose - 1)
                                else
                                    checkPass xs Nothing openMinusClose
                    Just _ -> checkPass xs Nothing openMinusClose
            checkPass [] _ _ = False

            escapeFrame begin end escape l =
                let
                    helper (x : xs) maybePrevEsc openMinusClose =
                        case maybePrevEsc of
                            Nothing ->
                                if escape x
                                then helper xs (Just x) openMinusClose
                                else
                                    if begin x
                                    then helper xs Nothing (openMinusClose + 1)
                                    else
                                        if end x
                                        then
                                            if openMinusClose - 1 == 0
                                            then []
                                            else 
                                                helper 
                                                xs 
                                                Nothing 
                                                (openMinusClose - 1)
                                        else
                                            x : helper xs Nothing openMinusClose
                            Just prevEsc ->
                                if escape x || begin x || end x
                                then x : helper xs Nothing openMinusClose
                                else
                                    prevEsc : x : helper xs Nothing openMinusClose
                    helper [] _ _ = error "Cannot Reach Here"
                in
                    helper l Nothing (0 :: Int)           
        in
            case S.parse prsr (S.fromList ls) of
                Right parsed_list ->
                    if checkPass ls Nothing (0 :: Int)
                    then checkListEqual parsed_list $
                        escapeFrame isBegin isEnd isEsc ls
                    else property False
                Left _ ->
                    if checkPass ls Nothing (0 :: Int)
                    then property False
                    else property True

escapedFrameByPass :: Property
escapedFrameByPass =
    forAll (listOf (chooseInt (min_value, max_value))) $ \list ->
        let
            ls = (0 : list) ++ (Prelude.replicate (Prelude.length list + 1) 1)

            isBegin = (== 0)

            isEnd = (== 1)

            isEsc = (== 2)

            prsr = P.escapedFrameBy isBegin isEnd isEsc FL.toList

            escapeFrame begin end escape l =
                let
                    helper (x : xs) maybePrevEsc openMinusClose =
                        case maybePrevEsc of
                            Nothing ->
                                if escape x
                                then helper xs (Just x) openMinusClose
                                else
                                    if begin x
                                    then helper xs Nothing (openMinusClose + 1)
                                    else
                                        if end x
                                        then
                                            if openMinusClose - 1 == 0
                                            then []
                                            else 
                                                helper 
                                                xs 
                                                Nothing 
                                                (openMinusClose - 1)
                                        else
                                            x : helper xs Nothing openMinusClose
                            Just prevEsc ->
                                if escape x || begin x || end x
                                then x : helper xs Nothing openMinusClose
                                else
                                    prevEsc : x : helper xs Nothing openMinusClose
                    helper [] _ _ = error "Cannot Reach Here"
                in
                    helper l Nothing (0 :: Int)           
        in
            case S.parse prsr (S.fromList ls) of
                Right parsed_list -> 
                    checkListEqual parsed_list $ escapeFrame isBegin isEnd isEsc ls
                _ -> property False

escapedFrameByFail1 :: Property
escapedFrameByFail1 =
    let
        msg = "Element found to satisfy more than one predicate"

        isBegin = (== 0)

        isEnd = (== 0)

        isEsc = (== 2)

        prsr = P.escapedFrameBy isBegin isEnd isEsc FL.toList

        ls = [0 :: Int]
    in
        case S.parse prsr (S.fromList ls) of
            Right _ -> property False
            Left err -> property $ (displayException err == msg)

escapedFrameByFail2 :: Property
escapedFrameByFail2 =
    let
        msg = "Element found to satisfy more than one predicate"

        isBegin = (== 0)

        isEnd = (== 1)

        isEsc = (== 1)

        prsr = P.escapedFrameBy isBegin isEnd isEsc FL.toList

        ls = [1 :: Int]
    in
        case S.parse prsr (S.fromList ls) of
            Right _ -> property False
            Left err -> property $ (displayException err == msg)

escapedFrameByFail3 :: Property
escapedFrameByFail3 =
    let
        msg = "Element found to satisfy more than one predicate"

        isBegin = (== 2)

        isEnd = (== 1)

        isEsc = (== 2)

        prsr = P.escapedFrameBy isBegin isEnd isEsc FL.toList

        ls = [2 :: Int]
    in
        case S.parse prsr (S.fromList ls) of
            Right _ -> property False
            Left err -> property $ (displayException err == msg)

wordBy1 :: Property
wordBy1 =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case S.parse (P.wordBy predicate FL.toList) (S.fromList ls) of
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

wordBy2 :: Property
wordBy2 =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        let
            strm = S.fromList ls

            predicate = (==0)

            eitherParsedList = 
                S.toList $ 
                    S.parseMany (P.wordBy predicate FL.toList) strm

            eitherSplitList =
                if Prelude.takeWhile predicate ls == ls
                then return [[]]
                else S.toList $
                    S.wordsBy predicate (FL.toList) strm
        in
            case eitherParsedList of
                Left _ -> property False
                Right parsedList ->
                    case eitherSplitList of
                        Left _ -> property False
                        Right splitList -> checkListEqual parsedList splitList

groupBy1 :: Property
groupBy1 =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case S.parse (P.groupBy cmp FL.toList) (S.fromList ls) of
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

groupBy2 :: Property
groupBy2 =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
        let
            strm = S.fromList ls

            cmp = (>)

            eitherParsedList = 
                S.toList $ 
                    S.parseMany (P.groupBy (\x y -> cmp y x) FL.toList) strm

            eitherGroupList =
                case ls of
                    [] -> return [[]]
                    _ -> S.toList $ S.groupsBy cmp (FL.toList) strm
        in
            case eitherParsedList of
                Left _ -> property False
                Right parsedList ->
                    case eitherGroupList of
                        Left _ -> property False
                        Right groupList -> checkListEqual parsedList groupList

-- splitWithPass :: Property
-- splitWithPass =
--     forAll (listOf (chooseInt (0, 1))) $ \ls ->
--         case S.parse (P.splitWith (,) (P.satisfy (== 0)) (P.satisfy (== 1))) (S.fromList ls) of
--             Right (result_first, result_second) -> case ls of
--                 0 : 1 : _ -> (result_first == 0) && (result_second == 1)
--                 _ -> False
--             Left _ -> case ls of
--                 0 : 1 : _ -> False
--                 _ -> True

-- splitWithFailLeft :: Property
-- splitWithFailLeft =
--     property (case S.parse (P.splitWith (,) (P.die "die") (P.yield (1 :: Int))) (S.fromList [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

-- splitWithFailRight :: Property
-- splitWithFailRight =
--     property (case S.parse (P.splitWith (,) (P.yield (1 :: Int)) (P.die "die")) (S.fromList [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

-- splitWithFailBoth :: Property
-- splitWithFailBoth =
--     property (case S.parse (P.splitWith (,) (P.die "die") (P.die "die")) (S.fromList [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

-- teeWithPass :: Property
-- teeWithPass =
--     forAll (chooseInt (0, 10000)) $ \n ->
--         forAll (listOf (chooseInt (0, 1))) $ \ls ->
--             let
--                 prsr = P.take n FL.toList
--             in
--                 case S.parse (P.teeWith (,) prsr prsr) (S.fromList ls) of
--                     Right (ls_1, ls_2) -> checkListEqual (Prelude.take n ls) ls_1 .&&. checkListEqual ls_1 ls_2
--                     Left _ -> property False

-- teeWithFailLeft :: Property
-- teeWithFailLeft =
--     property (case S.parse (P.teeWith (,) (P.die "die") (P.yield (1 :: Int))) (S.fromList [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

-- teeWithFailRight :: Property
-- teeWithFailRight =
--     property (case S.parse (P.teeWith (,) (P.yield (1 :: Int)) (P.die "die")) (S.fromList [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

-- teeWithFailBoth :: Property
-- teeWithFailBoth =
--     property (case S.parse (P.teeWith (,) (P.die "die") (P.die "die")) (S.fromList [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

deintercalate1 :: Property
deintercalate1 =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case S.parse prsr (S.fromList ls) of
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
        case S.parse prsr (S.fromList ls) of
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

-- shortestPass :: Property
-- shortestPass =
--     forAll (listOf (chooseInt(min_value, max_value))) $ \ls ->
--         let
--             prsr_1 = P.takeWhile (<= (mid_value `Prelude.div` 2)) FL.toList
--             prsr_2 = P.takeWhile (<= mid_value) FL.toList
--             prsr_shortest = P.shortest prsr_1 prsr_2
--         in
--             case S.parse prsr_shortest (S.fromList ls) of
--                 Right short_list -> checkListEqual short_list (Prelude.takeWhile (<= 2500) ls)
--                 Left _ -> property False

-- shortestFailLeft :: Property
-- shortestFailLeft =
--     property (case S.parse (P.shortest (P.die "die") (P.yield (1 :: Int))) (S.fromList [1 :: Int]) of
--         Right r -> r == 1
--         Left _ -> False)

-- shortestFailRight :: Property
-- shortestFailRight =
--     property (case S.parse (P.shortest (P.yield (1 :: Int)) (P.die "die")) (S.fromList [1 :: Int]) of
--         Right r -> r == 1
--         Left _ -> False)

-- shortestFailBoth :: Property
-- shortestFailBoth =
--     property (case S.parse (P.shortest (P.die "die") (P.die "die")) (S.fromList [1 :: Int]) of
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
        prop "fail err = Left (SomeException (ParseError err))" parserFail
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
        prop "P.takeBetween low high = Prelude.take high when low <= len" takeBetweenPass
        prop "P.takeBetween low high = Prelude.take high when low <= len and fail otherwise" takeBetween
        prop "P.take = Prelude.take" Main.take
        prop "P.takeEQ = Prelude.take when len >= n" takeEQPass
        prop "P.takeEQ = Prelude.take when len >= n and fail otherwise" Main.takeEQ
        prop "P.takeGE n ls = ls when len >= n" takeGEPass
        prop "P.takeGE n ls = ls when len >= n and fail otherwise" Main.takeGE
        -- prop "lookAhead . take n >> lookAhead . take n = lookAhead . take n" lookAheadPass
        -- prop "Fail when stream length exceeded" lookAheadFail
        -- prop "lookAhead . take n >> lookAhead . take n = lookAhead . take n, else fail" lookAhead
        prop "P.takeWhileP prd P.take = takeWhileMaxLen prd" takeWhileP1
        prop "P.takeWhile = Prelude.takeWhile" Main.takeWhile
        prop "P.takeWhile = Prelude.takeWhile if taken something, else check why failed" takeWhile1
        prop "P.sliceSepByP prd P.take = takeWhileMaxLen (not . prd)" sliceSepByP1
        prop "P.sliceSepBy = Prelude.takeWhile (not . predicate)" sliceSepBy
        -- prop "test for sliceSepByMax function" sliceSepByMax
        prop "P.sliceSepWith = takeFirstOrUntilSep" sliceSepWith
        prop "P.sliceEndWith = takeWhileAndFirstFail (not . predicate)" sliceEndWith1
        prop "similar to S.splitWithSuffix pred f = S.splitParse (PR.sliceEndWith pred f)" sliceEndWith2
        prop "P.sliceBeginWith predicate = takeWhileOrFirst (not . predicate)" sliceBeginWith
        prop "P.escapedSliceSepBy = escapeSep Nothing" escapedSliceSepBy
        prop "P.escapedFrameBy = escapeFrame - when pass" escapedFrameBy
        prop "P.escapedFrameBy = escapeFrame - always pass" escapedFrameByPass
        prop "begin = end" escapedFrameByFail1
        prop "end = escape" escapedFrameByFail2
        prop "escape = begin" escapedFrameByFail3
        prop "P.wordBy = takeFirstFails" wordBy1
        prop "similar to S.wordsBy pred f = S.splitParse (PR.wordBy pred f)" wordBy2
        prop "P.groupBy = takeWhileCmpFirst" groupBy1
        prop "S.groupsBy cmp f = S.splitParse (PR.groupBy cmp f)" groupBy2
        -- prop "pass test for splitWith function" splitWithPass
        -- prop "left fail test for splitWith function" splitWithFailLeft
        -- prop "right fail test for splitWith function" splitWithFailRight
        -- prop "both fail test for splitWith function" splitWithFailBoth
        -- prop "pass test for teeWith function" teeWithPass
        -- prop "left fail test for teeWith function" teeWithFailLeft
        -- prop "right fail test for teeWith function" teeWithFailRight
        -- prop "both fail test for teeWith function" teeWithFailBoth
        prop "P.deintercalate concatFold prsr_1 concatFold prsr_2 = partition" deintercalate1
        prop "P.deintercalate FL.toList prsr_1 FL.toList prsr_2 = partitionAlternate" deintercalate2
        -- prop "pass test for shortest function" shortestPass
        -- prop "left fail test for shortest function" shortestFailLeft
        -- prop "right fail test for shortest function" shortestFailRight
        -- prop "both fail test for shortest function" shortestFailBoth
    takeProperties
