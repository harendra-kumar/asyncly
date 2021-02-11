module Main (main) where

import Control.Exception (SomeException(..), displayException)
import Data.Word (Word8, Word32, Word64)
import Streamly.Test.Common (listEquals, checkListEqual, chooseInt)
import Test.Hspec (Spec, hspec, describe)
import Test.Hspec.QuickCheck
import Test.QuickCheck
       (arbitrary, forAll, elements, Property, property, listOf,
        vectorOf, Gen)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import Prelude hiding (sequence)

import qualified Data.List as List
import qualified Prelude
import qualified Streamly.Internal.Data.Array.Foreign as A
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as P
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Test.Hspec as H

#if MIN_VERSION_QuickCheck(2,14,0)

import Test.QuickCheck (chooseAny)

#else

import System.Random (Random(random))
import Test.QuickCheck.Gen (Gen(MkGen))

-- | Generates a random element over the natural range of `a`.
chooseAny :: Random a => Gen a
chooseAny = MkGen (\r _ -> let (x,_) = random r in x)

#endif


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

-- Accumulator Tests

fromFold :: Property
fromFold =
    forAll (listOf $ chooseInt (min_value, max_value)) $ \ls ->
        case (==) <$> S.parse (P.fromFold FL.sum) (S.fromList ls)
                  <*> S.fold FL.sum (S.fromList ls) of
            Right is_equal -> is_equal
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
        forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
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
        forAll (vectorOf list_length (chooseInt (min_value, max_value))) $ \ls ->
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
    forAll (chooseInt (min_value, max_value)) $ \m ->
        forAll (chooseInt (m, max_value)) $ \n ->
            forAll (chooseInt (m, max_value)) $ \list_length ->
                forAll (vectorOf list_length (chooseInt (min_value, max_value)))
                    $ \ls ->
                        case S.parse (P.takeBetween m n FL.toList)
                                (S.fromList ls) of
                            Right parsed_list ->
                                let lpl = Prelude.length parsed_list
                                in checkListEqual parsed_list
                                    $ Prelude.take lpl ls
                            Left _ -> property False

takeBetween :: Property
takeBetween =
    forAll (chooseInt (min_value, max_value)) $ \m ->
        forAll (chooseInt (min_value, max_value)) $ \n ->
                forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
                let
                    list_length = Prelude.length ls
                in
                    case S.parse (P.takeBetween m n FL.toList)
                            (S.fromList ls) of
                        Right parsed_list ->
                            if m <= list_length && n >= list_length
                            then
                                let lpl = Prelude.length parsed_list
                                in checkListEqual parsed_list
                                    $ Prelude.take lpl ls
                            else property False
                        Left _ -> property (m > n || list_length < m)


-- deintercalate :: Property
-- deintercalate =
--      forAll (chooseInt (min_value, max_value)) $ \n ->
--         forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
--             case S.parse (P.deintercalate FL.drain (P.satisfy odd) FL.drain
--                 (P.satisfy even)) (S.fromList ls)  of
--                 Right parsed_list -> property (length ls == length parsed_list)
--                 Left _ -> property False

takeEQPass :: Property
takeEQPass =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (chooseInt (n, max_value)) $ \list_length ->
            forAll (vectorOf list_length
                        (chooseInt (min_value, max_value))) $ \ls ->
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
                        if n <= list_length
                        then checkListEqual parsed_list (Prelude.take n ls)
                        else property False
                    Left _ -> property (n > list_length)

takeGEPass :: Property
takeGEPass =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (chooseInt (n, max_value)) $ \list_length ->
            forAll (vectorOf list_length (chooseInt (min_value, max_value)))
                $ \ls ->
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
                        if n <= list_length
                        then checkListEqual parsed_list ls
                        else property False
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

sliceSepByP :: Property
sliceSepByP =
    forAll (listOf (chooseInt (min_value, max_value )))  $ \ls ->
        case S.parse (P.sliceSepByP predicate prsr) (S.fromList ls) of
            Right parsed_list ->
                checkListEqual parsed_list (tkwhl ls)
            Left _ -> property False
        where
            predicate = (>= 100)
            prsr = P.many FL.toList (P.satisfy (const True))
            tkwhl ls = Prelude.takeWhile (not . predicate) ls

sliceBeginWith :: Property
sliceBeginWith =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls ->
        let ls1 = 1:ls
        in
            case S.parse parser (S.fromList ls1) of
                Right parsed_list ->
                  if not $ Prelude.null ls1
                  then
                    let tls = Prelude.takeWhile (not . predicate) (tail ls1)
                    in checkListEqual parsed_list $
                      if predicate (head ls1)
                      then head ls1 : tls
                      else Prelude.takeWhile (not . predicate) ls1
                  else property $ Prelude.null parsed_list
                Left _ -> property False
            where
                predicate = odd
                parser = P.sliceBeginWith predicate FL.toList

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
                    if predicate x
                    then
                        checkListEqual parsed_list
                           $ Prelude.takeWhile predicate ls
                    else
                        property False
            Left _ -> case ls of
                [] -> property True
                (x : _) -> property (not $ predicate x)
        where
            predicate = (== 0)

groupBy :: Property
groupBy =
    forAll (listOf (chooseInt (0, 1)))
        $ \ls ->
              case S.parse parser (S.fromList ls) of
                  Right parsed -> checkListEqual parsed (groupByLF ls)
                  Left _ -> property False

    where

    cmp = (==)
    parser = P.groupBy cmp FL.toList
    groupByLF lst
        | null lst = []
        | otherwise = head $ List.groupBy cmp lst


wordBy :: Property
wordBy =
    forAll (listOf (elements [' ', 's']))
        $ \ls ->
              case S.parse parser (S.fromList ls) of
                  Right parsed -> checkListEqual parsed (words' ls)
                  Left _ -> property False

    where

    predicate = (== ' ')
    parser = P.many FL.toList (P.wordBy predicate FL.toList)
    words' lst =
        let wrds = words lst
         in if wrds == [] then [""] else wrds

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

deintercalate :: Property
deintercalate =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        case S.parse prsr (S.fromList ls) of
            Right parsed_list_tuple ->
                parsed_list_tuple == List.partition (== 0) ls
            Left _ -> False

        where
            prsr = P.deintercalate concatFold prsr_1 concatFold prsr_2
            prsr_1 = P.takeWhile (== 0) FL.toList
            prsr_2 = P.takeWhile (== 1) FL.toList
            concatFold = FL.Fold
                            (\concatList curr_list ->
                                return $ FL.Partial $ concatList ++ curr_list)
                            (return []) return

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

many :: Property
many =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        let fldstp conL currL = return $ FL.Partial $ conL ++ currL
            concatFold = FL.Fold fldstp (return (FL.Partial [])) return
            prsr =
                P.many concatFold $ P.fromFold $ FL.sliceSepBy (== 1) FL.toList
        in
            case S.parse prsr (S.fromList ls) of
                Right res_list -> checkListEqual res_list
                                    $ Prelude.filter (== 0) ls
                Left _ -> property False

-- many_empty :: Property
-- many_empty =
--     property (case S.parse (P.many FL.toList (P.die "die")) (S.fromList [1 :: Int]) of
--         Right res_list -> checkListEqual res_list ([] :: [Int])
--         Left _ -> property False)

some :: Property
some =
    forAll (listOf (chooseInt (0, 1))) $ \genLs ->
        let
            ls = 0 : genLs
            fldstp conL currL = return $ FL.Partial $ conL ++ currL
            concatFold = FL.Fold fldstp (return (FL.Partial [])) return
            prsr =
                P.some concatFold $ P.fromFold $ FL.sliceSepBy (== 1) FL.toList
        in
            case S.parse prsr (S.fromList ls) of
                Right res_list -> res_list == Prelude.filter (== 0) ls
                Left _ -> False

-- someFail :: Property
-- someFail =
--     property (case S.parse (P.some FL.toList (P.die "die")) (S.fromList [1 :: Int]) of
--         Right _ -> False
--         Left _ -> True)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

applicative :: Property
applicative =
    forAll (listOf (chooseAny :: Gen Int)) $ \ list1 ->
        forAll (listOf (chooseAny :: Gen Int)) $ \ list2 ->
            let parser =
                    (,)
                        <$> P.fromFold (FL.takeLE (length list1) FL.toList)
                        <*> P.fromFold (FL.takeLE (length list2) FL.toList)
             in monadicIO $ do
                    (olist1, olist2) <-
                        run $ S.parse parser (S.fromList $ list1 ++ list2)
                    listEquals (==) olist1 list1
                    listEquals (==) olist2 list2

sequence :: Property
sequence =
    forAll (vectorOf 11 (listOf (chooseAny :: Gen Int))) $ \ ins ->
        let p xs = P.fromFold (FL.takeLE (length xs) FL.toList)
         in monadicIO $ do
                outs <- run $
                        S.parse
                            (Prelude.sequence $ fmap p ins)
                            (S.fromList $ concat ins)
                listEquals (==) outs ins

monad :: Property
monad =
    forAll (listOf (chooseAny :: Gen Int)) $ \ list1 ->
        forAll (listOf (chooseAny :: Gen Int)) $ \ list2 ->
            let parser = do
                    olist1 <- P.fromFold (FL.takeLE (length list1) FL.toList)
                    olist2 <- P.fromFold (FL.takeLE (length list2) FL.toList)
                    return (olist1, olist2)
             in monadicIO $ do
                    (olist1, olist2) <-
                        run $ S.parse parser (S.fromList $ list1 ++ list2)
                    listEquals (==) olist1 list1
                    listEquals (==) olist2 list2

-------------------------------------------------------------------------------
-- Stream parsing
-------------------------------------------------------------------------------

parseMany :: Property
parseMany =
    forAll (chooseInt (1,100)) $ \len ->
        forAll (listOf (vectorOf len (chooseAny :: Gen Int))) $ \ ins ->
            monadicIO $ do
                outs <- do
                    let p = P.fromFold $ FL.takeLE len FL.toList
                    run
                        $ S.toList
                        $ S.parseMany p (S.fromList $ concat ins)
                listEquals (==) outs ins

-------------------------------------------------------------------------------
-- Test for a particular case hit during fs events testing
-------------------------------------------------------------------------------

evId :: [Word8]
evId = [96,238,17,9,0,0,0,0]

evFlags :: [Word8]
evFlags = [0,4,1,0,0,0,0,0]

evPathLen :: [Word8]
evPathLen = [71,0,0,0,0,0,0,0]

evPath :: [Word8]
evPath =
    [47,85,115,101,114,115,47,118,111,108,47,118,101,109,98,97,47,99,111,109
    ,112,111,115,101,119,101 ,108,108,45,116,101,99,104,47,69,110,103,47,112
    ,114,111,106,101,99,116,115,47,115,116,114,101,97,109,108,121,47,115,116
    ,114,101,97,109,108,121,47,116,109,112,47,122,122
    ]

event :: [Word8]
event = evId ++ evFlags ++ evPathLen ++ evPath

data Event = Event
   { eventId :: Word64
   , eventFlags :: Word32
   , eventAbsPath :: A.Array Word8
   } deriving (Show, Ord, Eq)

readOneEvent :: P.Parser IO Word8 Event
readOneEvent = do
    arr <- P.takeEQ 24 (A.writeN 24)
    let arr1 = A.unsafeCast arr :: A.Array Word64
        eid = A.unsafeIndex arr1 0
        eflags = A.unsafeIndex arr1 1
        pathLen = fromIntegral $ A.unsafeIndex arr1 2
    -- XXX handle if pathLen is 0
    path <- P.takeEQ pathLen (A.writeN pathLen)
    return $ Event
        { eventId = eid
        , eventFlags = fromIntegral eflags
        , eventAbsPath = path
        }

parseMany2Events :: Property
parseMany2Events =
    monadicIO $ do
        xs <-
            ( run
            $ S.toList
            $ S.parseMany readOneEvent
            $ S.fromList (concat (replicate 2 event))
            )
        assert (length xs == 2)
        -- XXX assuming little endian machine
        let ev = Event
                { eventId = 152170080
                , eventFlags = 66560
                , eventAbsPath = A.fromList evPath
                }
         in listEquals (==) xs (replicate 2 ev)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main =
    hspec $
    H.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
    describe "Instances" $ do
        prop "applicative" applicative
        prop "monad" monad
        prop "sequence" sequence

    describe "Stream parsing" $ do
        prop "parseMany" parseMany
        prop "parseMany2Events" parseMany2Events

    describe "test for accumulator" $ do
        prop "P.fromFold FL.sum = FL.sum" fromFold
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
        prop "P.takeBetween = Prelude.take when len >= m and len <= n"
            takeBetweenPass
        prop ("P.takeBetween = Prelude.take when len >= m and len <= n and fail"
              ++ "otherwise fail") Main.takeBetween
        prop "P.deintercalate test" deintercalate
        prop "P.takeEQ = Prelude.take when len >= n" takeEQPass
        prop "P.takeEQ = Prelude.take when len >= n and fail otherwise"
            Main.takeEQ
        prop "P.takeGE n ls = ls when len >= n" takeGEPass
        prop "P.takeGE n ls = ls when len >= n and fail otherwise" Main.takeGE
        -- prop "lookAhead . take n >> lookAhead . take n = lookAhead . take n" lookAheadPass
        -- prop "Fail when stream length exceeded" lookAheadFail
        -- prop "lookAhead . take n >> lookAhead . take n = lookAhead . take n, else fail" lookAhead
        prop "P.sliceSepByP test" Main.sliceSepByP
        prop ("P.sliceBeginWith pred = head : Prelude.takeWhile (not . pred)"
                ++ " tail") sliceBeginWith
        prop "P.takeWhile = Prelude.takeWhile" Main.takeWhile
        prop ("P.takeWhile1 = Prelude.takeWhile if taken something,"
                ++ " else check why failed") takeWhile1
        prop "P.groupBy = Prelude.head . Prelude.groupBy" groupBy
        prop "many (P.wordBy ' ') = words'" wordBy
        -- prop "" splitWithPass
        -- prop "" splitWithFailLeft
        -- prop "" splitWithFailRight
        -- prop "" splitWithFailBoth
        -- prop "" teeWithPass
        -- prop "" teeWithFailLeft
        -- prop "" teeWithFailRight
        -- prop "" teeWithFailBoth
        -- prop "" deintercalate
        -- prop "" shortestPass
        -- prop "" shortestFailLeft
        -- prop "" shortestFailRight
        -- prop "" shortestFailBoth
        prop ("P.many concatFold $ P.sliceSepBy (== 1) FL.toList ="
                ++ "Prelude.filter (== 0)") many
        -- prop "[] due to parser being die" many_empty
        prop ("P.some concatFold $ P.sliceSepBy (== 1) FL.toList ="
                ++ "Prelude.filter (== 0)") some
        -- prop "fail due to parser being die" someFail
    takeProperties
