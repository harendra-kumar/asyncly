{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (catch, ErrorCall(..))
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Char (toLower)
import Data.List
import GHC.Real (infinity)
import System.Environment (getArgs)
import Text.Read (readMaybe)

import BenchShow

------------------------------------------------------------------------------
-- Command line parsing
------------------------------------------------------------------------------

data BenchType
    = Compare String
    | Standard String
    deriving Show

data Options = Options
    { genGraphs :: Bool
    , sortByName :: Bool
    , useGauge :: Bool
    , benchType :: Maybe BenchType
    , fields :: [String]
    , diffStyle :: GroupStyle
    , cutOffPercent :: Double
    } deriving Show

defaultOptions :: Options
defaultOptions = Options False False False Nothing ["time"] PercentDiff 0

setGenGraphs :: Monad m => Bool -> StateT (a, Options) m ()
setGenGraphs val = do
    (args, opts) <- get
    put (args, opts { genGraphs = val })

setSortByName :: Monad m => Bool -> StateT (a, Options) m ()
setSortByName val = do
    (args, opts) <- get
    put (args, opts { sortByName = val })

setUseGauge :: Monad m => Bool -> StateT (a, Options) m ()
setUseGauge val = do
    (args, opts) <- get
    put (args, opts { useGauge = val })

setBenchType :: Monad m => BenchType -> StateT (a, Options) m ()
setBenchType val = do
    (args, opts) <- get
    put (args, opts { benchType = Just val })

setFields :: Monad m => [String] -> StateT (a, Options) m ()
setFields val = do
    (args, opts) <- get
    put (args, opts { fields = val })

setDiff :: Monad m => String -> StateT (a, Options) m ()
setDiff val = do
    (args, opts) <- get
    let cmpStyle =
            case val of
                "absolute" -> Absolute
                "multiples" -> Multiples
                "percent" -> PercentDiff
                x -> error $ "Unknown diff option: " ++ show x
     in put (args, opts { diffStyle = cmpStyle })

setCutOff :: Monad m => String -> StateT (a, Options) m ()
setCutOff val = do
    (args, opts) <- get
    case readMaybe val of
        Just x -> put (args, opts { cutOffPercent = x })
        Nothing -> error $ "Invalid cutoff value: " ++ show val

-- Like the shell "shift" to shift the command line arguments
shift :: StateT ([String], Options) (MaybeT IO) (Maybe String)
shift = do
    s <- get
    case s of
        ([], _) -> return Nothing
        (x : xs, opts) -> put (xs, opts) >> return (Just x)

parseBench :: StateT ([String], Options) (MaybeT IO) ()
parseBench = do
    x <- shift
    case x of
        Just str | "_cmp" `isSuffixOf` str -> setBenchType (Compare str)
        Just str -> setBenchType (Standard str)
        Nothing -> do
                liftIO $ putStrLn "please provide a benchmark type "
                mzero

parseFields :: StateT ([String], Options) (MaybeT IO) ()
parseFields = do
    x <- shift
    case x of
        Just str -> setFields (words str)
        Nothing -> do
                liftIO $ putStrLn
                    "please provide a list of fields after --fields"
                mzero

parseDiff :: StateT ([String], Options) (MaybeT IO) ()
parseDiff = do
    x <- shift
    case x of
        Just str -> setDiff str
        Nothing -> do
                liftIO $ putStrLn "please provide a diff type"
                mzero

parseCutOff :: StateT ([String], Options) (MaybeT IO) ()
parseCutOff = do
    x <- shift
    case x of
        Just str -> setCutOff str
        Nothing -> do
                liftIO $ putStrLn "please provide a cutoff percent"
                mzero

-- totally imperative style option parsing
parseOptions :: IO (Maybe Options)
parseOptions = do
    args <- getArgs
    runMaybeT $ flip evalStateT (args, defaultOptions) $ do
        parseLoop
        fmap snd get

    where

    parseOpt opt =
        case opt of
            "--graphs" -> setGenGraphs True
            "--sort-by-name" -> setSortByName True
            "--use-gauge" -> setUseGauge True
            "--benchmark" -> parseBench
            "--fields" -> parseFields
            "--diff-style" -> parseDiff
            "--diff-cutoff-percent" -> parseCutOff
            str -> do
                liftIO $ putStrLn $ "Unrecognized option " <> str
                mzero

    parseLoop = do
        next <- shift
        case next of
            Just opt -> parseOpt opt >> parseLoop
            Nothing -> return ()

ignoringErr :: IO () -> IO ()
ignoringErr a = catch a (\(ErrorCall err :: ErrorCall) ->
    putStrLn $ "Failed with error:\n" <> err <> "\nSkipping.")

------------------------------------------------------------------------------
-- Generic
------------------------------------------------------------------------------

makeGraphs :: String -> Config -> String -> IO ()
makeGraphs name cfg@Config{..} inputFile =
    ignoringErr $ graph inputFile name cfg

------------------------------------------------------------------------------
-- Arrays
------------------------------------------------------------------------------

showComparisons :: Options -> Config -> FilePath -> FilePath -> IO ()
showComparisons Options{..} cfg inp out =
    let cfg1 = cfg { classifyBenchmark = classifyComparison }
     in if genGraphs
        then ignoringErr $ graph inp "comparison"
                cfg1 { outputDir = Just out
                     , presentation = Groups Absolute
                     }
        else ignoringErr $ report inp Nothing cfg1

    where

    separator = if useGauge then '/' else '.'
    dropComponent = dropWhile (== separator) . dropWhile (/= separator)

    classifyComparison b =
        let b1 =
                if useGauge
                then b
                else dropComponent b --- drop "All." at the beginning
         in Just
            ( takeWhile (/= separator) b1
            , dropComponent b1
            )

------------------------------------------------------------------------------
-- text reports
------------------------------------------------------------------------------

selectBench
    :: Options
    -> (SortColumn -> Maybe GroupStyle -> Either String [(String, Double)])
    -> [String]
selectBench Options{..} f =
    reverse
    $ fmap fst
    $ filter (\(_,y) -> filterPred y)
    $ either
      (const $ either error sortFunc $ f (ColumnIndex 0) (Just PercentDiff))
      sortFunc
      $ f (ColumnIndex 1) (Just PercentDiff)

    where

    sortFunc = if sortByName then sortOn fst else sortOn snd

    cutOffMultiples = 1 + cutOffPercent / 100

    filterPred x =
        case benchType of
            Just (Compare _) ->
                x <= (negate cutOffPercent) || x >= cutOffPercent
            _ -> True

benchShow ::
       Options
    -> Config
    -> (Config -> String -> IO ())
    -> String
    -> FilePath
    -> IO ()
benchShow Options{..} cfg func inp out =
    if genGraphs
    then func cfg {outputDir = Just out} inp
    else ignoringErr $ report inp Nothing cfg

main :: IO ()
main = do
    res <- parseOptions
    case res of
        Nothing -> do
            putStrLn "cannot parse options"
            return ()
        Just opts@Options{fields = fs, benchType = btype} ->
            let cfg = defaultConfig
                    { presentation = Groups (diffStyle opts)
                    , selectBenchmarks = selectBench opts
                    , selectFields = filter
                        ( flip elem (fmap (fmap toLower) fs)
                        . fmap toLower
                        )
                    }
            in case btype of
                Just (Compare str) ->
                    showComparisons opts cfg
                        { title = Just str }
                        ("charts/" ++ str ++ "/results.csv")
                        ("charts/" ++ str)
                Just (Standard str) ->
                    benchShow opts cfg
                        { title = Just str }
                        (makeGraphs str)
                        ("charts/" ++ str ++ "/results.csv")
                        ("charts/" ++ str)
                Nothing ->
                    error "Please specify a benchmark using --benchmark."
