import Data.Function ((&))
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import GHC.Float (int2Double)
import System.Environment (getArgs)

import qualified Data.Map.Strict as M
import qualified Data.List as L

data MkSecState s i r
    = Begin s
    | Parsing1 s i
    | Parsing2 s i r

indexName :: Int
indexName = 0

indexAllocated :: Int
indexAllocated = 1

readyFileParsingAnd :: (MkSecState [String] i a -> r) -> FilePath -> IO r
readyFileParsingAnd f fp = f . Begin . lines <$> readFile fp

headingPrefix :: String
headingPrefix = "name"

parseCSVAnd ::
       (Map String Int -> Map String Int -> r)
    -> MkSecState [String] [(String, Int)] (Map String Int)
    -> r
parseCSVAnd f ps =
    case ps of
        Begin (x:xs)
            | headingPrefix `isPrefixOf` x -> parseCSVAnd f $ Parsing1 xs []
            | otherwise -> error "Incorrect beginning"
        Parsing1 (x:xs) ys ->
            if headingPrefix `isPrefixOf` x
            then parseCSVAnd f $ Parsing2 xs [] (M.fromList ys)
            else parseCSVAnd f $ Parsing1 xs ((format x) : ys)
        Parsing1 [] _ -> error "Only one section exists"
        Parsing2 (x:xs) ys r -> parseCSVAnd f $ Parsing2 xs ((format x) : ys) r
        Parsing2 [] ys r -> f r $ M.fromList ys

    where

    splitOn f xs =
        let (a, b) = break f xs
         in a : splitOn f (tail b)

    format x =
        let l = splitOn (== ',') x
         in (l !! indexName, read (l !! indexAllocated) :: Int)

diffPercentAnd ::
       (Map String Double -> r) -> Map String Int -> Map String Int -> r
diffPercentAnd f a b =
    M.intersectionWith (\x y -> (1 - (int2Double y) / (int2Double x)) * 100) a b
        & f

toListWorstAnd :: ([(String, Double)] -> r) -> Double -> Map String Double -> r
toListWorstAnd f deg mp =
    M.toList mp & L.sortOn snd & takeWhile (\x -> snd x <= deg) & f

reportFile :: FilePath -> Double -> IO (Maybe String)
reportFile fp deg =
    readyFileParsingAnd
        (parseCSVAnd (diffPercentAnd (toListWorstAnd muxRes deg)))
        fp

    where

    muxRes ls
        | length ls == 0 = Nothing
        | otherwise = Just $ unlines $ map (\(a, b) -> a ++ " " ++ show b) ls

main :: IO ()
main = do
  args <- getArgs
  status <- reportFile (head args) (read (args !! 1) :: Double)
  case status of
    Just err -> error err
    Nothing -> putStrLn "All good"
