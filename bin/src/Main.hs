{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Atkin

import qualified Data.Char          as Ch
import qualified Data.Map           as M
import           Data.Monoid        ((<>))
import qualified Data.Text.Lazy     as Tx.L
import qualified Data.Text.Lazy.IO  as Tx.L.IO
import           System.Environment (getArgs)
import qualified System.IO          as IO
import           Text.Read          (readMaybe)

main :: IO ()
main = do
    getArgs >>= \case
        [formatStr, nStr]
            | Just format <- M.lookup (Ch.toLower <$> formatStr) formats
            , Just n <- readMaybe nStr
            , 0 < n
                -> Tx.L.IO.hPutStr IO.stdout
                    ((format . buildSquare) (take n Atkin.primes))

        []      -> Tx.L.IO.hPutStrLn IO.stdout syntax

        _       -> Tx.L.IO.hPutStrLn IO.stderr syntax

syntax :: Tx.L.Text
syntax = Tx.L.unlines
    [ "syntax: findmypast-take-home-test <format> <n>"
    , ""
    , "<format> is \"CSV\" or \"text\""
    , "<n> is the number of primes to generate the table with"
    ]

type Table = [[Maybe Int]]  -- rows of columns

buildSquare :: [Int] -> Table
buildSquare primes
    = (Nothing : map Just primes) : map buildRow primes
  where
    buildRow rowPrime = Just rowPrime : map (Just . (* rowPrime)) primes

type Formatter = Table -> Tx.L.Text

formats :: M.Map String Formatter
formats = M.fromList
    [ ("text", formatText)
    , ("csv", formatCSV)
    ]

formatText :: Formatter
formatText = Tx.L.unlines . map Tx.L.unwords . pad . formatCells

pad :: [[Tx.L.Text]] -> [[Tx.L.Text]]
pad table = (map . map) pad' table
  where
    pad' cell
        = Tx.L.replicate (longestCellLength - Tx.L.length cell) " " <> cell
    longestCellLength = (Tx.L.length . last . last) table

-- hacky implementation: we know nothing needs quoting or escaping
formatCSV :: Formatter
formatCSV = Tx.L.unlines . map (Tx.L.intercalate ",") . formatCells

formatCells :: [[Maybe Int]] -> [[Tx.L.Text]]
formatCells = (map . map) (maybe "" (Tx.L.pack . show))
