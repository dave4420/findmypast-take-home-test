{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Atkin

import qualified Data.Char          as Ch
import qualified Data.Map           as M
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

buildSquare :: [Int] -> [[Int]]  -- generates rows of columns
buildSquare = undefined  --DAVE

type Formatter = [[Int]] -> Tx.L.Text

formats :: M.Map String Formatter
formats = M.fromList
    [ ("text", formatText)
    , ("csv", formatCSV)
    ]

formatText :: Formatter
formatText = undefined  --DAVE

formatCSV :: Formatter
formatCSV = undefined  --DAVE
