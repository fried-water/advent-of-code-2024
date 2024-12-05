module Main where

import qualified Day01
import qualified Day02
import qualified Day03
import System.Environment (getArgs)

run :: String -> FilePath -> IO ()
run day inputFile = do
  input <- readFile inputFile
  case day of
    "1" -> putStrLn $ show $ Day01.solve input
    "2" -> putStrLn $ show $ Day02.solve input
    "3" -> putStrLn $ show $ Day03.solve input
    _ -> putStrLn "invalid day"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [day, input] -> run day input
    _ -> putStrLn "usage: aoc2024 <day> <input-file>"
