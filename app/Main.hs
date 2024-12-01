module Main where

import System.Environment (getArgs)
import qualified Day01

run :: String -> FilePath -> IO ()
run day inputFile = do
   input <- readFile inputFile
   case day of
        "1" -> putStrLn $ Day01.solve input 
        _ -> putStrLn "invalid day"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [day, input] -> run day input
        _ -> putStrLn "usage: aoc2024 <day> <input-file>"

