module Day03 (solve) where

import Control.Arrow ((>>>), (&&&))
import Text.Regex.TDFA

data Inst = Value Int | Do | Dont

part1 :: [Inst] -> Int
part1 = sum . map accum
  where
    accum (Value x) = x
    accum _ = 0

part2 :: [Inst] -> Int
part2 = foldl accum (True, 0) >>> snd
  where
    accum (True, acc) (Value x) = (True, acc + x)
    accum (False, acc) (Value _) = (False, acc)
    accum (_, acc) Do = (True, acc)
    accum (_, acc) Dont = (False, acc)

parse :: String -> [Inst]
parse input = map convert (input =~ pattern :: [[String]])
  where
    pattern = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)|do\\(\\)|don't\\(\\)"
    convert ["do()", "", ""] = Do
    convert ["don't()", "", ""] = Dont
    convert [_, x, y] = Value $ (read x) * (read y)
    convert invalid = error $ show invalid

solve :: String -> (Int, Int)
solve = parse >>> (part1 &&& part2)
