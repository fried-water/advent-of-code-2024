module Day01 (solve) where

import Control.Arrow ((>>>))
import Data.List (sort)

type Input = ([Int], [Int])

part1 :: Input -> Int
part1 (xs, ys) = sum $ map (\(x, y) -> abs (x - y)) $ zip (sort xs) (sort ys)

part2 :: Input -> Int
part2 (xs, ys) = sum $ map (\x -> x * (length $ filter (x ==) ys)) xs

parse :: String -> Input
parse = lines >>> map (line . words) >>> unzip
  where
    line [x, y] = (read x, read y)
    line invalid = error $ show invalid

solve :: String -> (Int, Int)
solve = parse >>> (\x -> (part1 x, part2 x))
