module Day01 (solve) where

import Data.List.Split (splitOn)
import Data.List (sort)

part1 :: [Int] -> [Int] -> Int
part1 (x:xs) (y:ys) = abs (x - y) + part1 xs ys
part1 [] [] = 0

part2 :: [Int] -> [Int] -> Int
part2 (x:xs) ys = 
   let count = length $ filter (x==) ys in
   x * count + part2 xs ys
part2 [] _ = 0

parse :: ([Int], [Int]) -> String -> ([Int], [Int])
parse (xs, ys) line =
    case (words line) of
        [x, y] -> ((read x :: Int) : xs, (read y :: Int) : ys)
        _ -> error $ "unexpected line: " ++ line 

solve :: String -> String
solve input =
    let lines = filter (not . null) $ splitOn "\n" input in
    let (xs, ys)  = foldl parse ([], []) lines in
    let p1 = show $ part1 (sort xs) (sort ys) in
    let p2 = show $ part2 xs ys in
    p1 ++ " " ++ p2
