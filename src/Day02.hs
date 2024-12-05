module Day02 (solve) where

import Control.Arrow ((>>>), (&&&))

type Input = [[Int]]

safe :: [Int] -> Bool
safe xs =
  all ((< 4) . abs . uncurry (-)) adj
    && (all (uncurry (<)) adj || all (uncurry (>)) adj)
  where
    adj = zip xs (tail xs)

removeIdx :: [Int] -> Int -> [Int]
removeIdx xs x = map (\(_, x) -> x) $ filter (\(idx, _) -> idx /= x) $ zip [1 ..] xs

part1 :: Input -> Int
part1 = length . filter safe

part2 :: Input -> Int
part2 = length . filter (\x -> safe x || (any safe $ map (removeIdx x) [1 .. length x]))

parse :: String -> Input
parse = lines >>> map (words >>> map read)

solve :: String -> (Int, Int)
solve = parse >>> (part1 &&& part2)
