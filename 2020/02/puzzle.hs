module Main where

import Data.List.Split
import Flow

nValid :: (Int -> Int -> Char -> String -> Bool) -> [String] -> Int
nValid p l =
  map check l |> filter id |> length
  where 
    check s =
      let [range,charsemi,password] = splitOn " " s in
      let [lows,highs] = splitOn "-" range in
      let [char]:_ = splitOn ":" charsemi in
      let low = read lows :: Int in
      let high = read highs :: Int in
      p low high char password

part1 :: [String] -> Int
part1 = nValid p
  where
    p n1 n2 c s =
      let count = filter (== c) s |> length in
      count >= n1 && count <= n2

part2 :: [String] -> Int
part2 = nValid p
  where
    p n1 n2 c s =
      let count = zip s [1..] |> filter (\(ch, i) -> (i == n1 || i == n2) && c == ch) |> length in
      count == 1

main :: IO ()
main = do
  l <- lines <$> readFile "input"
  part1 l |> print
  part2 l |> print