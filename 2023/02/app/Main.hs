module Main where

import Data.List (isInfixOf, maximum)

maxRedCubes = 12
maxGreenCubes = 13
maxBlueCubes = 14

data Set = Set
  { red   :: Int
  , green :: Int
  , blue  :: Int
  } deriving (Show)

data Game = Game
  { gameN :: Int
  , sets  :: [Set]
  } deriving (Show)

join :: [String] -> [(String, String)]
join [] = []
join (h1:h2:tl) = (h1, h2) : (join tl)

-- ["2", "blue,", "4", "green;"]
parseSet :: [String] -> Set
parseSet entries =
  Set r g b
  where
    joinedEntries = join entries
    r = parseColor "red"
    g = parseColor "green"
    b = parseColor "blue"
    parseColor color = 
      let filteredEntries = filter (\(_, colorS) -> isInfixOf color colorS) joinedEntries
      in if null filteredEntries
         then 0
         else 
          let (n, _) : _ = filteredEntries 
          in read n :: Int

getSets :: [String] -> [Set]
getSets [] = []
getSets ss =
  let (setS, restS) = span (\s -> not $ isInfixOf ";" s) ss
      set = parseSet (setS ++ if null restS then [] else [restS !! 0])
      rest = if null restS then [] else getSets $ tail restS
  in set : rest

getGame :: String -> Game
getGame s =
  let gameL = words s
      (nS, _) = span (\c -> c /= ':') $ gameL !! 1
      sets = getSets $ drop 2 gameL 
  in Game (read nS :: Int) sets

isValidGame game =
  let invalidSets = filter (not . isValidSet) $ sets game
  in null invalidSets

isValidSet (Set r g b) =
  r <= maxRedCubes &&
  g <= maxGreenCubes &&
  b <= maxBlueCubes

getGameIds :: [String] -> [Int]
getGameIds gamesS =
  let games = map getGame gamesS in
  let validGames = filter isValidGame games
  in map gameN validGames

getSumOfGameIds input = 
  let gameIds = getGameIds $ lines input
  in show $ sum gameIds

getPower (Game _ sets) =
   let minR = maximum $ map red sets
       minG = maximum $ map green sets
       minB = maximum $ map blue sets
   in minR * minG * minB

getSumOfPower input =
  let games = map getGame $ lines input
      powers = map getPower games
  in show $ sum powers

main :: IO ()
main = do
  input <- readFile "app/input.txt"
  putStrLn $ getSumOfGameIds input
  putStrLn $ getSumOfPower input
