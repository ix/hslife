module Logic.Life where

import Data.Array
import Control.Monad

-- (x, y) is used for all co-ordinates in this program
data Cell = Cell {
  state       :: Bool
, coordinates :: (Int, Int)
} deriving (Read, Show, Eq)

type Simulation = Array (Int, Int) Cell

createSimulation :: Simulation 
createSimulation = listArray ((0, 0), (9, 9)) comprehension
  where comprehension = [Cell False (x, y) | x <- [0..9], y <- [0..9]]

toggle :: Simulation -> Cell -> Simulation 
toggle sim cell = sim // [((y, x), cell { state = not current })]
  where 
    (y, x) = coordinates cell
    current = state (sim ! (y, x))

neighbors :: Simulation -> Cell -> [Cell]
neighbors sim cell = filter state $ map ((!) sim) c'
  where 
    ((min, _), (max, _)) = bounds sim
    (y, x)               = coordinates cell
    -- neighbour co-ordinates, bounded
    c'                   = map (\(y, x) -> (mod y max, mod x max)) 
      [(y - 1, x - 1), (y, x - 1), (y + 1, x - 1),
       (y - 1, x),                 (y + 1, x),
       (y - 1, x + 1), (y, x + 1), (y + 1, x + 1)]

fate :: Simulation -> Simulation -> Cell -> Simulation 
fate src target cell
  | alive && n < 2              = toggle target cell
  | alive && (n == 2 || n == 3) = target
  | alive && n > 3              = toggle target cell
  | not alive && n == 3         = toggle target cell
  | otherwise                   = target 
  where n     = length $ neighbors src cell
        alive = state cell

prettyPrint :: Simulation -> IO ()
prettyPrint sim = putStr $ unlines $ map concat $ map (map stringify) c
  where ((_, _), (width, height)) = bounds sim
        c                         = chunks (width + 1) (elems sim)

stringify :: Cell -> String
stringify (Cell True _)  = "Ｏ"
stringify (Cell False _) = "　"

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n ls = f : chunks n s
  where (f, s) = splitAt n ls

advance :: Simulation -> Simulation
advance sim = foldl (fate sim) sim (elems sim) 

advanceN :: Int -> Simulation -> Simulation
advanceN times sim = foldl (\s n -> advance s) glider [0..times]

glider :: Simulation
glider = foldl (\sim (x, y) -> toggle sim (sim ! (y, x))) sim pairs
  where
    sim = createSimulation
    pairs = [(3, 3), (2, 3), (1, 3), (3, 2), (2, 1)]
