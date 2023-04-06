module UtilityFunctions (
  isEmptyBoard,
  emptyCoordinate,
  adjacentCoords,
  removeDuplicateGroups,
  removeDuplicateStones,
  removeDuplicateCoords,
  getAllCoordinates,
  isValidCoord
) where

import DataTypes
import Data.Maybe (isNothing)
import Data.List (nubBy, sort)
import Foreign.C.Types (CInt)

-- | Checks if the board only consists of Nothing values
isEmptyBoard :: Board -> Bool
isEmptyBoard board = all isNothing (concat board)

-- | Checks if the given coordinate on the baord is empty (Has the Nothing value)
emptyCoordinate :: Board -> Coordinate -> (Board -> Coordinate -> Maybe Stone) -> Bool
emptyCoordinate board coordinate getStoneFunc = isNothing (getStoneFunc board coordinate)

-- | Gets all the adjacent coordinates of the given coordinate and makes sure they are valid coordinates first
adjacentCoords :: Board -> Coordinate -> [Coordinate]
adjacentCoords board (x,y) = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], abs dx /= abs dy, isValidCoord board (x + dx, y + dy)]

-- | Remove all duplicate nested lists that are of the same type. called it removeDupliateGroups because that is what
-- it is used for. Sorts the coordinates within the group first before checking for dupliactes 
removeDuplicateGroups :: (Ord a) => [[a]] -> [[a]]
removeDuplicateGroups = nubBy equalGroups
  where
    equalGroups g1 g2 = sort g1 == sort g2

-- | Remove all duplicate lists that are of the same type. called it removeDupliateStones because that is what
-- it is used for. 
removeDuplicateStones :: (Eq a) => [a] -> [a]
removeDuplicateStones [] = []
removeDuplicateStones (x:xs) = x : removeDuplicateStones (filter (/= x) xs)

-- | Remove all duplicate lists of coordinates that are of the same type. called it removeDupliateCoordinates because that is what
-- it is used for 
removeDuplicateCoords :: (Eq a) => [(a, a)] -> [(a, a)]
removeDuplicateCoords [] = []
removeDuplicateCoords (x:xs) = x : removeDuplicateCoords (filter (/= x) xs)

-- | Gets all the coordinates on the board
getAllCoordinates :: CInt -> [Coordinate]
getAllCoordinates boardSize = [(x, y) | x <- [0 .. fromIntegral boardSize - 1], y <- [0 .. fromIntegral boardSize - 1]]

-- | Checks if a coordinate is within the bounds of the board    
isValidCoord :: Board -> Coordinate -> Bool
isValidCoord board (x,y) = x >= 0 && x < length board && y >= 0 && y < length board