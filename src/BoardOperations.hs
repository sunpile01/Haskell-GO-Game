module BoardOperations (
  handleInput,
  countTerritory,
  countStones,
  countGroups,
  decideChampion,
  placeStone,
  captureStones,
  bestMoveForCapture,
  bestMoveForConnectGroups,
  isEmptyPosition,
  findGroup,
  getStoneYX,
  groupHasLiberties,
  allUniqueGroupsOfColor,
  updateStones,
  getStoneXY,
  validMove,
  adjacentStones,
  
) where 
  
import DataTypes
import qualified SDL
import Data.Maybe (fromJust, isJust, isNothing)
import UtilityFunctions
import Foreign.C.Types (CInt)
import Data.List ((\\), maximumBy)
import Data.Ord (comparing)

tileSize :: CInt
tileSize = 30 

-- | Handles the input for the game. The input is either the user clicking the mouse or pressing a key
handleInput :: GameState -> SDL.Event -> GameState
handleInput gameState event@(SDL.Event _ payload) =
  case payload of
    SDL.MouseButtonEvent _ -> handleMouseClick gameState event
    SDL.KeyboardEvent _ -> handleKeyPress gameState event
    _ -> gameState
    
-- | Processes the mosuclick of the user and places a stone of the correct color on the intersection the user clicked.
-- The fucntion also updates the game state properties that needs to be updated after every move
handleMouseClick :: GameState -> SDL.Event -> GameState
handleMouseClick gameState (SDL.Event _ (SDL.MouseButtonEvent mouseButtonEventData)) =
  let SDL.P (SDL.V2 x y) = SDL.mouseButtonEventPos mouseButtonEventData   -- Gets the x and y position for the user click 
      x' = (fromIntegral x `div` fromIntegral tileSize)                   -- Gets x index for the square the user clicked.
      y' = (fromIntegral y `div` fromIntegral tileSize)                   -- Same, but for y.
      stone = if playerTurn gameState == Just Black then Just Black else Just White   -- The current players stone color
      nextTurnStone = if stone == Just Black then Just White else Just Black          -- Stone color for next turn
      move = Move stone (x', y')                                                      -- Move to be placed 
      previousBoard = currBoard gameState 
      twoTurnsAgoBoard = if isEmptyBoard (prevBoard gameState) then [] else prevBoard gameState --Board two turns ago
  in if isValidCoord (currBoard gameState) (x',y') && validMove (currBoard gameState) twoTurnsAgoBoard stone (x', y')
      -- If the move was valid then we update the gameState with the new move and the updated properties defined earlier
     then let updatedBoard = GameState (placeStone move (currBoard gameState)) previousBoard twoTurnsAgoBoard (capturedStones gameState) (scoringMethod gameState) nextTurnStone (sizeBoard gameState)
              boardWCaptures = captureStones updatedBoard stone               -- Capture any stones that had no liberties after the move 
           in boardWCaptures                                                  -- Final updated game state
     else gameState                                                           -- Move not valid return the old gameState
handleMouseClick gameState _ = gameState

-- | Updates the specified position on the board with the given stone. Haskell lists are immutable,
-- so it is necessary to create a new list as the return value with the updated row
placeStone :: Move -> Board -> Board
placeStone (Move stone (x,y)) oldBoard =
  let row = oldBoard !! y                 -- Get the row to be updated
      newRow = take x row ++ [stone] ++ drop (x+1) row -- New row with the updated value for the coordinate
      newBoard = take y oldBoard ++ [newRow] ++ drop (y +1) oldBoard  -- Replacing the old row with the new row for the updateBoard
  in newBoard

-- | Uses recursion and a list of already visited coordinates to find all stones in the group.
-- A group is same colored stones that is vertically or horizontally adjacent.
findGroup :: Board -> Maybe Stone -> Coordinate -> [Coordinate] -> (Board -> Coordinate -> Maybe Stone) -> [Coordinate]
findGroup board stone coord@(x, y) visited getStoneFunc
  | getStoneFunc board coord /= stone = []    -- Stone at the the current coordinate is not the same as the group
  | coord `elem` visited = []                 -- The current coordinate has already been visited
  | otherwise =                               -- Coordinate has not been visited already
      let newVisited = coord : visited        -- add it to the visited list
          neighbors = adjacentCoords board (x,y)    -- Get the neighbor coordinates for the current coordinate
          unvisitedNeighbors = filter (`notElem` newVisited) neighbors    -- Filter out the already visited ones
          sameColorNeighbors = filter (\c -> getStoneFunc board c == stone) unvisitedNeighbors  -- Filter out those of opposite color
         -- concatenate the coordinate to the list and then recursively call this function for the same colored neighbors with updated visited list
      in coord : concatMap (\c -> findGroup board stone c newVisited getStoneFunc) sameColorNeighbors

-- | Wrapper function for the findGroup function that adds the initial empty list of visited stones
findGroupWrapper :: Board -> Maybe Stone -> Coordinate -> (Board -> Coordinate -> Maybe Stone) -> [Coordinate]
findGroupWrapper board stone coord getStoneFunc = findGroup board stone coord [] getStoneFunc

-- | Checks if a group has liberties, checks if each point adjacent to the group is empty, if 'any' of them are then
--  the group has liberties
groupHasLiberties :: Board -> [Coordinate] -> (Board -> Coordinate -> Maybe Stone) -> Bool
-- concatMap generates all the adjacent coordinates to the group, then the lambda function checks all those adjacent coordinates generated.
groupHasLiberties board group getStoneFunc = any (\coord -> emptyCoordinate board coord getStoneFunc) (concatMap (adjacentCoords board) group)

-- | Function that checks if any stones or groups of stones on the board has 0 liberties, if they have 0 liberties the stone is captured
-- and the stone value is replaced by 'Nothing' the color of the stone is added to the captured coordinates
captureStones :: GameState -> Maybe Stone -> GameState
captureStones board stone = board { currBoard = newStones, capturedStones = newCapturedStones } -- Updating the game state after capture
  where
    opponent = if stone == Just Black then Just White else Just Black   -- Opponent stone color
    uniqueOpponentGroups = allUniqueGroupsOfColor opponent (currBoard board) (sizeBoard board)  -- All the unique opponent groups (can be a single stone)
    -- Gets all the groups that has no liberties, this is a list of [[Coordinate]] because it is a list og groups
    capturedGroups = filter (\group -> not (groupHasLiberties (currBoard board) group getStoneXY)) uniqueOpponentGroups
    stonesCaptured = concat capturedGroups   -- Here we flatten the [[Coordinate]] list into a list of [Coordinate]
    newStones = foldl (`updateStones` Nothing) (currBoard board) stonesCaptured -- Updates the board with 'Nothing' for the capturedStones
     -- Adds the captured stones to the list of already captured stones as Stone color instead of the Coordinate
    newCapturedStones = capturedStones board ++ map (\_ -> opponent) stonesCaptured

-- | Gets all the unique groups of a single color on the board
allUniqueGroupsOfColor :: Maybe Stone -> Board -> CInt -> [[Coordinate]]
allUniqueGroupsOfColor stone board boardSize = findGroupsForStones stone board colorStones
  where
    allCoordinates = getAllCoordinates    -- All coordinates on the board
    colorStones = filter (\c -> getStoneXY board c == stone) (allCoordinates boardSize) -- Only coordinates with the given stone color

findGroupsForStones :: Maybe Stone -> Board -> [Coordinate] -> [[Coordinate]]
findGroupsForStones _ _ []  = []
findGroupsForStones stone board (coord:coords) =    -- recursive case when input list has atleast one coordinate
 let group = findGroupWrapper board stone coord getStoneXY     -- calls findGoupWrapper to find the group of adjacent stones of the same color
     remainingStones = coords \\ group                         -- Removes the stones in the current group based on the list of coords
 in group : findGroupsForStones stone board remainingStones    -- group is concatenated to the result,
                                                               -- processes the remaining stones recursively