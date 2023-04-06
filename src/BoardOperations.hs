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