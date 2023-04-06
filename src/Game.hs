{-# LANGUAGE OverloadedStrings #-}
module Game (
  mainApp
) where

import qualified SDL
import Control.Monad (unless, void, when)
import qualified SDL.Font as Font
import Foreign.C.Types (CInt)
import Text.Read (readMaybe)
import Data.Text(Text)
import DrawFunctions
import DataTypes
import UtilityFunctions
import BoardOperations
import ParseSGFFiles

tileSize :: CInt
tileSize = 30 

-- | Initializes the game and game state. Reads the SGF file and gets the necessary content from it. Then starts the 
-- gameloop with the initial game state acquired from the sgf file. 
mainApp :: IO ()
mainApp = do 
    --userInput <- desiredScoringSystem -- Commented out since i cant get getLine to work properly
    initializeSDL
    let sgfFilePath = "InitialBoard.sgf"  -- Wanted to prompt the user with if they wanted to read the board state from a sgf file first
    sgfFile <- readFile sgfFilePath       -- But i could not get user input to work with getLine, so now it reads from the file as a default
    let moves = parseSGFMoves sgfFile     -- Commented this issue with the error message in the desiredScoringSystem function
        properties = parseSgfProperties sgfFile
        boardSize = applySGFBoardSize properties
        noStones = replicate (fromIntegral boardSize) (replicate (fromIntegral boardSize) Nothing)
        finalBoard = applySGFMoves noStones moves boardSize
        playerTurn = applySGFPlayerTurn properties 
       -- scoringSystem = if userInput == 1 then TerritoryScoring else AreaScoring
        initialBoard = GameState finalBoard [] [] [] AreaScoring playerTurn boardSize
    window <- createTheWindow boardSize
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    gameLoop renderer initialBoard 0 window
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit
    
    
-- | Main game loop that checks for events like mouseclicks and keypresses. Then handles these events by calling
-- functions that uses the respective events. Then it draws the application itself with the board. Calls itself recursively
-- until the user presses 'Q' or the X in the top right cornor of the application
gameLoop :: SDL.Renderer -> GameState -> Int -> SDL.Window -> IO ()
gameLoop renderer gameState count window = do
  events <- SDL.pollEvents                                      -- Get events
  let xTopRightPressed = SDL.QuitEvent `elem` map SDL.eventPayload events   -- Quit the application by clicking X top right
      qPressed = any isPressedQ events                          -- any checks for qPressed on the list of events and returns true or false
      newGameState = foldl handleInput gameState events         -- applies handleInput to the gameState for every event in the events list, thereby updating the gamestate 
      undoMove = any isPressedZ events                          -- Checks if Z is pressed
      playerPassed = any isPressedS events                  -- Checks if space is pressed
      (finalBoard, count') = decideBoard
        where                                 
          decideBoard       -- Locally defined function that uses guards to update the board state based on the users input              
            | playerPassed = (currBoard newGameState, count + 1)    -- A player passed, board stays the same and pass count increased
            -- Z was pressed and the move done earlier is undone
            | undoMove && not (isEmptyBoard (prevBoard newGameState)) = (prevBoard newGameState, if count /= 0 then count-1 else count)
            | currBoard newGameState /= currBoard gameState = (currBoard newGameState, if count > 0 then count - 1 else count) -- A move was made, decrease count
            | otherwise = (currBoard newGameState, count)           -- the updated board is the final board
  when (count' == 2) (void $ decideChampion newGameState)           -- Both players have passed and the winner is decided
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 0 0 255          -- Background color
  SDL.clear renderer
  drawBoard renderer finalBoard (sizeBoard newGameState) -- Draws the game board
  drawInterface renderer gameState window
  SDL.present renderer
  -- Keep running the game if user has not pressed q or x or the winner has not been decided yet
  unless (xTopRightPressed || qPressed || count' ==2 ) (gameLoop renderer (newGameState {currBoard = finalBoard}) count' window) 
  
-- | Simply checks if the Q key was pressed
isPressedQ :: SDL.Event -> Bool
isPressedQ event =
  case SDL.eventPayload event of
      SDL.KeyboardEvent keyboardEvent ->
        SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
        SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
      _ -> False
      
-- | Simply checks if the Z key was pressed
isPressedZ :: SDL.Event -> Bool
isPressedZ event =
  case SDL.eventPayload event of
      SDL.KeyboardEvent keyboardEvent ->
        SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
        SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeZ
      _ -> False    

-- | Simply checks if the Space key was pressed
isPressedS :: SDL.Event -> Bool
isPressedS event =
  case SDL.eventPayload event of
      SDL.KeyboardEvent keyboardEvent ->
        SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
        SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeS
      _ -> False  

-- | Creates the window for the application, and makes it resizable. Takes the boardSize as a parameter
createTheWindow :: CInt -> IO SDL.Window
createTheWindow boardSize= do
  window <- SDL.createWindow "GO-Game" SDL.defaultWindow {
    SDL.windowInitialSize = SDL.V2 (boardSize * tileSize * 2) (boardSize * tileSize * 2),
    SDL.windowResizable = True
  }
  SDL.showWindow window
  return window

-- | Initializes the SDL2 library
initializeSDL :: IO ()
initializeSDL = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality SDL.$= SDL.ScaleLinear
  Font.initialize

-- | Prompts the user for what scoring method they wish to use for their game
-- For some reason the program crashes everytime i try to use getLine and gives me this error:
-- <stdin>: hGetLine: invalid argument (Bad file descriptor)
-- So this funcitonality is not used actually implemented right now
desiredScoringSystem :: IO Integer
desiredScoringSystem = do
  putStrLn "Choose a scoring method:"
  putStrLn "Enter '1' for Territory Scoring"
  putStrLn "Enter '2' for Area Scoring"
  input <- getLine
  case readMaybe input :: Maybe Int of
    Just 1 -> return 1
    Just 2 -> return 2
    Just _ -> do
      putStrLn "Invalid input, please enter either 1 or 2"
      desiredScoringSystem
    Nothing -> do
      putStrLn "Invalid input, please enter either 1 or 2"
      desiredScoringSystem
