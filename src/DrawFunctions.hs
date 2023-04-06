{-# LANGUAGE OverloadedStrings #-}
module DrawFunctions (
  drawBoard,
  drawInterface
) where
  
import qualified SDL
import Linear (V4(..))
import Foreign.C.Types (CInt)
import Data.Text (Text, pack)
import SDL.Font
import DataTypes
import BoardOperations(countGroups, countTerritory)
import Control.Monad (forM_, when)
tileSize :: CInt
tileSize = 30 

-- | This function draws the game board, it draws lines to create the grid for the player to play their moves 
-- and calls the function that draws the stones within the squares on the grid that has a stone  
drawBoard :: SDL.Renderer -> Board -> CInt -> IO ()
drawBoard renderer board boardSize = do
  SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 255
  let boardWidth = tileSize * boardSize
      boardHeight = tileSize * boardSize
  mapM_ (\i -> do           -- Draws the lines for the grid based using a lambda function where 'i' is [1..boardSize]
            SDL.drawLine renderer (SDL.P (SDL.V2 (i * tileSize) 0)) (SDL.P (SDL.V2 (i * tileSize) boardHeight))
            SDL.drawLine renderer (SDL.P (SDL.V2 0 (i * tileSize))) (SDL.P (SDL.V2 boardWidth (i * tileSize)))) [1..boardSize]
  -- Goes through the board coordinates and draws a stone of the correct color if one is found at a coordinate using the drawstone function
  mapM_ (\(x,y) -> case board !! y !! x of                                -- Pattern matches to draw the correct stone color
                      Just Black -> drawStone renderer Black (x, y)       -- Draws the black stone at the coordinate
                      Just White -> drawStone renderer White (x, y)       -- draws the white stone at the coordinate
                      Nothing -> return ()) [(fromIntegral x, fromIntegral y) | x <- [0..boardSize-1], y <- [0..boardSize-1]]
