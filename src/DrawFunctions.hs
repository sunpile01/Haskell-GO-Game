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

-- | Draws the stone at the given coordinate by finding the center of the square and then calculating the radius so that the circle
-- is within the square (Not really a circle more like a rotated square)
drawStone :: SDL.Renderer -> Stone -> Coordinate -> IO ()
drawStone renderer stone (x, y) = do
  let color = case stone of               -- Pattern matching stone to use the correct color
                Black -> V4 0 0 0 255
                White -> V4 255 255 255 255
      -- sclaes the coordinate by the tilesize and then adds half the tilesize so it is in the center
      center = SDL.P (SDL.V2 (fromIntegral x * tileSize + tileSize `div` 2) (fromIntegral y * tileSize + tileSize `div` 2))
      radius = tileSize `div` 2 - 4                  -- -4 so it does not reach all the edges of the square, looks a bit better
  SDL.rendererDrawColor renderer SDL.$= color
  fillCircle renderer center radius             -- Fills the circle with the specified color

-- | This function fills the circle with the specified color. It uses a circle equation to only draw the points that are within
-- the circle. The nested forM_ functions iterate over all the points within a square that is found using the center points of the circle
-- and the radius of the circle. Then for every point within that square it checks if the point is within the circle calculated using the
--  circle equation.
fillCircle :: SDL.Renderer -> SDL.Point SDL.V2 CInt -> CInt -> IO ()
fillCircle renderer (SDL.P (SDL.V2 cx cy)) radius = do
  let circleCheck x y = (x - cx) ^ 2 + (y - cy) ^ 2 <= radius ^ 2
  forM_ [cy - radius .. cy + radius] $ \y ->
    forM_ [cx - radius .. cx + radius] $ \x -> do
      when (circleCheck x y) $ SDL.drawPoint renderer (SDL.P (SDL.V2 x y))