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

-- | Renders the text sent as a parameter with the given font to the screen
drawText :: SDL.Renderer -> SDL.Font.Font -> Text -> (Int, Int) -> IO ()
drawText renderer font text (x, y) = do
    textSurface <- SDL.Font.solid font (SDL.V4 255 255 255 0) text    -- Creates a text surface with given font text and color
    surface <- SDL.createTextureFromSurface renderer textSurface      -- creates a texture from the texutre surface
    info <- SDL.queryTexture surface                                  -- gets the width and heigth of the texture
    let w = SDL.textureWidth info
    let h = SDL.textureHeight info
    SDL.copy renderer surface Nothing (Just $ SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral x) (fromIntegral y))) (SDL.V2 w h)) --renders the texture
    SDL.freeSurface textSurface
    SDL.destroyTexture surface

-- | Draws text to help the user know what commands they have available. It also draws text that provides the user with
-- some information about the game state.
drawInterface :: SDL.Renderer -> GameState -> SDL.Window -> IO ()
drawInterface r gameState window = do
  (SDL.V2 windowWidthIntegral windowHeigthIntegral) <- SDL.glGetDrawableSize window -- Window height and width
  let windowWidth = fromIntegral windowWidthIntegral :: Int
      windowHeigth = fromIntegral windowHeigthIntegral :: Int
      fontSize = (windowWidth * 3) `div` 100                                        -- Font size changes based on window size
      fontSize2 = (windowWidth * 2) `div` 100
      p1 = windowWidth - 250
  font <- SDL.Font.load "./ttf/roboto/Roboto-Bold.ttf" fontSize

  -- The keyboard shortcuts the user have available for doing different actions:
  drawText r font "Q - Quit" (p1,50)
  drawText r font "E - Eliminates stones" (p1, 100)
  drawText r font "L - extends liberties" (p1, 150)
  drawText r font "C - connects stones" (p1, 200)
  drawText r font "S - skip turn" (p1, 250)
  drawText r font "Z - undo last move" (p1, 300)

  font2 <- SDL.Font.load  "./ttf/roboto/Roboto-Bold.ttf" fontSize2

  -- Shows some basic informaiton about the current state of the game:
  drawText r font2 "Current player to place a stone is: " (p1 - 400, 500)
  drawText r font2 (pack $ show $ if playerTurn gameState == Just Black then Black else White) (p1 -150 , 500)

  drawText r font2 "White Groups, territories and captured stones:" (50 , windowHeigth - 200)
  drawText r font2 (pack $ show $ countGroups (currBoard gameState) (Just White) (sizeBoard gameState)) (100,windowHeigth - 150)
  drawText r font2 (pack $ show $ countTerritory (currBoard gameState) (Just White) (sizeBoard gameState)) (100,windowHeigth - 100)
  drawText r font2 (pack $ show $ length $ filter (== Just Black) (capturedStones gameState)) (100,windowHeigth - 50)

  drawText r font2 "Black Groups, territories and captured stones:" (400,windowHeigth - 200)
  drawText r font2 (pack $ show $ countGroups (currBoard gameState) (Just Black) (sizeBoard gameState)) (450,windowHeigth - 150)
  drawText r font2 (pack $ show $ countTerritory (currBoard gameState) (Just Black) (sizeBoard gameState)) (450,windowHeigth - 100)
  drawText r font2 (pack $ show $ length $ filter (== Just White) (capturedStones gameState)) (450,windowHeigth - 50)
