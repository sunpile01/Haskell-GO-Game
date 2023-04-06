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

-- | Handles the different keypresses for the different computer generated moves available and then calls the 'updateGameComputerMove' function
-- with the correct 'ComputerMove' type
handleKeyPress :: GameState -> SDL.Event -> GameState
handleKeyPress gameState (SDL.Event _ (SDL.KeyboardEvent keyboardEvent))
  | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeE =
    updateGameComputerMove gameState MostEliminated
  | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeL =
    updateGameComputerMove gameState ExtendLiberties
  | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeC =
      updateGameComputerMove gameState ConnectGroups
handleKeyPress gameState _ = gameState

-- | Places the computer move on the board by pattern matching the type of ComputerMove and then executing the appropriate function
-- Then updates the game state with the new board created by the computer move and all the properties of the game state that need to
-- updated after every move
updateGameComputerMove :: GameState -> ComputerMove -> GameState
updateGameComputerMove gameState computerMove =
  let stone = if playerTurn gameState == Just Black then Just Black else Just White -- Gets the stone color of the player that to place the move for
      nextTurnStone = if stone == Just Black then Just White else Just Black        -- Stone to be placed the next turn
      previousBoard = currBoard gameState
      twoTurnsAgoBoard = if isEmptyBoard (prevBoard gameState) then [] else prevBoard gameState -- Get the board two turns in the past
      -- Pattern matches the ComputerMove type and executes the appropriate function depending on the type
      bestMoveCoord = case computerMove of
        MostEliminated -> bestMoveForCapture gameState              -- Computer move to capture most opponent stones
        ExtendLiberties -> bestMoveForLiberties gameState           -- Computer move that increases the liberties of a group the most
        ConnectGroups -> bestMoveForConnectGroups gameState         -- Computer move that connects the most amount of equal colored groups
  in if isJust bestMoveCoord && validMove (currBoard gameState) twoTurnsAgoBoard stone (fromJust bestMoveCoord) &&
                                                            isValidCoord (currBoard gameState) (fromJust bestMoveCoord)
     then let move = Move stone (fromJust bestMoveCoord)          -- If the move is valid as checked above then create the move and
              -- Update the gameState with all the variables we defined earlier that is needed in GameState and place the latest move
              updatedBoard = GameState (placeStone move (currBoard gameState)) previousBoard twoTurnsAgoBoard (capturedStones gameState) (scoringMethod gameState) nextTurnStone (sizeBoard gameState)
              boardWCaptures = captureStones updatedBoard stone  -- Updates gameState by removing captured stones and adds newly captured stones
          in boardWCaptures                                      -- Final gameState that the function returns
     else gameState                                              -- If move was not valid return the old gameState

-- | The coordinate of the move that will capture the most amount of enemy stones
-- One note here is that all the bestMoves function were almost identical and only used a different function for generating the
-- tuple with coordinates and then the integer value for that coordinate (for example amount of captured stones or amount of libeties)
-- Therefore i created the bestMove function that just uses different mapper functions
bestMoveForCapture :: GameState -> Maybe Coordinate
bestMoveForCapture gameState = findBestMove gameState mapper  -- The mapper function is sent to the bestMove function
  where
    currentPlayer = playerTurn gameState                  -- Stone color of the current player
    -- mapper is a function that takes a coord as parameter, and returns the amount of captured stones by calling the captureStones with a simulated move
    mapper coord = length (capturedStones (captureStones (gameState { currBoard = placeStone (Move currentPlayer coord) (currBoard gameState) }) currentPlayer))

-- | The coordinate of the move that will extend the liberties of a group of the given color the most
-- Same concept as bestMoveForCapture but this function utilizes the groupLibertiesFromCoord as the mapper function
bestMoveForLiberties :: GameState -> Maybe Coordinate
bestMoveForLiberties gameState = findBestMove gameState mapper
  where
    currentPlayer = playerTurn gameState
    mapper coord = groupLibertiesFromCoord (placeStone (Move currentPlayer coord) (currBoard gameState)) currentPlayer coord

-- | The coordinate of the move that will connect the most amount of groups for the given color.
-- Would be cool if it took into account the amount of liberties the connected groups would make, but I have not had time to implement this yet.
-- Same concept as bestMoveForCapture but this function utilizes the numGroupsConnectedByMove as the mapper function
bestMoveForConnectGroups :: GameState -> Maybe Coordinate
bestMoveForConnectGroups gameState = findBestMove gameState mapper
  where
    currentPlayer = playerTurn gameState
    mapper coord = numGroupsConnectedByMove (currBoard gameState) currentPlayer coord

-- | Find the amount of liberties the group with the given coordinate has
groupLibertiesFromCoord :: Board -> Maybe Stone -> Coordinate -> Int
groupLibertiesFromCoord board stone coord =
  let group = findGroupWrapper board stone coord getStoneYX -- Finds the group of the coordinate
      liberties = countGroupLiberties board group           -- Get the number of liberties of the group
  in liberties

-- | Checks if placing a stone at the given coordinate would connect two or more groups
numGroupsConnectedByMove :: Board -> Maybe Stone -> Coordinate -> Int
numGroupsConnectedByMove board stone coord =
  let adjacentCoordinates = filter (\c -> getStoneYX board c == stone) (adjacentCoords board coord) -- Only adjacent coordinates of the same color
      adjacentGroups = map (\c -> findGroupWrapper board stone c getStoneYX) adjacentCoordinates    -- Finds the adjacent groups
      uniqueAdjacentGroups = removeDuplicateGroups adjacentGroups                                   -- Removes potential duplicate groups
      connectedGroups = if length uniqueAdjacentGroups >= 2 then length uniqueAdjacentGroups else 0 -- If number of groups is greater than 2
  in connectedGroups



-- | Finds the coordinate that evaluates to the best move by using the mapper function sent as a paramteter
findBestMove :: GameState -> (Coordinate -> Int) -> Maybe Coordinate
findBestMove gameState mapper = do
  let currentPlayer = playerTurn gameState
      allCoordinates = getAllCoordinates      -- Get all coordinates since we will perform an exhaustive search
      -- Remove all none valid coordinates
      validCoordinates = filter (validMove (currBoard gameState) (prevBoard gameState) currentPlayer) (allCoordinates (sizeBoard gameState))
      -- Creates tuples with the potential coordinate and the integer which is the 'value' of that move (higher = better), mapper is a funtion sent as a parameter
      moves = map (\coord -> (coord, mapper coord)) validCoordinates
  if null moves then Nothing else Just (fst (maximumBy (comparing snd) moves)) -- Finds the coordinate with the highest corresponding integer value and returns it


-- | Updates the specified position on the board with the given stone. Haskell lists are immutable,
-- so it is necessary to create a new list as the return value with the updated row
placeStone :: Move -> Board -> Board
placeStone (Move stone (x,y)) oldBoard =
  let row = oldBoard !! y                 -- Get the row to be updated
      newRow = take x row ++ [stone] ++ drop (x+1) row -- New row with the updated value for the coordinate
      newBoard = take y oldBoard ++ [newRow] ++ drop (y +1) oldBoard  -- Replacing the old row with the new row for the updateBoard
  in newBoard

-- | Checks if a given move is valid. Calls helper functions to check the specific cases to satisfy a valid move.
-- Every condition must be satisfied for the move to be valid.
validMove :: Board -> Board -> Maybe Stone -> Coordinate -> Bool
validMove currBoard twoTurnsAgoBoard stone coord =
     isEmptyPosition currBoard coord &&                        -- Checks if there is a stone on the coordinate for the stone to be placed
     not (isKOMove currBoard twoTurnsAgoBoard coord stone) &&  -- Checks if the move would create KO (repeating board state two turns ago)
     not (isSuicideMove currBoard stone coord)                 -- Checks if the move would result in the stone having no liberties and not captureing a stone

-- | Checks if there is a stone on the coordinate for the stone to be placed
isEmptyPosition :: Board -> Coordinate -> Bool
isEmptyPosition board coord = isNothing (getStoneYX board coord)

-- | Checks if the move would result in the stone having no liberties and not capturing an opponent stone
isSuicideMove :: Board -> Maybe Stone -> Coordinate -> Bool
isSuicideMove board stone coord =
  let tempBoard = placeStone (Move stone coord) board           -- Simulate the placement of the stone
      -- If Placement connects the stone to one or more equal color stones 'group' is all those coordinates, if not group is just the same coordinate
      group = findGroupWrapper tempBoard stone coord getStoneYX
      -- If the group (can be one stone) has no liberties and the move does not result in any captures then it is a suicide move
  in not (groupHasLiberties tempBoard group getStoneYX) && null (capturedStones (captureStones (GameState tempBoard board [] [] TerritoryScoring Nothing 0) stone))

-- | Checks if the move would create KO (repeating board state two turns ago)
isKOMove :: Board -> Board -> Coordinate -> Maybe Stone -> Bool
isKOMove currentBoard twoTurnsAgoBoard coord stone =
  let tempBoard = placeStone (Move stone coord) currentBoard  -- Simulate placing the stone on the board
      tempBoardCaptures = captureStones (GameState tempBoard [] [] [] TerritoryScoring Nothing 0) stone -- Simulate stones potentially being captured
  in currBoard tempBoardCaptures == twoTurnsAgoBoard        -- If the simulated stone placing results in the same board state two turns ago it is a KO move

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

-- | Updates the coordinate with a new stone value
updateStones :: Board -> Maybe Stone -> Coordinate -> [[Maybe Stone]]
updateStones board stone (x, y) =
  let (beforeRow, row:afterRow) = splitAt x board -- Splits the 2D list into two three parts, one before x one at x and one after x
      (beforeCol, _:afterCol) = splitAt y row     -- This splits the row part from above into 3 parts one before y one at y and one after y
      newRow = beforeCol ++ [stone] ++ afterCol   -- creates the new row by adding the beforeCol from above the new stone value and the afterCol from above
  in beforeRow ++ [newRow] ++ afterRow            -- The updated 2D list with the newly created row containing the new stone value

-- | Used this for everything that had to do with getting stones from a coordinate
getStoneYX :: [[Maybe Stone]] -> Coordinate -> Maybe Stone
getStoneYX board (x,y) = board !! y !! x

-- | Needed this for all that worked with capturing stones
getStoneXY :: [[Maybe Stone]] -> Coordinate -> Maybe Stone
getStoneXY board (x,y) = board !! x !! y

-- | Counts all the coordinates that is a territory of a certain player and returns the integer number
countTerritory :: Board -> Maybe Stone -> CInt-> Int
countTerritory board stone boardSize = length $ filter (isTerritory board stone) (allCoordinates boardSize) -- Checks if each coordinate is a territory of
  where                                                                                                     -- the given player
    allCoordinates = getAllCoordinates

-- | It is a territory of a certain player if all the empty coordinates in that area is surrounded by stones of a specific
-- color. Explained in other words:
-- An empty point (or coordinate) is considered part of a player's territory if:
-- It is not occupied by a stone (it is Nothing).
-- All the adjacent points of the empty point are either:
-- a. Occupied by the same player's stones (either Black or White), or
-- b. Part of the same territory (they are also empty and meet the same conditions).
isTerritory :: Board -> Maybe Stone -> Coordinate -> Bool
isTerritory board stone coord
  | isJust (getStoneYX board coord) = False             -- If it is a stone value then it is not a territory of a player
  | otherwise = all (== stone) borderingStones     -- if all of the unique adjacent stone coordinates to the group of Nothing coordinates is a certain color
  where
    emptyGroup = findGroupWrapper board Nothing coord getStoneYX      -- Gets the group of empty coordinates
    -- Checks all the unique adjacent stone coordinates to the group of Nothing coordinates
    borderingStones = removeDuplicateStones $ concatMap (adjacentStones board) emptyGroup

-- | Get the stones adjacent to a coordinate. Could also have used adjacentCoords in the isTerritory,
-- but felt like it was easier to read when i created this adjacentStones function instead.
adjacentStones :: Board -> Coordinate -> [Maybe Stone]
adjacentStones board coord =
  filter isJust (map (getStoneYX board) (adjacentCoords board coord))


-- | Count the number of stones on the board for each player
countStones :: Board -> Maybe Stone -> Int
countStones board stone = length $ filter (== stone) (concat board)  -- Filter all coordinates that are not equal to the given stone value

-- | Counts the amount of unique groups of a given color on the board.
countGroups :: Board -> Maybe Stone -> CInt -> Int
countGroups board stone boardSize=
  let uniqueGroups = allUniqueGroupsOfColor stone board boardSize     -- Get all the unique groups of that color
      validGroups = filter (\group -> length group >= 2) uniqueGroups -- Makes sure there are atleast two stones in the group
  in length validGroups                                               -- Length of the list of validgroups

-- | Decides the winner of the game by calculating the scores of each player based on the chosen scoring method for the game.
-- Then prints out the scoring mehtod used, the winner and the respective scores
decideChampion :: GameState -> IO ()
decideChampion gameState = do
  let (blackScore, whiteScore) = case scoringMethod gameState of              -- Pattern matching against the scoringMethod data type
        TerritoryScoring -> (blackTerritoryScore, whiteTerritoryScore + 6.5)  -- + 6.5 is the komi value of starting last
        AreaScoring      -> (blackAreaScore, whiteAreaScore + 6.5)
  -- Print the scoring method, points and winner
  if scoringMethod gameState == TerritoryScoring then putStrLn "Scoring method used is Territoryscoring: " else
    putStrLn "Scoring method used is Areascoring: "
  putStrLn $ "  Black score: " ++ show blackScore
  putStrLn $ "  White score: " ++ show whiteScore
  putStrLn $ "  Winner: " ++ show (if blackScore > whiteScore then "Black" else "White")
  where
    board = currBoard gameState
    blackCaptured = length $ filter (== Just Black) (capturedStones gameState)  -- The amount of black stones in the capturedStones variable stored in gameState
    whiteCaptured = length $ filter (== Just White) (capturedStones gameState)  -- Same just for white
    blackStones = countStones board (Just Black)                                       -- Counts the amount of stones of a given color
    whiteStones = countStones board (Just White)
    blackTerritory = countTerritory board (Just Black) (sizeBoard gameState)           -- Calculate the territory of each player
    whiteTerritory = countTerritory board (Just White) (sizeBoard gameState)
    blackTerritoryScore = fromIntegral (whiteCaptured + blackTerritory)         -- Territory score for each player
    whiteTerritoryScore = fromIntegral (blackCaptured + whiteTerritory)
    blackAreaScore = fromIntegral (blackStones + blackTerritory)                -- Area score for each player
    whiteAreaScore = fromIntegral (whiteStones + whiteTerritory)




-- | I did not end up using this code in the code that is used for the game. It is a fnctionality that could be implemented later
-- Where for example the user wants to find out the liberties a group defined by a coordinate has.

-- | Returns the number of liberties a group has
countGroupLiberties :: Board -> [Coordinate] -> Int
countGroupLiberties board group =
  let adjacentCoordsList = concatMap (adjacentCoords board) group         -- Gets the adjacent coordinates of the group
      emptyAdjacentCoords = filter (isNothing . getStoneYX board) adjacentCoordsList  -- Gets all the empty adjacent coordinates
      uniqueEmptyAdjacentCoords = removeDuplicateCoords emptyAdjacentCoords           -- Removes the duplicate cooridnates
  in length uniqueEmptyAdjacentCoords

-- | Gets all the coordinates for the group of the correct color and the uses the CountGroupLiberties function to find
-- the number of liberties the group has
groupDegreesOfFreedom :: Board -> Coordinate -> Int
groupDegreesOfFreedom board coord =
  let stoneColor = if getStoneYX board coord == Just White then Just White else Just Black  -- Get the stoneColor
      group = findGroupWrapper board stoneColor coord getStoneYX                            -- Get the group
      groupLiberties = countGroupLiberties board group  -- Returns the number of adjacent empty coordinates to the group
  in groupLiberties                                     -- The degrees of freedom the group has