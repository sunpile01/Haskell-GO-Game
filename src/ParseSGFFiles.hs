module ParseSGFFiles (
  parseSGFMoves,
  parseSgfProperties,
  applySGFBoardSize,
  applySGFMoves,
  applySGFPlayerTurn
) where 
  
import Foreign.C.Types (CInt)
import Data.List.Split (splitOn)
import Data.Char
import DataTypes
import Data.List (isPrefixOf)
import BoardOperations (captureStones, placeStone)

-- | Parses the moves in the SGF file, the mvoes are the fields with the prefix AB, AW, B and W in the SGF file.
-- The moves are stored in a list of the custom SGFMove data type
parseSGFMoves :: String -> [SGFMove]
parseSGFMoves = parse . concatMap words . lines  -- point free notation ConcatMap words .lines gets all the words from the file into a list of words
  where
    parse [] = []                 
    parse (x:xs)                -- Recursive case, pattern with x being the head and xs being the tail
      -- If the current string (x) starts with "AB", parse the moves for AddBlack, 
      | "AB" `isPrefixOf` x = parseMoves AddBlack (drop 3 x) ++ parse xs -- uses ++ because parseMoves returns a list of moves so we 
      | "AW" `isPrefixOf` x = parseMoves AddWhite (drop 3 x) ++ parse xs -- concatenate the two lists. This is because AB and AW is followed by multiple moves
      | ";B[" `isPrefixOf` x = PlayBlack (parseCoord (drop 3 x)) : parse xs -- We only have to parse the coordinate beacuse B or W only has one move after it
      | ";W[" `isPrefixOf` x = PlayWhite (parseCoord (drop 3 x)) : parse xs -- We therefore use : to concatenate the new move to the list
      | otherwise = parse xs                            -- None of the above cases was detected continue with the rest of the list
    parseMoves moveType str =                               -- Function that parses the consecutvie moves after AW and AB 
      map (moveType . parseCoord) (splitOn "][" (init str)) -- splits the strings on  ][ because that is what is between each coordinate
    parseCoord (a:b:_) = (ord a - ord 'a', ord b - ord 'a') -- SGF file uses letters so this converts the letters into numbers 
    parseCoord _ = error "Wrong format in the SGF File"

-- | This function parses the file in the same way, but looks for properties for the game and not moves instead.
parseSgfProperties :: String -> [SGFProperties]
parseSgfProperties = parse . concatMap words . lines
  where
    parse [] = []
    parse (x:xs)
      | "SZ[" `isPrefixOf` x = SetBoardSize (parseSize (init (drop 3 x)) :: CInt) : parse xs  -- Parse SZ property
      | "PT[" `isPrefixOf` x = SetPlayerTurn (parsePlayer (init (drop 3 x))) : parse xs       -- Parse PT property
      | otherwise = parse xs                                                                  -- Neither SZ or PT property
    parseSize s = fromIntegral (read s :: Int) :: CInt    -- Just converts the number wrapped in String to a number of the CInt type
    parsePlayer "Black" = Black                           -- Just returns the Black or White depending on what the String in the SGF was
    parsePlayer "White" = White
    parsePlayer _ = error "Invalid player"
 
 -- | For every move in the SGFMove list it updates the board by using the applySGFMove function 
applySGFMoves :: Board -> [SGFMove] -> CInt -> Board
applySGFMoves board moves boardSize = foldl (\b move -> applySGFMove b move boardSize) board moves
 
-- | This function processes places an SGF nove on the actual gameBoard. When it is a AB or AW field we dont have to check for captures
applySGFMove :: Board -> SGFMove -> CInt -> Board
applySGFMove board move boardSize = 
  case move of                                                    -- Pattern matches the type of sgf move 
    AddBlack coord -> placeStone (Move (Just Black) coord) board  -- Just have to place the stone and not check for captures for AB and AW
    AddWhite coord -> placeStone (Move (Just White) coord) board
    PlayBlack coord -> updateGameState (Just Black) coord         -- Here we have to check for potential captures for the B and W
    PlayWhite coord -> updateGameState (Just White) coord         
  where
    updateGameState stone coord =                                 -- Helper function that returns the updated board that has done potential captures
      let gameState = GameState (placeStone (Move stone coord) board) [] [] [] TerritoryScoring stone boardSize
          gameStateAfterCaptures = captureStones gameState stone  -- Caotures potential stones or groups without liberties
      in currBoard gameStateAfterCaptures

-- | If i add possibilities for extra SGF properties i should definetly use a helper function That does the work for the 
-- common parts of the code for every applySGF.. function. But it added more code doing so when i only have 2 cases

-- | Extracts the boardSize from the list of SGF properties and returns this board size
applySGFBoardSize :: [SGFProperties] -> CInt
applySGFBoardSize [] = 13 :: CInt           -- Default board size if no setBoardSize property is found 
applySGFBoardSize (x:xs) =                  -- Uses recursion to check the moves until it finds the matching setBoardSize type
  case x of
    SetBoardSize boardSize -> boardSize
    _                 -> applySGFBoardSize xs    

-- | Extracts whose turn it is from the list of SGF properties
applySGFPlayerTurn :: [SGFProperties] -> Maybe Stone
applySGFPlayerTurn [] = Just Black       -- Default player turn if no SetPlayerTurn property is found
applySGFPlayerTurn (move:moves) =        -- Uses recursion to check the moves until it finds the matching setBoardSize type
  case move of
    SetPlayerTurn player -> Just player
    _                    -> applySGFPlayerTurn moves
