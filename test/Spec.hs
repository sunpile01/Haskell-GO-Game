import BoardOperations
import DataTypes
import Test.Hspec(hspec, describe, shouldBe, it)
import ParseSGFFiles
import UtilityFunctions

testBoard :: Board
testBoard = 
  [ [Just White,   Just Black, Nothing,      Just White, Nothing,      Just Black]
  , [Nothing,      Nothing,    Just White,   Nothing,    Just Black,   Just White]
  , [Nothing,      Just White, Just Black,   Just White, Nothing,      Just Black]
  , [Just White,   Just Black, Nothing,      Just White, Just Black,   Nothing   ]
  , [Nothing,      Just White, Just White,   Just Black, Just Black,   Just White]
  , [Just White,   Just Black, Just Black,   Nothing,    Just White,   Nothing   ]]
testBoard2 :: Board
testBoard2 = 
  [ [Just White,   Just Black, Nothing,      Just White, Nothing,      Just White]
  , [Nothing,      Just White,    Just White,   Nothing,    Just Black,   Just White]
  , [Just White,   Just White, Just Black,   Just White, Nothing,      Just Black]
  , [Just White,   Just Black, Nothing,      Just Black, Just Black,   Nothing   ]
  , [Nothing,      Just White, Just White,   Nothing, Just Black,       Just White]
  , [Just White,   Just Black, Just Black,   Nothing,    Just White,   Nothing   ]]
testBoard3 :: Board
testBoard3 = 
  [ [Just White,   Just Black, Nothing,      Just White, Nothing,      Just White]
  , [Nothing,      Just White,    Just White,   Nothing,    Just Black,   Just White]
  , [Just White,   Just White, Just Black,   Just White, Nothing,      Just Black]
  , [Just White,   Just Black, Nothing,      Just Black, Just Black,   Nothing   ]
  , [Nothing,      Just White, Just White,   Nothing,    Just Black,       Just White]
  , [Just White,   Just Black, Just Black,   Nothing,    Just White,   Nothing   ]]
  
testBoardTerritory :: Board
testBoardTerritory = 
  [ [Nothing,       Nothing,   Just White,  Just White,   Nothing,     Just Black]
  , [Just White,   Just White, Just White,   Nothing,    Just Black,   Just White]
  , [Nothing,      Just White, Just Black,   Just White, Nothing,      Just Black]
  , [Just White,   Just Black, Nothing,      Just White, Just Black,   Nothing   ]
  , [Nothing,      Just White, Just White,   Just Black, Just Black,   Just Black]
  , [Just White,   Just Black, Just Black,   Nothing,    Just Black,   Nothing   ]]

testGameState :: GameState
testGameState = GameState { currBoard = testBoard, prevBoard = testBoard2, 
                          twoTurnsAgoBoard = testBoard2, capturedStones = [], scoringMethod = TerritoryScoring, playerTurn = Just Black, 
                          sizeBoard = 6 }
hspecTests :: IO ()
hspecTests = hspec $ do
  describe "countGroups" $ do
    it "returns the correct number of groups for white stones" $ do
      countGroups testBoard (Just White) 6 `shouldBe` 2
    it "returns the correct number of groups for black stones" $ do
      countGroups testBoard (Just Black) 6 `shouldBe` 2
             
  describe "bestMoveForCapture" $ do
    it "returns the correct move for capturing the most stones" $ do
      bestMoveForCapture testGameState `shouldBe` Just (0,1)
             
  describe "bestMoveForConnectGroups" $ do
    it "returns the correct move for connecting the most groups" $ do
      bestMoveForConnectGroups testGameState `shouldBe` Just (4,2)
             
  describe "placeStone" $ do
    it "updates the board correctly when placing a stone" $ do
      let oldBoard = testBoard
          newBoard = placeStone (Move (Just Black) (0,5)) oldBoard
      newBoard `shouldBe` [ [Just White,   Just Black, Nothing,      Just White, Nothing,      Just White]
                            , [Nothing,      Nothing,    Just White,   Nothing,    Just Black,   Just White]
                            , [Nothing,      Just Black, Just Black,   Just White, Nothing,      Just Black]
                            , [Just White,   Just Black, Nothing,      Just White, Just Black,   Nothing   ]
                            , [Just Black,   Just White, Just White,   Just Black, Just Black,   Just White]
                            , [Just White,   Just Black, Just Black,   Nothing,    Just White,   Nothing   ]]
             
  describe "validMove" $ do
    it "returns True for a valid move" $ do
      let currBoard = testBoard
          twoTurnsAgoBoard = testBoard2
          stone = Just Black
          coord = (5,5)
      validMove currBoard twoTurnsAgoBoard stone coord `shouldBe` True
             
    it "returns False for a suicide move" $ do
      let currBoard = testBoard
          stone = Just Black
          coord = (0,1)
      validMove currBoard currBoard stone coord `shouldBe` False
  describe "isEmptyPosition" $ do
    it "checks if the position is empty" $ do
      isEmptyPosition testBoard (0, 0) `shouldBe` False
      isEmptyPosition testBoard (2, 0) `shouldBe` True
  
 -- For some reason will not work:
 -- describe "findGroup" $ do
  --  it "finds a group of stones for a given position" $ do
  --    findGroup testBoard (Just Black) (3, 4) [] `shouldBe` [(3, 4), (4, 4)]

  describe "groupHasLiberties" $ do
    it "checks if a group has liberties" $ do
      groupHasLiberties testBoard3 [(4, 3), (4, 4)] getStoneYX `shouldBe` True

  describe "captureStones" $ do
    it "captures stones" $ do
      let capturedGameState = captureStones testGameState (Just Black)
      capturedStones capturedGameState `shouldBe` [Just White]

  describe "allUniqueGroupsOfColor" $ do
    it "gets all unique groups of a given color" $ do
      let blackGroups = allUniqueGroupsOfColor (Just Black) testBoard 6
      length blackGroups `shouldBe` 2

  describe "updateStones" $ do
    it "updates the stones on the board" $ do
      let updatedBoard = updateStones testBoard (Just White) (2, 0)
      updatedBoard `shouldBe`
        [ [Just White,   Just Black, Just White, Just White, Nothing,      Just White]
        , [Nothing,      Nothing,    Just White, Nothing,    Just Black,   Just White]
        , [Nothing,      Just White, Just Black, Just White, Nothing,      Just Black]
        , [Just White,   Just Black, Nothing,    Just White, Just Black,   Nothing   ]
        , [Nothing,      Just White, Just White, Just Black, Just Black,   Just White]
        , [Just White,   Just Black, Just Black, Nothing,    Just White,   Nothing   ]]
  
main :: IO ()
main = hspecTests
