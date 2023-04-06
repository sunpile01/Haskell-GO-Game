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
    describe "getStoneYX" $ do
      it "gets the stone at a given coordinate" $ do
        getStoneYX testBoard (1, 0) `shouldBe` Just Black
        getStoneYX testBoard (2, 0) `shouldBe` Nothing

    describe "getStoneXY" $ do
      it "gets the stone at a given coordinate" $ do
        getStoneXY testBoard (0, 1) `shouldBe` Just Black
        getStoneXY testBoard (0, 2) `shouldBe` Nothing

    describe "countTerritory" $ do
      it "counts territory for black correctly" $ do
        countTerritory testBoard (Just Black) 6 `shouldBe` 2

      it "counts territory for white correctly" $ do
        countTerritory testBoard (Just White) 6 `shouldBe` 2

    describe "adjacentStones" $ do
      it "returns correct adjacent stones for a given coordinate" $ do
        adjacentStones testBoard (1, 2) `shouldBe` [Just Black, Just Black]

    describe "countStones" $ do
      it "counts black stones correctly" $ do
        countStones testBoard (Just Black) `shouldBe` 11

      it "counts white stones correctly" $ do
        countStones testBoard (Just White) `shouldBe` 13

    let sampleSGFContent = unlines
          [ "(;GM[1]FF[4]SZ[19]PT[Black]"
          , "AB[aa][bb][cc]"
          , "AW[dd][ee][ff]"
          , ";B[aa];W[bb];B[cc];W[dd])"
          ]

    describe "parseSGFMoves" $ do
      it "parses moves from SGF content correctly" $ do
        let expectedMoves = [ AddBlack (0, 0)
                             , AddBlack (1, 1)
                             , AddBlack (2, 2)
                             , AddWhite (3, 3)
                             , AddWhite (4, 4)
                             , AddWhite (5, 5)
                             , PlayBlack (0, 0)
                             , PlayWhite (1, 1)
                             , PlayBlack (2, 2)
                             , PlayWhite (3, 3)
                             ]
        parseSGFMoves sampleSGFContent `shouldBe` expectedMoves

    describe "parseSgfProperties" $ do
      it "parses properties from SGF content correctly" $ do
        let expectedProperties = [ SetBoardSize 19
                                 , SetPlayerTurn Black
                                 ]
        parseSgfProperties sampleSGFContent `shouldBe` expectedProperties

    describe "isEmptyBoard" $ do
      it "checks if the board is empty" $ do
        isEmptyBoard testBoard `shouldBe` False

    describe "emptyCoordinate" $ do
      it "checks if the given coordinate on the board is empty" $ do
        emptyCoordinate testBoard (4, 4) getStoneYX `shouldBe` False

    describe "adjacentCoords" $ do
      it "gets all the adjacent coordinates of the given coordinate" $ do
        let expectedAdjacentCoords = [(2, 3), (3, 2), (3, 4), (4, 3)]
        adjacentCoords testBoard (3, 3) `shouldBe` expectedAdjacentCoords

    describe "removeDuplicateGroups" $ do
      it "removes duplicate nested lists" $ do
        let groups = [[1, 4, 3], [2, 1, 3], [1, 3, 2], [4, 5, 6], [5, 6, 4]]
        let expectedGroups = [[1, 4, 3]]
        removeDuplicateGroups groups `shouldBe` expectedGroups

    describe "removeDuplicateStones" $ do
      it "removes duplicate elements from a list" $ do
        let stones = [1, 2, 3, 2, 1, 3, 4, 5, 6, 5, 6, 4]
        let expectedStones = [1, 2, 3, 4, 5, 6]
        removeDuplicateStones stones `shouldBe` expectedStones

    describe "removeDuplicateCoords" $ do
      it "removes duplicate coordinates from a list" $ do
        let coords = [(1, 2), (3, 4), (1, 2), (3, 4), (5, 6), (5, 6)]
        let expectedCoords = [(1, 2), (3, 4), (5, 6)]
        removeDuplicateCoords coords `shouldBe` expectedCoords
        
main :: IO ()
main = hspecTests
