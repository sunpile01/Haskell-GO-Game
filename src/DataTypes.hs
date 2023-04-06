module DataTypes (
  SGFProperties(..),
  SGFMove(..),
  Stone (..),
  ComputerMove(..),
  Board,
  ScoringSystem(..),
  Coordinate,
  Move(..),
  GameState(..),
) where
import Foreign.C.Types (CInt)

data SGFMove
  = AddBlack Coordinate
  | AddWhite Coordinate
  | PlayBlack Coordinate
  | PlayWhite Coordinate
  deriving (Show, Eq)
  
data SGFProperties = SetBoardSize CInt | SetPlayerTurn Stone deriving (Show, Eq)
data Stone = Black | White deriving (Show, Eq)
data ComputerMove = MostEliminated | ExtendLiberties | ConnectGroups
type Board = [[Maybe Stone]]

data GameState = GameState {
  currBoard :: Board,
  prevBoard :: Board, 
  twoTurnsAgoBoard :: Board,
  capturedStones :: [Maybe Stone],
  scoringMethod :: ScoringSystem,
  playerTurn :: Maybe Stone,
  sizeBoard :: CInt
}

data ScoringSystem = TerritoryScoring | AreaScoring deriving (Eq, Show)
type Coordinate = (Int, Int)

data Move = Move {
  stone :: Maybe Stone,
  coord :: Coordinate
} deriving (Show, Eq)
