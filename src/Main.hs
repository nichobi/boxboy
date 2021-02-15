module Main where
import           Control.Exception     (finally)
import           Data.List
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector           as V
import           Prelude               hiding (Left, Right)
import           System.Console.ANSI
import           Util
import           Vector2d

data Tile = Empty | Block | PlayerTile
  deriving (Show, Eq)

data GameState = GameState
  { sWorld      :: Vector2d Tile
  , sQbbyCoords :: (Int, Int)
  , sPrevState  :: Maybe GameState
  } deriving (Show)

world :: Vector2d Tile
world = fromLists $ reverse $ replicate 10 Block : replicate 5 (replicate 10 Empty)

initState :: GameState
initState = GameState
  { sWorld      = world
  , sQbbyCoords = (1, snd (size world) - 2)
  , sPrevState  = Nothing
  }

nextState :: String -> GameState -> GameState
nextState k g = updatePrevState g $ handleKey k $ move Down g
  where handleKey "k" = move Up
        handleKey "j" = move Down
        handleKey "h" = move Left
        handleKey "l" = move Right
        handleKey ""  = id
        handleKey  _  = id
        updatePrevState old new = new {sPrevState = Just old}

renderTile Empty      = ' '
renderTile Block      = '▒'
renderTile PlayerTile = '▄'

worldWithPlayer :: GameState -> Vector2d Tile
worldWithPlayer GameState{sQbbyCoords = p, sWorld = w}  = replace w p PlayerTile

drawScreen :: GameState -> IO ()
-- Draw only what's changed since the previous state
drawScreen gs@GameState{sPrevState = Just ps} =
    V.mapM_ (uncurry printPoint) $ diffV2 (worldWithPlayer ps) (worldWithPlayer gs)
  where printPoint :: (Int,Int) -> Tile -> IO ()
        printPoint (x,y) t = setCursorPosition y x >> putChar (renderTile t)
-- Draw entire screen, as there is no previous state
drawScreen gs = clearScreen >> setCursorPosition 0 0 >> putStrLn (render gs)
  where render :: GameState -> String
        render gs = intercalate "\n" $ toLists $ fmap renderTile $ worldWithPlayer gs

main = inputMode True >> loopWithDelay (1/10) update initState `finally` inputMode False
  where update s = drawScreen s >> flip nextState s <$> pollKey

data Direction = Up | Down | Left | Right deriving (Show, Eq)

dirToDelta :: Direction -> (Int, Int)
dirToDelta Up    = ( 0, -1)
dirToDelta Down  = ( 0,  1)
dirToDelta Left  = (-1,  0)
dirToDelta Right = ( 1,  0)

move :: Direction -> GameState -> GameState
move d gs
  | not $ newCoords `within` sWorld gs     = gs
  | sWorld gs `atIndex` newCoords /= Empty = gs
  | otherwise                              = gs {sQbbyCoords = newCoords}
  where newCoords = sQbbyCoords gs `addTuple` dirToDelta d
