module Main where
import           Control.Exception   (finally)
import           Data.List
import           Prelude             hiding (Left, Right)
import           System.Console.ANSI
import           Util
import           Vector2d

data Tile = Empty | Block | PlayerTile
  deriving (Show, Eq)

data GameState = GameState
  { sWorldSize  :: (Int, Int)
  , sWorld      :: Vector2d Tile
  , sQbbyCoords :: (Int, Int)
  } deriving (Show)

world :: Vector2d Tile
world = fromLists $ reverse $ replicate 10 Block : replicate 5 (replicate 10 Empty)

initState :: GameState
initState = GameState
  { sWorldSize = size world
  , sWorld = world
  , sQbbyCoords = (1,snd (size world) - 2)
  }

nextState :: String -> GameState -> GameState
nextState "k" = move Up
nextState "j" = move Down
nextState "h" = move Left
nextState "l" = move Right
nextState  _  = id

render :: GameState -> String
render g = intercalate "\n" $ toLists $ fmap renderTile $ addPlayerTile (sQbbyCoords g) $ sWorld g
  where renderTile Empty      = ' '
        renderTile Block      = '▒'
        renderTile PlayerTile = '▄'
        addPlayerTile :: (Int,Int) -> Vector2d Tile -> Vector2d Tile
        addPlayerTile p s  = replace s p PlayerTile

main = inputMode True >> loop update initState `finally` inputMode False
  where loop f = g
          where g x = f x >>= g

update s =
  clearScreen >>
  setCursorPosition 0 0 >>
  putStrLn (render s) >>
  flip nextState s <$> getKey

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
