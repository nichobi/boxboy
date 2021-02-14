module Util where
import           System.Console.ANSI
import           System.IO           (BufferMode (..), hReady, hSetBuffering,
                                      hSetEcho, stdin)

map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f, g) (x,y) = (f x, g y)

addTuple :: Num a => (a,a) -> (a,a) -> (a,a)
addTuple = map2 . map2 ((+), (+))

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

inputMode True  =  hideCursor
                >> hSetBuffering stdin NoBuffering
                >> hSetEcho stdin False
inputMode False =  showCursor
                >> hSetBuffering stdin LineBuffering
                >> hSetEcho stdin True

