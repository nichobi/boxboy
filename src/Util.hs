module Util where
import           Control.Concurrent     (threadDelay)
import           Control.Monad (guard)
import           Data.Time.Clock.System (getSystemTime, systemToTAITime)
import           Data.Time.Clock.TAI    (AbsoluteTime, diffAbsoluteTime)
import           System.Console.ANSI
import           System.IO              (BufferMode (..), hReady, hSetBuffering,
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
pollKey :: IO [Char]
pollKey = do
    ready <- hReady stdin
    if ready then getKey else pure ""


inputMode True  =  hideCursor
                >> hSetBuffering stdin NoBuffering
                >> hSetEcho stdin False
inputMode False =  showCursor
                >> hSetBuffering stdin LineBuffering
                >> hSetEcho stdin True

loopWithDelay t f = g
  where g x = (f x) `delayed` t >>= g

delayed :: IO a -> Double -> IO a
delayed f t = do
    startTime <- systemToTAITime <$> getSystemTime
    res       <- f
    endTime   <- systemToTAITime <$> getSystemTime
    _         <- compensateTime t startTime endTime
    return res

compensateTime :: Double -> AbsoluteTime -> AbsoluteTime -> IO ()
compensateTime t start end
  | delta >= t = pure ()
  | otherwise  = threadDelay millisLeft
    where delta      = realToFrac $ diffAbsoluteTime end start :: Double
          millisLeft = round $ 1e6 * (t - delta)

