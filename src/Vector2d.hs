module Vector2d where

import           Control.Monad (guard)
import           Data.Vector
import           Prelude       hiding (head, length, map, replicate, zip)
import qualified Prelude       as P
import           Util

data Vector2d a = Vector2d (Int, Int) (Vector (Vector a))
  deriving (Show)

initialise :: (Int, Int) -> a -> Vector2d a
initialise (w, h) a = Vector2d (w, h) $ replicate h (replicate w a)

fromLists :: [[a]] -> Vector2d a
fromLists l = Vector2d (w,h) $ fromList $ P.map fromList l
  where h = P.length l
        w | h == 0    = 0
          | otherwise = P.length $ P.head l

toLists :: Vector2d a -> [[a]]
toLists (Vector2d _ v) = toList $ map toList v

size :: Vector2d a -> (Int,Int)
size (Vector2d s _) = s

atIndex :: Vector2d a -> (Int, Int) -> a
atIndex (Vector2d _ v) (x,y) = (v ! y) ! x

instance Functor Vector2d where
  fmap g (Vector2d s v) = Vector2d s (map (map g) v)

replace :: Vector2d a -> (Int, Int) -> a -> Vector2d a
replace (Vector2d s v) (x,y) a = Vector2d s v'
  where r  = v ! y
        r' = r // [(x, a)]
        v' = v // [(y, r')]

within :: (Int, Int) -> Vector2d a -> Bool
within (x,y) (Vector2d (w, h) _)
  | x < 0  || y < 0  = False
  | x >= w || y >= h = False
  | otherwise        = True

zipV2 :: Vector2d a -> Vector2d b -> Vector2d (a, b)
zipV2 (Vector2d s v1) (Vector2d _ v2) = Vector2d s $ map (uncurry zip) $ zip v1 v2

diffV2 :: Eq a => Vector2d a -> Vector2d a -> Vector ((Int, Int), a)
diffV2 v1 v2 = do
  let (Vector2d _ v) = zipV2 v1 v2
  (y, row)          <- enumerate v
  (x, (a,b))        <- enumerate row
  guard (a/=b)
  return ((x,y), b)
    where enumerate v = zip (generate (length v) id) v

