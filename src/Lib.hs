module Lib
  ( someFunc,
    Point (..),
    isOrigin,
    distance,
  )
where

-- | A sample function printing out a sample string.
someFunc :: IO ()
someFunc = putStrLn "0.1.3.3"

-- | A Point.
data Point = Point {x :: Int, y :: Int}
  deriving (Eq)

-- | Show instance for Point.
instance Show Point where
  show (Point x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

-- | Check if the point is at origin, i.e., (0,0)
isOrigin :: Point -> Bool
isOrigin (Point 0 0) = True
isOrigin _ = False

-- | Computes the distance between two Points.
distance :: Point -> Point -> Float
distance (Point x1 y1) (Point x2 y2) = sqrt . fromIntegral $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2
