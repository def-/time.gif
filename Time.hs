import System.Random
import GifStream
import Data.Time.Clock
import Data.Time.LocalTime
import Data.List

-- Stopping focus of the browser tab stops the animation. Reload the page to fix it.

type Position = (Int,Int)

numbers =
  [ [ [1,1,1]
    , [1,0,1]
    , [1,0,1]
    , [1,0,1]
    , [1,1,1] ]

  , [ [0,1,0]
    , [0,1,0]
    , [0,1,0]
    , [0,1,0]
    , [0,1,0] ]

  , [ [1,1,1]
    , [0,0,1]
    , [1,1,1]
    , [1,0,0]
    , [1,1,1] ]

  , [ [1,1,1]
    , [0,0,1]
    , [0,1,1]
    , [0,0,1]
    , [1,1,1] ]

  , [ [1,0,1]
    , [1,0,1]
    , [1,1,1]
    , [0,0,1]
    , [0,0,1] ]

  , [ [1,1,1]
    , [1,0,0]
    , [1,1,1]
    , [0,0,1]
    , [1,1,1] ]

  , [ [1,1,1]
    , [1,0,0]
    , [1,1,1]
    , [1,0,1]
    , [1,1,1] ]

  , [ [1,1,1]
    , [0,0,1]
    , [0,0,1]
    , [0,0,1]
    , [0,0,1] ]

  , [ [1,1,1]
    , [1,0,1]
    , [1,1,1]
    , [1,0,1]
    , [1,1,1] ]

  , [ [1,1,1]
    , [1,0,1]
    , [1,1,1]
    , [0,0,1]
    , [1,1,1] ]

  , [ [0]
    , [0]
    , [0]
    , [0]
    , [0] ]

  , [ [0,0,0]
    , [0,1,0]
    , [0,0,0]
    , [0,1,0]
    , [0,0,0] ]
  ]

-- 30000 seems to be the lowest value that works in Firefox
-- 30 ms => 33 fps
delay = 1000000 -- in µs
port = 5002

width = 33
height = 7
zoom = 4

main :: IO ()
main = server port delay logic

logic :: IO () -> IO Char -> (Frame -> IO ()) -> IO ()
logic wait getInput sendFrame = go
  where
    go = do
      now <- getCurrentTime
      let (TimeOfDay h m s) = localTimeOfDay $ utcToLocalTime utc now
      let nums = append $ map (numbers !!) $ intersperse 10 [h `div` 10, h `mod` 10, 11, m `div` 10, m `mod` 10, 11, (floor s) `div` 10, (floor s) `mod` 10]
      let framedNums = map (map colorize) (frame nums)
      sendFrame (scale zoom framedNums)

      wait
      go

append [x] = x
append (x:xs) = zipWith (++) x (append xs)

frame xs = map framepart (z : xs ++ [z])
  where z = take (length (xs !! 0)) (repeat 0)
        framepart xs = 0 : xs ++ [0]

colorize 1 = (3,3,3)
colorize 0 = (0,0,0)

scale :: Int -> Frame -> Frame
scale z frame = concatMap (replicate z) (map (concatMap (replicate z)) frame)
