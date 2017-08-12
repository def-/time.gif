import GifStream
import Numbers

import System.Random
import Data.Time.Clock
import Data.Time.LocalTime
import Data.List

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

      let display = map (map colorize) $ frame $
            foldr1 (zipWith (++)) $ map (numbers !!) $ intersperse 10
              [ h `div` 10, h `mod` 10, 11
              , m `div` 10, m `mod` 10, 11
              , (floor s) `div` 10, (floor s) `mod` 10
              ]

      sendFrame (scale zoom display)
      wait
      go

frame xs = map (preappend 0) $ preappend (take (length (xs !! 0)) (repeat 0)) xs

preappend x xs = x : xs ++ [x]

colorize 1 = (3,3,3)
colorize 0 = (0,0,0)

scale :: Int -> Frame -> Frame
scale z frame = concatMap (replicate z) (map (concatMap (replicate z)) frame)
