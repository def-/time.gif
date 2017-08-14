import GifStream
import Numbers

import System.Random
import Data.Time.Clock
import Data.Time.LocalTime
import Data.List

import qualified Data.ByteString as B
import qualified Data.Vector.Storable as V

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

      let display = scale zoom $ map (map colorize) $ framing $
            foldr1 (zipWith (++)) $ map (numbers !!) $ intersperse 10
              [ h `div` 10, h `mod` 10, 11
              , m `div` 10, m `mod` 10, 11
              , (floor s) `div` 10, (floor s) `mod` 10
              ]

      let w = length $ head display
      let h = length display
      let img = frame (delay `div` 10000) (B.concat $ map mapLine display, w, h)

      sendFrame (img, w, h)
      wait
      go

framing xs = map (preappend 0) $ preappend (take (length (xs !! 0)) (repeat 0)) xs

mapLine x = B.pack $ map (\(r,g,b) -> fromIntegral $ 16*r+4*g+b) x

preappend x xs = x : xs ++ [x]

colorize 1 = (3,3,3)
colorize 0 = (0,0,0)

scale z frame = concatMap (replicate z) (map (concatMap (replicate z)) frame)
