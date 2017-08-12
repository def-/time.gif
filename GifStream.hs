{-# LANGUAGE OverloadedStrings #-}

-- | Generate dynamic GIF streams and provide them on an HTTP server.
module GifStream (
  -- Functions
  server,
  -- Types
  RGB,
  Frame,
  FrameSignal,
  Logic
  )
  where

import System.IO

import Network hiding (accept)
import Network.Socket
import Network.Socket.ByteString (sendAll)

import Control.Monad
import Control.Concurrent

import Data.IORef

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8() -- for OverloadedStrings

type RGB = (Int,Int,Int) -- ^ Values in [0..3]
type Frame = [[RGB]]
type FrameSignal = MSignal Frame
type Logic = IO () -> IO Char -> (Frame -> IO ()) -> IO ()

-- | Run an HTTP server that delivers a continuing stream of a GIF to every
--   incoming connections. A logic function is called to generate the GIF
--   frames.
server :: PortNumber -> Int -> Logic -> IO ()
server port delay logic = withSocketsDo $ do
  hSetBuffering stdin NoBuffering
  sock <- listenOn $ PortNumber port

  putStrLn $ "Listening on http://127.0.0.1:" ++ show port ++ "/"

  wait <- getMetronome delay
  getAction <- inputGetter
  frameSignal <- newMSignal

  forkIO $ loop delay frameSignal sock

  logic wait getAction $ sendMSignal frameSignal

-- | Wait for incoming connections and start delivering a GIF to them
loop :: Int -> FrameSignal -> Socket -> IO ()
loop delay frameSignal sock = do
  (conn, _) <- accept sock

  forkIO $ body conn
  loop delay frameSignal sock

  where -- lower delay in GIF to force browser to actually show the gif we send
    body c = do
      f <- receiveMSignal frameSignal
      sendAll c $ msg $ initialFrame (delay `div` 10000) f
      nextFrame c

    nextFrame c = do
      f <- receiveMSignal frameSignal
      sendAll c $ frame (delay `div` 10000) f
      nextFrame c

    msg content = B.intercalate "\r\n"
      [ "HTTP/1.0 200 OK"
      , "Server: gifstream/0.1"
      , "Content-Type: image/gif"
      , "Content-Transfer-Encoding: binary"
      , "Cache-Control: no-cache"
      , "Cache-Control: no-store"
      , "Cache-Control: no-transform"
      , ""
      , content
      ]

-- | Get a function that waits for the specified time whenever it's called
getMetronome :: Int -> IO (IO ())
getMetronome delay = do
    var <- newMVar ()
    forkIO $ forever $ do
        threadDelay delay
        putMVar var ()
    return $ takeMVar var

-- | Get a function that returns the last key pressed whenever it's called
inputGetter :: IO (IO Char)
inputGetter = do
    inputRef <- newIORef 'd' -- Default input
    forkIO $ forever $ do
        c <- getChar
        writeIORef inputRef c
    return $ readIORef inputRef

-- | Create the initial frame of a GIF. Note that this frame determines the size of the GIF.
initialFrame :: Int -> Frame -> B.ByteString
initialFrame delay img = B.concat
  [ "GIF89a"
  , number w, number h, gctInfo, bgColor, aspect -- logical screen descriptor
  , realCT, dummyCT                              -- color table
  , "!\255\vNETSCAPE2.0\ETX\SOH\NUL\NUL\NUL"     -- application extension
  , frame delay img
  ]
  where
    w = length $ head img
    h = length img
    gctInfo = B.singleton 0xf6
    bgColor = smallNumber 127
    aspect  = "\NUL"

    realCT  = B.concat $ map B.pack [[r,g,b] | r <- colors, g <- colors, b <- colors]
    colors  = [0,64,128,255]
    dummyCT = B.concat $ replicate 64 $ B.pack [255,255,255]

-- | Create the next frame in a GIF
frame :: Int -> Frame -> B.ByteString
frame delay img = B.concat
  [ "!\249\EOT\b", number delay, "\255", "\NUL"  -- graphic control extension
  , ",", yPos, xPos, number w, number h, localCT -- image descriptor
  , lzwMinSize, imageData, "\NUL"                -- image
  ]
  where
    w = length $ head img
    h = length img
    yPos = number 0
    xPos = number 0
    localCT = "\NUL"

    lzwMinSize = B.singleton 0x07
    imageData = B.concat $ map (B.concat . mapLine) img

    mapLine x
      | null ys   = z
      | otherwise = z ++ mapLine ys
      where (y,ys) = splitAt 126 x
            z = [ bytesToFollow, clear
                , B.pack $ map (\(r,g,b) -> fromIntegral $ 16*r+4*g+b) y
                ]
            bytesToFollow = smallNumber $ length y + 1
            clear = B.singleton 0x80

-- | Close the GIF file
finalize :: B.ByteString
finalize = B.concat [bytesToFollow, stop, "\NUL", ";"]
  where
    bytesToFollow = smallNumber 1
    stop = B.singleton 0x81

-- | Convert a number to one Byte
smallNumber :: Int -> B.ByteString
smallNumber x = B.singleton $ fromIntegral $ x `mod` 256

-- | Convert a number to two Bytes
number :: Int -> B.ByteString
number x = B.pack $ map fromIntegral [x `mod` 256, x `div` 256]

-- | A Module for broadcast signalling between threads.
-- By Joachim Breitner

-- | MSignal is an opaque data type
newtype MSignal a = MS (MVar a)

-- | Creates a new MSignal object. This can be used to send and receive signals, possibly containing some data. If you do not want to transmit data, use @'MSignal' ()@
newMSignal :: IO (MSignal a)
newMSignal = MS `liftM` newEmptyMVar

-- | Sends new data to all threads currently running 'receiveMSignal'
sendMSignal :: MSignal a -> a -> IO ()
sendMSignal (MS mv) v = do
  forkIO $ takeMVar mv >> return () -- Cleanup afterwards
  putMVar mv v

-- | Blocks until another threads sends data using 'sendMSignal'. It then returns the sent data.
receiveMSignal :: MSignal a -> IO a
receiveMSignal (MS mv) = readMVar mv
