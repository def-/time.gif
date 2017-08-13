{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

-- | Generate dynamic GIF streams and provide them on an HTTP server.
module GifStream (
  -- Functions
  server,
  frame,
  -- Types
  Frame,
  FrameSignal,
  Logic
  )
  where

import System.IO
import System.IO.Unsafe

import Network hiding (accept)
import Network.Socket (accept, close)
import Network.Socket.ByteString (sendAll, recv)

import Codec.Picture.Gif.LZWEncoding

import Control.Concurrent
import Control.Exception
import Control.Monad

import Data.Maybe
import Data.List
import Data.IORef
import Data.Word (Word8)

import Data.Binary.Put( Put
                      , putWord8
                      , putWord16le
                      , putLazyByteString
                      , runPut
                      )

import qualified Data.Vector.Storable as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8() -- for OverloadedStrings
import Data.ByteString.Lazy (toStrict)

type Frame = (B.ByteString,Int,Int)
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

  where
    body c = handle (\(SomeException _) -> close c) $ do
      f <- receiveMSignal frameSignal
      recv c 4096
      threadDelay 500000
      sendAll c $ msg $ initialFrame (delay `div` 10000) f
      nextFrame c

    nextFrame c = do
      (f, _, _) <- receiveMSignal frameSignal
      sendAll c f
      nextFrame c

    msg content = B.intercalate "\r\n"
      [ "HTTP/1.0 200 OK"
      , "Server: gifstream/0.1"
      , "Content-Type: image/gif"
      , "Content-Transfer-Encoding: binary"
      , "Cache-Control: no-cache"
      , "Cache-Control: no-store"
      , "Cache-Control: no-transform"
      , "Expires: 0"
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
initialFrame delay (img, w, h) = B.concat
  [ "GIF89a"
  , number w, number h, gctInfo, bgColor, aspect -- logical screen descriptor
  , realCT, dummyCT                              -- color table
  , "!\255\vNETSCAPE2.0\ETX\SOH\NUL\NUL\NUL"     -- application extension
  , img
  ]
  where
    gctInfo = B.singleton 0xf6
    bgColor = B.singleton 128
    aspect  = "\NUL"

    realCT  = B.concat $ map B.pack [[r,g,b] | r <- colors, g <- colors, b <- colors]
    colors  = [0,64,128,255]
    dummyCT = B.concat $ replicate 64 $ B.pack [255,255,255]

-- | Create the next frame in a GIF
frame :: Int -> Frame -> B.ByteString
frame delay (img, w, h) = B.concat
  [ "!\249\EOT\b", number delay, "\255", "\NUL"  -- graphic control extension
  , ",", yPos, xPos, number w, number h, localCT -- image descriptor
  , lzwKeySize, lzwImg                           -- image
  ]
  where
    yPos = number 0
    xPos = number 0
    lzwKeySize = B.singleton (computeMinimumLzwKeySize w)
    lzwImg = toStrict $ runPut $ putDataBlocks $
             lzwEncode (computeMinimumLzwKeySize w) $
             V.generate (B.length img) $ \i -> B.index img i
    localCT = "\NUL"

putDataBlocks :: BL.ByteString -> Put
putDataBlocks wholeString = putSlices wholeString >> putWord8 0
  where putSlices str | BL.length str == 0 = pure ()
                      | BL.length str > 0xFF =
            let (before, after) = BL.splitAt 0xFF str in
            putWord8 0xFF >> putLazyByteString before >> putSlices after
        putSlices str =
            putWord8 (fromIntegral $ BL.length str) >> putLazyByteString str

computeMinimumLzwKeySize itemCount = go 2
  where go k | 2 ^ k >= itemCount = k
             | otherwise = go $ k + 1

-- | Close the GIF file
finalize :: B.ByteString
finalize = B.concat [bytesToFollow, stop, "\NUL", ";"]
  where
    bytesToFollow = B.singleton 1
    stop = B.singleton 0x81

-- | Convert a number to two Bytes
number :: Int -> B.ByteString
number x = toStrict $ runPut $ putWord16le $ fromIntegral x

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
