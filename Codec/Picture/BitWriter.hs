{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
-- | This module implement helper functions to read & write data
-- at bits level.
module Codec.Picture.BitWriter( BoolReader
                              , emptyBoolState
                              , BoolState
                              , byteAlignJpg
                              , getNextBitsLSBFirst
                              , getNextBitsMSBFirst 
                              , getNextBitJpg
                              , getNextIntJpg
                              , setDecodedString
                              , setDecodedStringMSB
                              , setDecodedStringJpg
                              , runBoolReader

                              , BoolWriteStateRef 
                              , newWriteStateRef
                              , finalizeBoolWriter
                              , writeBits'
                              , writeBitsGif

                              , initBoolState 
                              , initBoolStateJpg
                              , execBoolReader
                              , runBoolReaderWith
                              ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<*>), (<$>) )
#endif

import Data.STRef
import Control.Monad( when )
import Control.Monad.ST( ST )
import qualified Control.Monad.Trans.State.Strict as S
import Data.Int ( Int32 )
import Data.Word( Word8, Word32 )
import Data.Bits( (.&.), (.|.), unsafeShiftR, unsafeShiftL )

import Codec.Picture.VectorByteConversion( blitVector )
import qualified Data.Vector.Storable.Mutable as M
import qualified Data.Vector.Storable as VS
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L


--------------------------------------------------
----            Reader
--------------------------------------------------
-- | Current bit index, current value, string
data BoolState = BoolState {-# UNPACK #-} !Int
                           {-# UNPACK #-} !Word8
                           !B.ByteString

emptyBoolState :: BoolState
emptyBoolState = BoolState (-1) 0 B.empty

-- | Type used to read bits
type BoolReader s a = S.StateT BoolState (ST s) a

runBoolReader :: BoolReader s a -> ST s a
runBoolReader action = S.evalStateT action $ BoolState 0 0 B.empty

runBoolReaderWith :: BoolState -> BoolReader s a -> ST s (a, BoolState)
runBoolReaderWith st action = S.runStateT action st

execBoolReader :: BoolState -> BoolReader s a -> ST s BoolState
execBoolReader st reader = S.execStateT reader st

initBoolState :: B.ByteString -> BoolState
initBoolState str = case B.uncons str of
     Nothing -> BoolState 0 0 B.empty
     Just (v, rest) -> BoolState 0 v rest

initBoolStateJpg :: B.ByteString -> BoolState
initBoolStateJpg str = 
   case B.uncons str of
     Nothing -> BoolState 0 0 B.empty
     Just (0xFF, rest) -> case B.uncons rest of
            Nothing                  -> BoolState 7 0 B.empty
            Just (0x00, afterMarker) -> BoolState 7 0xFF afterMarker
            Just (_   , afterMarker) -> initBoolStateJpg afterMarker
     Just (v, rest) -> BoolState 7 v rest

-- | Bitify a list of things to decode.
setDecodedString :: B.ByteString -> BoolReader s ()
setDecodedString str = case B.uncons str of
     Nothing        -> S.put $ BoolState      0 0 B.empty
     Just (v, rest) -> S.put $ BoolState      0 v    rest

-- | Drop all bit until the bit of indice 0, usefull to parse restart
-- marker, as they are byte aligned, but Huffman might not.
byteAlignJpg :: BoolReader s ()
byteAlignJpg = do
  BoolState idx _ chain <- S.get
  when (idx /= 7) (setDecodedStringJpg chain)

getNextBitJpg :: BoolReader s Bool
{-# INLINE getNextBitJpg #-}
getNextBitJpg = do
    BoolState idx v chain <- S.get
    let val = (v .&. (1 `unsafeShiftL` idx)) /= 0
    if idx == 0
      then setDecodedStringJpg chain
      else S.put $ BoolState (idx - 1) v chain
    return val

getNextIntJpg :: Int -> BoolReader s Int32
{-# INLINE getNextIntJpg #-}
getNextIntJpg = go 0 where
  go !acc !0 = return acc
  go !acc !n = do
    BoolState idx v chain <- S.get
    let !leftBits = 1 + fromIntegral idx
    if n >= leftBits then do
      setDecodedStringJpg chain
      let !remaining = n - leftBits
          !mask = (1 `unsafeShiftL` leftBits) - 1
          !finalV = fromIntegral v .&. mask
          !theseBits = finalV `unsafeShiftL` remaining
      go (acc .|. theseBits) remaining
    else do
      let !remaining = leftBits - n
          !mask = (1 `unsafeShiftL` n) - 1
          !finalV = fromIntegral v `unsafeShiftR` remaining
      S.put $ BoolState (fromIntegral remaining - 1) v chain
      return $ (finalV .&. mask) .|. acc


setDecodedStringMSB :: B.ByteString -> BoolReader s ()
setDecodedStringMSB str = case B.uncons str of
  Nothing        -> S.put $ BoolState      8 0 B.empty
  Just (v, rest) -> S.put $ BoolState      8 v    rest


{-# INLINE getNextBitsMSBFirst #-}
getNextBitsMSBFirst :: Int -> BoolReader s Word32
getNextBitsMSBFirst requested = go 0 requested where
  go :: Word32 -> Int -> BoolReader s Word32
  go !acc !0 = return acc
  go !acc !n = do
    BoolState idx v chain <- S.get
    let !leftBits = fromIntegral idx
    if n >= leftBits then do
      setDecodedStringMSB chain
      let !theseBits = fromIntegral v `unsafeShiftL` (n - leftBits)
      go (acc .|. theseBits) (n - leftBits)
    else do
      let !remaining = leftBits - n
          !mask = (1 `unsafeShiftL` remaining) - 1
      S.put $ BoolState (fromIntegral remaining) (v .&. mask) chain
      return $ (fromIntegral v `unsafeShiftR` remaining) .|. acc

{-# INLINE getNextBitsLSBFirst #-}
getNextBitsLSBFirst :: Int -> BoolReader s Word32
getNextBitsLSBFirst count = aux 0 count
  where aux acc 0 = return acc
        aux acc n = do
            bit <- getNextBit
            let nextVal | bit = acc .|. (1 `unsafeShiftL` (count - n))
                        | otherwise = acc
            aux nextVal (n - 1)

{-# INLINE getNextBit #-}
getNextBit :: BoolReader s Bool
getNextBit = do
    BoolState idx v chain <- S.get
    let val = (v .&. (1 `unsafeShiftL` idx)) /= 0
    if idx == 7
      then setDecodedString chain
      else S.put $ BoolState (idx + 1) v chain
    return val

-- | Bitify a list of things to decode. Handle Jpeg escape
-- code (0xFF 0x00), thus should be only used in JPEG decoding.
setDecodedStringJpg :: B.ByteString -> BoolReader s ()
setDecodedStringJpg str = case B.uncons str of
     Nothing        -> S.put $ BoolState 7 0 B.empty
     Just (0xFF, rest) -> case B.uncons rest of
            Nothing                  -> S.put $ BoolState 7 0 B.empty
            Just (0x00, afterMarker) -> -- trace "00" $ 
                S.put $ BoolState 7 0xFF afterMarker
            Just (_   , afterMarker) -> setDecodedStringJpg afterMarker
     Just (v, rest) ->
        S.put $ BoolState 7 v rest

--------------------------------------------------
----            Writer
--------------------------------------------------
defaultBufferSize :: Int
defaultBufferSize = 256 * 1024

data BoolWriteStateRef s = BoolWriteStateRef
        { bwsCurrBuffer   :: STRef s (M.MVector s Word8)
        , bwsBufferList   :: STRef s [B.ByteString]
        , bwsWrittenWords :: STRef s Int
        , bwsBitAcc       :: STRef s Word8
        , bwsBitReaded    :: STRef s Int
        }

newWriteStateRef :: ST s (BoolWriteStateRef s)
newWriteStateRef = do
    origMv <- M.new defaultBufferSize
    BoolWriteStateRef <$> newSTRef origMv
                      <*> newSTRef []
                      <*> newSTRef 0
                      <*> newSTRef 0
                      <*> newSTRef 0

finalizeBoolWriter :: BoolWriteStateRef s -> ST s L.ByteString
finalizeBoolWriter st = do
    flushLeftBits' st
    forceBufferFlushing' st
    L.fromChunks <$> readSTRef (bwsBufferList st)

forceBufferFlushing' :: BoolWriteStateRef s -> ST s ()
forceBufferFlushing' (BoolWriteStateRef { bwsCurrBuffer = vecRef
                                        , bwsWrittenWords = countRef
                                        , bwsBufferList = lstRef
                                        }) = do
    vec <- readSTRef vecRef
    count <- readSTRef countRef
    lst <- readSTRef lstRef

    nmv <- M.new defaultBufferSize
    str <- byteStringFromVector vec count

    writeSTRef vecRef nmv
    writeSTRef lstRef $ lst ++ [str]
    writeSTRef countRef 0

flushCurrentBuffer' :: BoolWriteStateRef s -> ST s ()
flushCurrentBuffer' st = do
    count <- readSTRef $ bwsWrittenWords st
    when (count >= defaultBufferSize)
         (forceBufferFlushing' st)

byteStringFromVector :: M.MVector s Word8 -> Int -> ST s B.ByteString
byteStringFromVector vec size = do
    frozen <- VS.unsafeFreeze vec
    return $ blitVector frozen 0 size

setBitCount' :: BoolWriteStateRef s -> Word8 -> Int -> ST s ()
{-# INLINE setBitCount' #-}
setBitCount' st acc count = do
    writeSTRef (bwsBitAcc st) acc
    writeSTRef (bwsBitReaded st) count

resetBitCount' :: BoolWriteStateRef s -> ST s ()
{-# INLINE resetBitCount' #-}
resetBitCount' st = setBitCount' st 0 0

pushByte' :: BoolWriteStateRef s -> Word8 -> ST s ()
{-# INLINE pushByte' #-}
pushByte' st v = do
    flushCurrentBuffer' st
    idx <- readSTRef (bwsWrittenWords st)
    vec <- readSTRef (bwsCurrBuffer st)
    M.write vec idx v
    writeSTRef (bwsWrittenWords st) $ idx + 1

flushLeftBits' :: BoolWriteStateRef s -> ST s ()
flushLeftBits' st = do
    currCount <- readSTRef $ bwsBitReaded st
    when (currCount > 0) $ do
      currWord <- readSTRef $ bwsBitAcc st
      pushByte' st $ currWord `unsafeShiftL` (8 - currCount)

-- | Append some data bits to a Put monad.
writeBits' :: BoolWriteStateRef s
           -> Word32     -- ^ The real data to be stored. Actual data should be in the LSB
           -> Int        -- ^ Number of bit to write from 1 to 32
           -> ST s ()
{-# INLINE writeBits' #-}
writeBits' st d c = do
    currWord <- readSTRef $ bwsBitAcc st
    currCount <- readSTRef $  bwsBitReaded st
    serialize d c currWord currCount
  where dumpByte 0xFF = pushByte' st 0xFF >> pushByte' st 0x00
        dumpByte    i = pushByte' st i

        serialize bitData bitCount currentWord count
            | bitCount + count == 8 = do
                     resetBitCount' st
                     dumpByte (fromIntegral $ (currentWord `unsafeShiftL` bitCount) .|.
                                                fromIntegral cleanData)

            | bitCount + count < 8 =
                let newVal = currentWord `unsafeShiftL` bitCount
                in setBitCount' st (newVal .|. fromIntegral cleanData) $ count + bitCount

            | otherwise =
                let leftBitCount = 8 - count :: Int
                    highPart = cleanData `unsafeShiftR` (bitCount - leftBitCount) :: Word32
                    prevPart = fromIntegral currentWord `unsafeShiftL` leftBitCount :: Word32

                    nextMask = (1 `unsafeShiftL` (bitCount - leftBitCount)) - 1 :: Word32
                    newData = cleanData .&. nextMask :: Word32
                    newCount = bitCount - leftBitCount :: Int

                    toWrite = fromIntegral $ prevPart .|. highPart :: Word8
                in dumpByte toWrite >> serialize newData newCount 0 0

              where cleanMask = (1 `unsafeShiftL` bitCount) - 1 :: Word32
                    cleanData = bitData .&. cleanMask     :: Word32

-- | Append some data bits to a Put monad.
writeBitsGif :: BoolWriteStateRef s
             -> Word32     -- ^ The real data to be stored. Actual data should be in the LSB
             -> Int        -- ^ Number of bit to write from 1 to 32
             -> ST s ()
{-# INLINE writeBitsGif #-}
writeBitsGif st d c = do
    currWord <- readSTRef $ bwsBitAcc st
    currCount <- readSTRef $  bwsBitReaded st
    serialize d c currWord currCount
  where dumpByte = pushByte' st

        serialize bitData bitCount currentWord count
            | bitCount + count == 8 = do
                     resetBitCount' st
                     dumpByte (fromIntegral $ currentWord  .|.
                                                (fromIntegral cleanData `unsafeShiftL` count))

            | bitCount + count < 8 =
                let newVal = fromIntegral cleanData `unsafeShiftL` count
                in setBitCount' st (newVal .|. currentWord) $ count + bitCount

            | otherwise =
                let leftBitCount = 8 - count :: Int
                    newData = cleanData `unsafeShiftR` leftBitCount :: Word32
                    newCount = bitCount - leftBitCount :: Int
                    toWrite = fromIntegral $ fromIntegral currentWord 
                                            .|. (cleanData `unsafeShiftL` count) :: Word8
                in dumpByte toWrite >> serialize newData newCount 0 0

              where cleanMask = (1 `unsafeShiftL` bitCount) - 1 :: Word32
                    cleanData = bitData .&. cleanMask     :: Word32

{-# ANN module "HLint: ignore Reduce duplication" #-}

