{-# LANGUAGE BangPatterns, CPP #-}
module Codec.Picture.Gif.LZWEncoding( lzwEncode ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>) )
import Data.Monoid( mempty )
#endif

import Control.Monad.ST( runST )
import qualified Data.ByteString.Lazy as L
import Data.Maybe( fromMaybe )
import Data.Word( Word8 )

#if MIN_VERSION_containers(0,5,0)
import qualified Data.IntMap.Strict as I
#else
import qualified Data.IntMap as I
#endif
import qualified Data.Vector.Storable as V

import Codec.Picture.BitWriter

type Trie = I.IntMap TrieNode

data TrieNode = TrieNode
    { trieIndex :: {-# UNPACK #-} !Int
    , trieSub   :: !Trie
    }

emptyNode :: TrieNode
emptyNode = TrieNode
    { trieIndex = -1
    , trieSub = mempty
    }

initialTrie :: Trie
initialTrie = I.fromList
    [(i, emptyNode { trieIndex = i }) | i <- [0 .. 255]]

lookupUpdate :: V.Vector Word8 -> Int -> Int -> Trie -> (Int, Int, Trie)
lookupUpdate vector freeIndex firstIndex trie =
    matchUpdate $ go trie 0 firstIndex 
  where
    matchUpdate (lzwOutputIndex, nextReadIndex, sub) =
        (lzwOutputIndex, nextReadIndex, fromMaybe trie sub)

    maxi = V.length vector
    go !currentTrie !prevIndex !index
      | index >= maxi = (prevIndex, index, Nothing)
      | otherwise = case I.lookup val currentTrie of
          Just (TrieNode ix subTable) ->
              let (lzwOutputIndex, nextReadIndex, newTable) =
                        go subTable ix $ index + 1
                  tableUpdater t =
                      I.insert val (TrieNode ix t) currentTrie
              in
              (lzwOutputIndex, nextReadIndex, tableUpdater <$> newTable)

          Nothing | index == maxi -> (prevIndex, index, Nothing)
                  | otherwise -> (prevIndex, index, Just $ I.insert val newNode currentTrie)

      where val = fromIntegral $ vector `V.unsafeIndex` index
            newNode = emptyNode { trieIndex = freeIndex }

lzwEncode :: Int -> V.Vector Word8 -> L.ByteString
lzwEncode initialKeySize vec = runST $ do
    bitWriter <- newWriteStateRef 

    let updateCodeSize 12 writeIdx _
            | writeIdx == 2 ^ (12 :: Int) - 1 = do
               writeBitsGif bitWriter (fromIntegral clearCode) 12
               return (startCodeSize, firstFreeIndex, initialTrie)

        updateCodeSize codeSize writeIdx trie
            | writeIdx == 2 ^ codeSize =
                return (codeSize + 1, writeIdx + 1, trie)
            | otherwise = return (codeSize, writeIdx + 1, trie)

        go readIndex (codeSize, _, _) | readIndex >= maxi =
            writeBitsGif bitWriter (fromIntegral endOfInfo) codeSize
        go !readIndex (!codeSize, !writeIndex, !trie) = do
            let (indexToWrite, endIndex, trie') =
                    lookuper writeIndex readIndex trie
            writeBitsGif bitWriter (fromIntegral indexToWrite) codeSize
            updateCodeSize codeSize writeIndex trie'
                >>= go endIndex 

    writeBitsGif bitWriter (fromIntegral clearCode) startCodeSize
    go 0 (startCodeSize, firstFreeIndex, initialTrie)

    finalizeBoolWriter bitWriter
  where
    maxi = V.length vec

    startCodeSize = initialKeySize + 1

    clearCode = 2 ^ initialKeySize :: Int
    endOfInfo = clearCode + 1
    firstFreeIndex = endOfInfo + 1
    
    lookuper = lookupUpdate vec
