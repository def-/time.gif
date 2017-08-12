{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Module provides basic types for image manipulation in the library.
-- Defined types are used to store all of those __Juicy Pixels__
module Codec.Picture.Types( -- * Types
                            -- ** Image types
                            Image( .. )
                          , MutableImage( .. )
                          , DynamicImage( .. )
                          , PalettedImage( .. )
                          , Palette
                          , Palette'( .. )

                            -- ** Image functions
                          , createMutableImage
                          , newMutableImage
                          , freezeImage
                          , unsafeFreezeImage
                          , thawImage
                          , unsafeThawImage

                            -- ** Image Lenses
                          , Traversal
                          , imagePixels
                          , imageIPixels

                            -- ** Pixel types
                          , Pixel8
                          , Pixel16
                          , Pixel32
                          , PixelF
                          , PixelYA8( .. )
                          , PixelYA16( .. )
                          , PixelRGB8( .. )
                          , PixelRGB16( .. )
                          , PixelRGBF( .. )
                          , PixelRGBA8( .. )
                          , PixelRGBA16( .. )
                          , PixelCMYK8( .. )
                          , PixelCMYK16( .. )
                          , PixelYCbCr8( .. )
                          , PixelYCbCrK8( .. )

                          -- * Type classes
                          , ColorConvertible( .. )
                          , Pixel(..)
                          -- $graph
                          , ColorSpaceConvertible( .. )
                          , LumaPlaneExtractable( .. )
                          , TransparentPixel( .. )

                            -- * Helper functions
                          , pixelMap
                          , pixelMapXY
                          , pixelFold
                          , pixelFoldM
                          , pixelFoldMap

                          , dynamicMap
                          , dynamicPixelMap
                          , palettedToTrueColor
                          , palettedAsImage
                          , dropAlphaLayer
                          , withImage
                          , zipPixelComponent3
                          , generateImage
                          , generateFoldImage
                          , gammaCorrection
                          , toneMapping

                            -- * Color plane extraction
                          , ColorPlane ( )

                          , PlaneRed( .. )
                          , PlaneGreen( .. )
                          , PlaneBlue( .. )
                          , PlaneAlpha( .. )
                          , PlaneLuma( .. )
                          , PlaneCr( .. )
                          , PlaneCb( .. )
                          , PlaneCyan( .. )
                          , PlaneMagenta( .. )
                          , PlaneYellow( .. )
                          , PlaneBlack( .. )

                          , extractComponent
                          , unsafeExtractComponent

                            -- * Packeable writing (unsafe but faster)
                          , PackeablePixel( .. )
                          , fillImageWith
                          , readPackedPixelAt
                          , writePackedPixelAt
                          , unsafeWritePixelBetweenAt
                          ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( Monoid, mempty )
import Control.Applicative( Applicative, pure, (<*>), (<$>) )
#endif

import Data.Monoid( (<>) )
import Control.Monad( foldM, liftM, ap )
import Control.DeepSeq( NFData( .. ) )
import Control.Monad.ST( ST, runST )
import Control.Monad.Primitive ( PrimMonad, PrimState )
import Foreign.ForeignPtr( castForeignPtr )
import Foreign.Storable ( Storable )
import Data.Bits( unsafeShiftL, unsafeShiftR, (.|.), (.&.) )
import Data.Typeable ( Typeable )
import Data.Word( Word8, Word16, Word32, Word64 )
import Data.Vector.Storable ( (!) )
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M

#include "ConvGraph.hs"

-- | The main type of this package, one that most
-- functions work on, is Image.
--
-- Parameterized by the underlying pixel format it
-- forms a rigid type. If you wish to store images
-- of different or unknown pixel formats use 'DynamicImage'.
--
-- Image is essentially a rectangular pixel buffer
-- of specified width and height. The coordinates are
-- assumed to start from the upper-left corner
-- of the image, with the horizontal position first
-- and vertical second.
data Image a = Image
    { -- | Width of the image in pixels
      imageWidth  :: {-# UNPACK #-} !Int
      -- | Height of the image in pixels.
    , imageHeight :: {-# UNPACK #-} !Int

      -- | Image pixel data. To extract pixels at a given position
      -- you should use the helper functions.
      --
      -- Internally pixel data is stored as consecutively packed
      -- lines from top to bottom, scanned from left to right
      -- within individual lines, from first to last color
      -- component within each pixel.
    , imageData   :: V.Vector (PixelBaseComponent a)
    }
    deriving (Typeable)

-- | Type for the palette used in Gif & PNG files.
type Palette = Image PixelRGB8

-- | Class used to describle plane present in the pixel
-- type. If a pixel has a plane description associated,
-- you can use the plane name to extract planes independently.
class ColorPlane pixel planeToken where
    -- | Retrieve the index of the component in the
    -- given pixel type.
    toComponentIndex :: pixel -> planeToken -> Int

-- | Define the plane for the red color component
data PlaneRed = PlaneRed
    deriving (Typeable)

-- | Define the plane for the green color component
data PlaneGreen = PlaneGreen
    deriving (Typeable)

-- | Define the plane for the blue color component
data PlaneBlue = PlaneBlue
    deriving (Typeable)

-- | Define the plane for the alpha (transparency) component
data PlaneAlpha = PlaneAlpha
    deriving (Typeable)

-- | Define the plane for the luma component
data PlaneLuma = PlaneLuma
    deriving (Typeable)

-- | Define the plane for the Cr component
data PlaneCr = PlaneCr
    deriving (Typeable)

-- | Define the plane for the Cb component
data PlaneCb = PlaneCb
    deriving (Typeable)

-- | Define plane for the cyan component of the
-- CMYK color space.
data PlaneCyan = PlaneCyan
    deriving (Typeable)

-- | Define plane for the magenta component of the
-- CMYK color space.
data PlaneMagenta = PlaneMagenta
    deriving (Typeable)

-- | Define plane for the yellow component of the
-- CMYK color space.
data PlaneYellow = PlaneYellow
    deriving (Typeable)

-- | Define plane for the black component of
-- the CMYK color space.
data PlaneBlack = PlaneBlack
    deriving (Typeable)

-- | Extract a color plane from an image given a present plane in the image
-- examples:
--
-- @
--  extractRedPlane :: Image PixelRGB8 -> Image Pixel8
--  extractRedPlane = extractComponent PlaneRed
-- @
--
extractComponent :: forall px plane. ( Pixel px
                                     , Pixel (PixelBaseComponent px)
                                     , PixelBaseComponent (PixelBaseComponent px)
                                                    ~ PixelBaseComponent px
                                     , ColorPlane px plane )
                 => plane -> Image px -> Image (PixelBaseComponent px)
extractComponent plane = unsafeExtractComponent idx
    where idx = toComponentIndex (undefined :: px) plane

-- | Extract a plane of an image. Returns the requested color
-- component as a greyscale image.
--
-- If you ask for a component out of bound, the `error` function will
-- be called.
unsafeExtractComponent :: forall a
                        . ( Pixel a
                          , Pixel (PixelBaseComponent a)
                          , PixelBaseComponent (PixelBaseComponent a)
                                              ~ PixelBaseComponent a)
                       => Int     -- ^ The component index, beginning at 0 ending at (componentCount - 1)
                       -> Image a -- ^ Source image
                       -> Image (PixelBaseComponent a)
unsafeExtractComponent comp img@(Image { imageWidth = w, imageHeight = h })
  | comp >= padd = error $ "extractComponent : invalid component index ("
                         ++ show comp ++ ", max:" ++ show padd ++ ")"
  | otherwise = Image { imageWidth = w, imageHeight = h, imageData = plane }
      where plane = stride img padd comp
            padd = componentCount (undefined :: a)

-- | For any image with an alpha component (transparency),
-- drop it, returning a pure opaque image.
dropAlphaLayer :: (TransparentPixel a b) => Image a -> Image b
dropAlphaLayer = pixelMap dropTransparency

-- | Class modeling transparent pixel, should provide a method
-- to combine transparent pixels
class (Pixel a, Pixel b) => TransparentPixel a b | a -> b where
    -- | Just return the opaque pixel value
    dropTransparency :: a -> b

    -- | access the transparency (alpha layer) of a given
    -- transparent pixel type.
    getTransparency :: a -> PixelBaseComponent a
{-# DEPRECATED getTransparency "please use 'pixelOpacity' instead" #-}

instance TransparentPixel PixelRGBA8 PixelRGB8 where
    {-# INLINE dropTransparency #-}
    dropTransparency (PixelRGBA8 r g b _) = PixelRGB8 r g b
    {-# INLINE getTransparency #-}
    getTransparency (PixelRGBA8 _ _ _ a) = a

lineFold :: (Monad m) => a -> Int -> (a -> Int -> m a) -> m a
{-# INLINE lineFold #-}
lineFold initial count f = go 0 initial
  where go n acc | n >= count = return acc
        go n acc = f acc n >>= go (n + 1)

stride :: (Storable (PixelBaseComponent a))
       => Image a -> Int -> Int -> V.Vector (PixelBaseComponent a)
stride Image { imageWidth = w, imageHeight = h, imageData = array }
        padd firstComponent = runST $ do
    let cell_count = w * h
    outArray <- M.new cell_count

    let go writeIndex _ | writeIndex >= cell_count = return ()
        go writeIndex readIndex = do
          (outArray `M.unsafeWrite` writeIndex) $ array `V.unsafeIndex` readIndex
          go (writeIndex + 1) $ readIndex + padd

    go 0 firstComponent
    V.unsafeFreeze outArray

instance NFData (Image a) where
    rnf (Image width height dat) = width  `seq`
                                   height `seq`
                                   dat    `seq`
                                   ()

-- | Image or pixel buffer, the coordinates are assumed to start
-- from the upper-left corner of the image, with the horizontal
-- position first, then the vertical one. The image can be transformed in place.
data MutableImage s a = MutableImage
    { -- | Width of the image in pixels
      mutableImageWidth  :: {-# UNPACK #-} !Int

      -- | Height of the image in pixels.
    , mutableImageHeight :: {-# UNPACK #-} !Int

      -- | The real image, to extract pixels at some position
      -- you should use the helpers functions.
    , mutableImageData   :: M.STVector s (PixelBaseComponent a)
    }
    deriving (Typeable)

-- | `O(n)` Yield an immutable copy of an image by making a copy of it
freezeImage :: (Storable (PixelBaseComponent px), PrimMonad m)
            => MutableImage (PrimState m) px -> m (Image px)
freezeImage (MutableImage w h d) = Image w h `liftM` V.freeze d

-- | `O(n)` Yield a mutable copy of an image by making a copy of it.
thawImage :: (Storable (PixelBaseComponent px), PrimMonad m)
          => Image px -> m (MutableImage (PrimState m) px)
thawImage (Image w h d) = MutableImage w h `liftM` V.thaw d

-- | `O(1)` Unsafe convert an imutable image to an mutable one without copying.
-- The source image shouldn't be used after this operation.
unsafeThawImage :: (Storable (PixelBaseComponent px), PrimMonad m)
                => Image px -> m (MutableImage (PrimState m) px)
{-# NOINLINE unsafeThawImage #-}
unsafeThawImage (Image w h d) = MutableImage w h `liftM` V.unsafeThaw d

-- | `O(1)` Unsafe convert a mutable image to an immutable one without copying.
-- The mutable image may not be used after this operation.
unsafeFreezeImage ::  (Storable (PixelBaseComponent a), PrimMonad m)
                  => MutableImage (PrimState m) a -> m (Image a)
unsafeFreezeImage (MutableImage w h d) = Image w h `liftM` V.unsafeFreeze d

-- | Create a mutable image, filled with the given background color.
createMutableImage :: (Pixel px, PrimMonad m)
                   => Int -- ^ Width
                   -> Int -- ^ Height
                   -> px  -- ^ Background color
                   -> m (MutableImage (PrimState m) px)
createMutableImage width height background =
   generateMutableImage (\_ _ -> background) width height

-- | Create a mutable image with garbage as content. All data
-- is uninitialized.
newMutableImage :: forall px m. (Pixel px, PrimMonad m)
                => Int -- ^ Width
                -> Int -- ^ Height
                -> m (MutableImage (PrimState m) px)
newMutableImage w h = MutableImage w h `liftM` M.new (w * h * compCount)
  where compCount = componentCount (undefined :: px)

instance NFData (MutableImage s a) where
    rnf (MutableImage width height dat) = width  `seq`
                                          height `seq`
                                          dat    `seq`
                                          ()

-- | Image type enumerating all predefined pixel types.
-- It enables loading and use of images of different
-- pixel types.
data DynamicImage =
       -- | A greyscale image.
       ImageY8    (Image Pixel8)
       -- | A greyscale image with 16bit components
     | ImageY16   (Image Pixel16)
       -- | A greyscale HDR image
     | ImageYF    (Image PixelF)
       -- | An image in greyscale with an alpha channel.
     | ImageYA8   (Image PixelYA8)
      -- | An image in greyscale with alpha channel on 16 bits.
     | ImageYA16  (Image PixelYA16)
       -- | An image in true color.
     | ImageRGB8  (Image PixelRGB8)
       -- | An image in true color with 16bit depth.
     | ImageRGB16 (Image PixelRGB16)
       -- | An image with HDR pixels
     | ImageRGBF  (Image PixelRGBF)
       -- | An image in true color and an alpha channel.
     | ImageRGBA8 (Image PixelRGBA8)
       -- | A true color image with alpha on 16 bits.
     | ImageRGBA16 (Image PixelRGBA16)
       -- | An image in the colorspace used by Jpeg images.
     | ImageYCbCr8 (Image PixelYCbCr8)
       -- | An image in the colorspace CMYK
     | ImageCMYK8  (Image PixelCMYK8)
       -- | An image in the colorspace CMYK and 16 bits precision
     | ImageCMYK16 (Image PixelCMYK16)
    deriving (Typeable)

-- | Type used to expose a palette extracted during reading.
-- Use palettedAsImage to convert it to a palette usable for
-- writing.
data Palette' px = Palette'
  { -- | Number of element in pixels.
    _paletteSize :: !Int
    -- | Real data used by the palette.
  , _paletteData :: !(V.Vector (PixelBaseComponent px))
  }
  deriving Typeable

-- | Convert a palette to an image. Used mainly for
-- backward compatibility.
palettedAsImage :: Palette' px -> Image px
palettedAsImage p = Image (_paletteSize p) 1 $ _paletteData p

-- | Describe an image and it's potential associated
-- palette. If no palette is present, fallback to a
-- DynamicImage
data PalettedImage
  = TrueColorImage DynamicImage -- ^ Fallback
  | PalettedY8    (Image Pixel8) (Palette' Pixel8)
  | PalettedRGB8  (Image Pixel8) (Palette' PixelRGB8)
  | PalettedRGBA8 (Image Pixel8) (Palette' PixelRGBA8)
  | PalettedRGB16 (Image Pixel8) (Palette' PixelRGB16)
  deriving (Typeable)

-- | Flatten a PalettedImage to a DynamicImage
palettedToTrueColor :: PalettedImage -> DynamicImage
palettedToTrueColor img = case img of
  TrueColorImage d -> d
  PalettedY8    i p -> ImageY8 $ toTrueColor 1 (_paletteData p) i
  PalettedRGB8  i p -> ImageRGB8 $ toTrueColor 3 (_paletteData p) i
  PalettedRGBA8 i p -> ImageRGBA8 $ toTrueColor 4 (_paletteData p) i
  PalettedRGB16 i p -> ImageRGB16 $ toTrueColor 3 (_paletteData p) i
  where 
    toTrueColor c vec = pixelMap (unsafePixelAt vec . (c *) . fromIntegral)

-- | Helper function to help extract information from dynamic
-- image. To get the width of a dynamic image, you can use
-- the following snippet:
--
-- > dynWidth :: DynamicImage -> Int
-- > dynWidth img = dynamicMap imageWidth img
--
dynamicMap :: (forall pixel . (Pixel pixel) => Image pixel -> a)
           -> DynamicImage -> a
dynamicMap f (ImageY8    i) = f i
dynamicMap f (ImageY16   i) = f i
dynamicMap f (ImageYF    i) = f i
dynamicMap f (ImageYA8   i) = f i
dynamicMap f (ImageYA16  i) = f i
dynamicMap f (ImageRGB8  i) = f i
dynamicMap f (ImageRGB16 i) = f i
dynamicMap f (ImageRGBF  i) = f i
dynamicMap f (ImageRGBA8 i) = f i
dynamicMap f (ImageRGBA16 i) = f i
dynamicMap f (ImageYCbCr8 i) = f i
dynamicMap f (ImageCMYK8 i) = f i
dynamicMap f (ImageCMYK16 i) = f i

-- | Equivalent of the `pixelMap` function for the dynamic images.
-- You can perform pixel colorspace independant operations with this
-- function.
--
-- For instance, if you want to extract a square crop of any image,
-- without caring about colorspace, you can use the following snippet.
--
-- > dynSquare :: DynamicImage -> DynamicImage
-- > dynSquare = dynamicPixelMap squareImage
-- >
-- > squareImage :: Pixel a => Image a -> Image a
-- > squareImage img = generateImage (\x y -> pixelAt img x y) edge edge
-- >    where edge = min (imageWidth img) (imageHeight img)
--
dynamicPixelMap :: (forall pixel . (Pixel pixel) => Image pixel -> Image pixel)
                -> DynamicImage -> DynamicImage
dynamicPixelMap f = aux
  where
    aux (ImageY8    i) = ImageY8 (f i)
    aux (ImageY16   i) = ImageY16 (f i)
    aux (ImageYF    i) = ImageYF (f i)
    aux (ImageYA8   i) = ImageYA8 (f i)
    aux (ImageYA16  i) = ImageYA16 (f i)
    aux (ImageRGB8  i) = ImageRGB8 (f i)
    aux (ImageRGB16 i) = ImageRGB16 (f i)
    aux (ImageRGBF  i) = ImageRGBF (f i)
    aux (ImageRGBA8 i) = ImageRGBA8 (f i)
    aux (ImageRGBA16 i) = ImageRGBA16 (f i)
    aux (ImageYCbCr8 i) = ImageYCbCr8 (f i)
    aux (ImageCMYK8 i) = ImageCMYK8 (f i)
    aux (ImageCMYK16 i) = ImageCMYK16 (f i)

instance NFData DynamicImage where
    rnf (ImageY8 img)     = rnf img
    rnf (ImageY16 img)    = rnf img
    rnf (ImageYF img)     = rnf img
    rnf (ImageYA8 img)    = rnf img
    rnf (ImageYA16 img)   = rnf img
    rnf (ImageRGB8 img)   = rnf img
    rnf (ImageRGB16 img)  = rnf img
    rnf (ImageRGBF img)   = rnf img
    rnf (ImageRGBA8 img)  = rnf img
    rnf (ImageRGBA16 img) = rnf img
    rnf (ImageYCbCr8 img) = rnf img
    rnf (ImageCMYK8 img)  = rnf img
    rnf (ImageCMYK16 img)  = rnf img

-- | Type alias for 8bit greyscale pixels. For simplicity,
-- greyscale pixels use plain numbers instead of a separate type.
type Pixel8 = Word8

-- | Type alias for 16bit greyscale pixels.
type Pixel16 = Word16

-- | Type alias for 32bit greyscale pixels.
type Pixel32 = Word32

-- | Type alias for 32bit floating point greyscale pixels. The standard
-- bounded value range is mapped to the closed interval [0,1] i.e.
--
-- > map promotePixel [0, 1 .. 255 :: Pixel8] == [0/255, 1/255 .. 1.0 :: PixelF]
type PixelF = Float

-- | Pixel type storing 8bit Luminance (Y) and alpha (A) information.
-- Values are stored in the following order:
--
--  * Luminance
--
--  * Alpha
--
data PixelYA8 = PixelYA8 {-# UNPACK #-} !Pixel8  -- Luminance
                         {-# UNPACK #-} !Pixel8  -- Alpha value
              deriving (Eq, Ord, Show, Typeable)

-- | Pixel type storing 16bit Luminance (Y) and alpha (A) information.
-- Values are stored in the following order:
--
--  * Luminance
--
--  * Alpha
--
data PixelYA16 = PixelYA16 {-# UNPACK #-} !Pixel16  -- Luminance
                           {-# UNPACK #-} !Pixel16  -- Alpha value
              deriving (Eq, Ord, Show, Typeable)

-- | Classic pixel type storing 8bit red, green and blue (RGB) information.
-- Values are stored in the following order:
--
--  * Red
--
--  * Green
--
--  * Blue
--
data PixelRGB8 = PixelRGB8 {-# UNPACK #-} !Pixel8 -- Red
                           {-# UNPACK #-} !Pixel8 -- Green
                           {-# UNPACK #-} !Pixel8 -- Blue
               deriving (Eq, Ord, Show, Typeable)

-- | Pixel type storing value for the YCCK color space:
--
-- * Y (Luminance)
--
-- * Cb
--
-- * Cr
--
-- * Black
--
data PixelYCbCrK8 = PixelYCbCrK8 {-# UNPACK #-} !Pixel8
                                 {-# UNPACK #-} !Pixel8
                                 {-# UNPACK #-} !Pixel8
                                 {-# UNPACK #-} !Pixel8
               deriving (Eq, Ord, Show, Typeable)

-- | Pixel type storing 16bit red, green and blue (RGB) information.
-- Values are stored in the following order:
--
--  * Red
--
--  * Green
--
--  * Blue
--
data PixelRGB16 = PixelRGB16 {-# UNPACK #-} !Pixel16 -- Red
                             {-# UNPACK #-} !Pixel16 -- Green
                             {-# UNPACK #-} !Pixel16 -- Blue
               deriving (Eq, Ord, Show, Typeable)

-- | HDR pixel type storing floating point 32bit red, green and blue (RGB) information.
-- Same value range and comments apply as for 'PixelF'.
-- Values are stored in the following order:
--
--  * Red
--
--  * Green
--
--  * Blue
--
data PixelRGBF = PixelRGBF {-# UNPACK #-} !PixelF -- Red
                           {-# UNPACK #-} !PixelF -- Green
                           {-# UNPACK #-} !PixelF -- Blue
               deriving (Eq, Ord, Show, Typeable)

-- | Pixel type storing 8bit luminance, blue difference and red difference (YCbCr) information.
-- Values are stored in the following order:
--
--  * Y (luminance)
--
--  * Cb
--
--  * Cr
--
data PixelYCbCr8 = PixelYCbCr8 {-# UNPACK #-} !Pixel8 -- Y luminance
                               {-# UNPACK #-} !Pixel8 -- Cb blue difference
                               {-# UNPACK #-} !Pixel8 -- Cr red difference
                 deriving (Eq, Ord, Show, Typeable)

-- | Pixel type storing 8bit cyan, magenta, yellow and black (CMYK) information.
-- Values are stored in the following order:
--
--   * Cyan
--
--   * Magenta
--
--   * Yellow
--
--   * Black
--
data PixelCMYK8 = PixelCMYK8 {-# UNPACK #-} !Pixel8 -- Cyan
                             {-# UNPACK #-} !Pixel8 -- Magenta
                             {-# UNPACK #-} !Pixel8 -- Yellow
                             {-# UNPACK #-} !Pixel8 -- Black
                 deriving (Eq, Ord, Show, Typeable)

-- | Pixel type storing 16bit cyan, magenta, yellow and black (CMYK) information.
-- Values are stored in the following order:
--
--   * Cyan
--
--   * Magenta
--
--   * Yellow
--
--   * Black
--
data PixelCMYK16 = PixelCMYK16 {-# UNPACK #-} !Pixel16 -- Cyan
                               {-# UNPACK #-} !Pixel16 -- Magenta
                               {-# UNPACK #-} !Pixel16 -- Yellow
                               {-# UNPACK #-} !Pixel16 -- Black
                 deriving (Eq, Ord, Show, Typeable)


-- | Classical pixel type storing 8bit red, green, blue and alpha (RGBA) information.
-- Values are stored in the following order:
--
--  * Red
--
--  * Green
--
--  * Blue
--
--  * Alpha
--
data PixelRGBA8 = PixelRGBA8 {-# UNPACK #-} !Pixel8 -- Red
                             {-# UNPACK #-} !Pixel8 -- Green
                             {-# UNPACK #-} !Pixel8 -- Blue
                             {-# UNPACK #-} !Pixel8 -- Alpha
                deriving (Eq, Ord, Show, Typeable)

-- | Pixel type storing 16bit red, green, blue and alpha (RGBA) information.
-- Values are stored in the following order:
--
--  * Red
--
--  * Green
--
--  * Blue
--
--  * Alpha
--
data PixelRGBA16 = PixelRGBA16 {-# UNPACK #-} !Pixel16 -- Red
                               {-# UNPACK #-} !Pixel16 -- Green
                               {-# UNPACK #-} !Pixel16 -- Blue
                               {-# UNPACK #-} !Pixel16 -- Alpha
                deriving (Eq, Ord, Show, Typeable)

-- | Definition of pixels used in images. Each pixel has a color space, and a representative
-- component (Word8 or Float).
class ( Storable (PixelBaseComponent a)
      , Num (PixelBaseComponent a), Eq a ) => Pixel a where
    -- | Type of the pixel component, "classical" images
    -- would have Word8 type as their PixelBaseComponent,
    -- HDR image would have Float for instance
    type PixelBaseComponent a :: *

    -- | Call the function for every component of the pixels.
    -- For example for RGB pixels mixWith is declared like this:
    --
    -- > mixWith f (PixelRGB8 ra ga ba) (PixelRGB8 rb gb bb) =
    -- >    PixelRGB8 (f 0 ra rb) (f 1 ga gb) (f 2 ba bb)
    --
    mixWith :: (Int -> PixelBaseComponent a -> PixelBaseComponent a -> PixelBaseComponent a)
            -> a -> a -> a

    -- | Extension of the `mixWith` which separate the treatment
    -- of the color components of the alpha value (transparency component).
    -- For pixel without alpha components, it is equivalent to mixWith.
    --
    -- > mixWithAlpha f fa (PixelRGBA8 ra ga ba aa) (PixelRGB8 rb gb bb ab) =
    -- >    PixelRGBA8 (f 0 ra rb) (f 1 ga gb) (f 2 ba bb) (fa aa ab)
    --
    mixWithAlpha :: (Int -> PixelBaseComponent a -> PixelBaseComponent a
                         -> PixelBaseComponent a)  -- ^ Function for color component
                 -> (PixelBaseComponent a -> PixelBaseComponent a
                         -> PixelBaseComponent a) -- ^ Function for alpha component
                 -> a -> a -> a
    {-# INLINE mixWithAlpha #-}
    mixWithAlpha f _ = mixWith f

    -- | Return the opacity of a pixel, if the pixel has an
    -- alpha layer, return the alpha value. If the pixel
    -- doesn't have an alpha value, return a value
    -- representing the opaqueness.
    pixelOpacity :: a -> PixelBaseComponent a

    -- | Return the number of components of the pixel
    componentCount :: a -> Int

    -- | Apply a function to each component of a pixel.
    -- If the color type possess an alpha (transparency channel),
    -- it is treated like the other color components.
    colorMap :: (PixelBaseComponent a -> PixelBaseComponent a) -> a -> a

    -- | Calculate the index for the begining of the pixel
    pixelBaseIndex :: Image a -> Int -> Int -> Int
    pixelBaseIndex (Image { imageWidth = w }) x y =
            (x + y * w) * componentCount (undefined :: a)

    -- | Calculate theindex for the begining of the pixel at position x y
    mutablePixelBaseIndex :: MutableImage s a -> Int -> Int -> Int
    mutablePixelBaseIndex (MutableImage { mutableImageWidth = w }) x y =
            (x + y * w) * componentCount (undefined :: a)

    -- | Extract a pixel at a given position, (x, y), the origin
    -- is assumed to be at the corner top left, positive y to the
    -- bottom of the image
    pixelAt :: Image a -> Int -> Int -> a

    -- | Same as pixelAt but for mutable images.
    readPixel :: PrimMonad m => MutableImage (PrimState m) a -> Int -> Int -> m a

    -- | Write a pixel in a mutable image at position x y
    writePixel :: PrimMonad m => MutableImage (PrimState m) a -> Int -> Int -> a -> m ()

    -- | Unsafe version of pixelAt, read a pixel at the given
    -- index without bound checking (if possible).
    -- The index is expressed in number (PixelBaseComponent a)
    unsafePixelAt :: V.Vector (PixelBaseComponent a) -> Int -> a

    -- | Unsafe version of readPixel,  read a pixel at the given
    -- position without bound checking (if possible). The index
    -- is expressed in number (PixelBaseComponent a)
    unsafeReadPixel :: PrimMonad m => M.STVector (PrimState m) (PixelBaseComponent a) -> Int -> m a

    -- | Unsafe version of writePixel, write a pixel at the
    -- given position without bound checking. This can be _really_ unsafe.
    -- The index is expressed in number (PixelBaseComponent a)
    unsafeWritePixel :: PrimMonad m => M.STVector (PrimState m) (PixelBaseComponent a) -> Int -> a -> m ()


-- | Implement upcasting for pixel types.
-- Minimal declaration of `promotePixel`.
-- It is strongly recommended to overload promoteImage to keep
-- performance acceptable
class (Pixel a, Pixel b) => ColorConvertible a b where
    -- | Convert a pixel type to another pixel type. This
    -- operation should never lose any data.
    promotePixel :: a -> b

    -- | Change the underlying pixel type of an image by performing a full copy
    -- of it.
    promoteImage :: Image a -> Image b
    promoteImage = pixelMap promotePixel

-- | This class abstract colorspace conversion. This
-- conversion can be lossy, which ColorConvertible cannot
class (Pixel a, Pixel b) => ColorSpaceConvertible a b where
    -- | Pass a pixel from a colorspace (say RGB) to the second one
    -- (say YCbCr)
    convertPixel :: a -> b

    -- | Helper function to convert a whole image by taking a
    -- copy it.
    convertImage :: Image a -> Image b
    convertImage = pixelMap convertPixel

generateMutableImage :: forall m px. (Pixel px, PrimMonad m)
                     => (Int -> Int -> px)  -- ^ Generating function, with `x` and `y` params.
                     -> Int        -- ^ Width in pixels
                     -> Int        -- ^ Height in pixels
                     -> m (MutableImage (PrimState m) px)
{-# INLINE generateMutableImage #-}
generateMutableImage f w h = MutableImage w h `liftM` generated where
  compCount = componentCount (undefined :: px)

  generated = do
    arr <- M.new (w * h * compCount)
    let lineGenerator _ !y | y >= h = return ()
        lineGenerator !lineIdx y = column lineIdx 0
          where column !idx !x | x >= w = lineGenerator idx $ y + 1
                column idx x = do
                    unsafeWritePixel arr idx $ f x y
                    column (idx + compCount) $ x + 1
    lineGenerator 0 0
    return arr

-- | Create an image given a function to generate pixels.
-- The function will receive values from 0 to width-1 for the x parameter
-- and 0 to height-1 for the y parameter. The coordinates 0,0 are the upper
-- left corner of the image, and (width-1, height-1) the lower right corner.
--
-- for example, to create a small gradient image:
--
-- > imageCreator :: String -> IO ()
-- > imageCreator path = writePng path $ generateImage pixelRenderer 250 300
-- >    where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128
--
generateImage :: forall px. (Pixel px)
              => (Int -> Int -> px)  -- ^ Generating function, with `x` and `y` params.
              -> Int        -- ^ Width in pixels
              -> Int        -- ^ Height in pixels
              -> Image px
{-# INLINE generateImage #-}
generateImage f w h = runST img where
  img :: ST s (Image px)
  img = generateMutableImage f w h >>= unsafeFreezeImage

-- | Create an image using a monadic initializer function.
-- The function will receive values from 0 to width-1 for the x parameter
-- and 0 to height-1 for the y parameter. The coordinates 0,0 are the upper
-- left corner of the image, and (width-1, height-1) the lower right corner.
--
-- The function is called for each pixel in the line from left to right (0 to width - 1)
-- and for each line (0 to height - 1).
withImage :: forall m pixel. (Pixel pixel, PrimMonad m)
          => Int                     -- ^ Image width
          -> Int                     -- ^ Image height
          -> (Int -> Int -> m pixel) -- ^ Generating functions
          -> m (Image pixel)
withImage width height pixelGenerator = do
  let pixelComponentCount = componentCount (undefined :: pixel)
  arr <- M.new (width * height * pixelComponentCount)
  let mutImage = MutableImage
        { mutableImageWidth = width
        , mutableImageHeight = height
        , mutableImageData = arr
        }

  let pixelPositions = [(x, y) | y <- [0 .. height-1], x <- [0..width-1]]
  sequence_ [pixelGenerator x y >>= unsafeWritePixel arr idx
                        | ((x,y), idx) <- zip pixelPositions [0, pixelComponentCount ..]]
  unsafeFreezeImage mutImage

-- | Create an image given a function to generate pixels.
-- The function will receive values from 0 to width-1 for the x parameter
-- and 0 to height-1 for the y parameter. The coordinates 0,0 are the upper
-- left corner of the image, and (width-1, height-1) the lower right corner.
--
-- the acc parameter is a user defined one.
--
-- The function is called for each pixel in the line from left to right (0 to width - 1)
-- and for each line (0 to height - 1).
generateFoldImage :: forall a acc. (Pixel a)
                  => (acc -> Int -> Int -> (acc, a)) -- ^ Function taking the state, x and y
                  -> acc        -- ^ Initial state
                  -> Int        -- ^ Width in pixels
                  -> Int        -- ^ Height in pixels
                  -> (acc, Image a)
generateFoldImage f intialAcc w h =
 (finalState, Image { imageWidth = w, imageHeight = h, imageData = generated })
  where compCount = componentCount (undefined :: a)
        (finalState, generated) = runST $ do
            arr <- M.new (w * h * compCount)
            let mutImage = MutableImage {
                                mutableImageWidth = w,
                                mutableImageHeight = h,
                                mutableImageData = arr }
            foldResult <- foldM (\acc (x,y) -> do
                    let (acc', px) = f acc x y
                    writePixel mutImage x y px
                    return acc') intialAcc [(x,y) | y <- [0 .. h-1], x <- [0 .. w-1]]

            frozen <- V.unsafeFreeze arr
            return (foldResult, frozen)

-- | Fold over the pixel of an image with a raster scan order:
-- from top to bottom, left to right
{-# INLINE pixelFold #-}
pixelFold :: forall acc pixel. (Pixel pixel)
          => (acc -> Int -> Int -> pixel -> acc) -> acc -> Image pixel -> acc
pixelFold f initialAccumulator img@(Image { imageWidth = w, imageHeight = h }) =
  columnFold 0 initialAccumulator 0
    where
      !compCount = componentCount (undefined :: pixel)
      !vec = imageData img

      lfold !y acc !x !idx
        | x >= w = columnFold (y + 1) acc idx
        | otherwise = 
            lfold y (f acc x y $ unsafePixelAt vec idx) (x + 1) (idx + compCount)

      columnFold !y lineAcc !readIdx
        | y >= h = lineAcc
        | otherwise = lfold y lineAcc 0 readIdx

-- | Fold over the pixel of an image with a raster scan order:
-- from top to bottom, left to right, carrying out a state
pixelFoldM :: (Pixel pixel, Monad m)
           => (acc -> Int -> Int -> pixel -> m acc) -- ^ monadic mapping function
           -> acc                              -- ^ Initial state
           -> Image pixel                       -- ^ Image to fold over
           -> m acc
{-# INLINE pixelFoldM  #-}
pixelFoldM action initialAccumulator img@(Image { imageWidth = w, imageHeight = h }) =
  lineFold initialAccumulator h columnFold
    where
      pixelFolder y acc x = action acc x y $ pixelAt img x y
      columnFold lineAcc y = lineFold lineAcc w (pixelFolder y)


-- | Fold over the pixel of an image with a raster scan order:
-- from top to bottom, left to right. This functions is analog
-- to the foldMap from the 'Foldable' typeclass, but due to the
-- Pixel constraint, Image cannot be made an instance of it.
pixelFoldMap :: forall m px. (Pixel px, Monoid m) => (px -> m) -> Image px -> m
pixelFoldMap f Image { imageWidth = w, imageHeight = h, imageData = vec } = folder 0
  where
    compCount = componentCount (undefined :: px)
    maxi = w * h * compCount

    folder idx | idx >= maxi = mempty
    folder idx = f (unsafePixelAt vec idx) <> folder (idx + compCount)

-- | `map` equivalent for an image, working at the pixel level.
-- Little example : a brightness function for an rgb image
--
-- > brightnessRGB8 :: Int -> Image PixelRGB8 -> Image PixelRGB8
-- > brightnessRGB8 add = pixelMap brightFunction
-- >      where up v = fromIntegral (fromIntegral v + add)
-- >            brightFunction (PixelRGB8 r g b) =
-- >                    PixelRGB8 (up r) (up g) (up b)
--
pixelMap :: forall a b. (Pixel a, Pixel b)
         => (a -> b) -> Image a -> Image b
{-# SPECIALIZE INLINE pixelMap :: (PixelYCbCr8 -> PixelRGB8) -> Image PixelYCbCr8 -> Image PixelRGB8 #-}
{-# SPECIALIZE INLINE pixelMap :: (PixelRGB8 -> PixelYCbCr8) -> Image PixelRGB8 -> Image PixelYCbCr8 #-}
{-# SPECIALIZE INLINE pixelMap :: (PixelRGB8 -> PixelRGB8) -> Image PixelRGB8 -> Image PixelRGB8 #-}
{-# SPECIALIZE INLINE pixelMap :: (PixelRGB8 -> PixelRGBA8) -> Image PixelRGB8 -> Image PixelRGBA8 #-}
{-# SPECIALIZE INLINE pixelMap :: (PixelRGBA8 -> PixelRGBA8) -> Image PixelRGBA8 -> Image PixelRGBA8 #-}
{-# SPECIALIZE INLINE pixelMap :: (Pixel8 -> PixelRGB8) -> Image Pixel8 -> Image PixelRGB8 #-}
{-# SPECIALIZE INLINE pixelMap :: (Pixel8 -> Pixel8) -> Image Pixel8 -> Image Pixel8 #-}
pixelMap f Image { imageWidth = w, imageHeight = h, imageData = vec } =
  Image w h pixels
    where sourceComponentCount = componentCount (undefined :: a)
          destComponentCount = componentCount (undefined :: b)

          pixels = runST $ do
            newArr <- M.new (w * h * destComponentCount)
            let lineMapper _ _ y | y >= h = return ()
                lineMapper readIdxLine writeIdxLine y = colMapper readIdxLine writeIdxLine 0
                  where colMapper readIdx writeIdx x
                            | x >= w = lineMapper readIdx writeIdx $ y + 1
                            | otherwise = do
                                unsafeWritePixel newArr writeIdx . f $ unsafePixelAt vec readIdx
                                colMapper (readIdx + sourceComponentCount)
                                          (writeIdx + destComponentCount)
                                          (x + 1)
            lineMapper 0 0 0

            -- unsafeFreeze avoids making a second copy and it will be
            -- safe because newArray can't be referenced as a mutable array
            -- outside of this where block
            V.unsafeFreeze newArr


-- | Helpers to embed a rankNTypes inside an Applicative
newtype GenST a = GenST { genAction :: forall s. ST s (M.STVector s a) }

-- | Traversal type matching the definition in the Lens package.
type Traversal s t a b =
    forall f. Applicative f => (a -> f b) -> s -> f t 

writePx :: Pixel px
        => Int -> GenST (PixelBaseComponent px) -> px -> GenST (PixelBaseComponent px)
{-# INLINE writePx #-}
writePx idx act px = GenST $ do
   vec <- genAction act
   unsafeWritePixel vec idx px
   return vec

freezeGenST :: Pixel px
            => Int -> Int -> GenST (PixelBaseComponent px) -> Image px
freezeGenST w h act =
  Image w h (runST (genAction act >>= V.unsafeFreeze))

-- | Traversal in "raster" order, from left to right the top to bottom.
-- This traversal is matching pixelMap in spirit.
--
-- Since 3.2.4
imagePixels :: forall pxa pxb. (Pixel pxa, Pixel pxb)
            => Traversal (Image pxa) (Image pxb) pxa pxb
{-# INLINE imagePixels #-}
imagePixels f Image { imageWidth = w, imageHeight = h, imageData = vec } =
  freezeGenST w h <$> pixels
  where
    sourceComponentCount = componentCount (undefined :: pxa)
    destComponentCount = componentCount (undefined :: pxb)

    maxi = w * h * sourceComponentCount
    pixels =
      go (pure $ GenST $ M.new (w * h * destComponentCount)) 0 0

    go act readIdx _ | readIdx >= maxi = act
    go act readIdx writeIdx =
      go newAct (readIdx + sourceComponentCount) (writeIdx + destComponentCount)
      where
        px = f (unsafePixelAt vec readIdx)
        newAct = writePx writeIdx <$> act <*> px

-- | Traversal providing the pixel position with it's value.
-- The traversal in raster order, from lef to right, then top
-- to bottom. The traversal match pixelMapXY in spirit.
--
-- Since 3.2.4
imageIPixels :: forall pxa pxb. (Pixel pxa, Pixel pxb)
             => Traversal (Image pxa) (Image pxb) (Int, Int, pxa) pxb
{-# INLINE imageIPixels #-}
imageIPixels f Image { imageWidth = w, imageHeight = h, imageData = vec } =
  freezeGenST w h <$> pixels
  where
    sourceComponentCount = componentCount (undefined :: pxa)
    destComponentCount = componentCount (undefined :: pxb)

    pixels =
      lineMapper (pure $ GenST $ M.new (w * h * destComponentCount)) 0 0 0

    lineMapper act _ _ y | y >= h = act
    lineMapper act readIdxLine writeIdxLine y =
        go act readIdxLine writeIdxLine 0
      where
        go cact readIdx writeIdx x
          | x >= w = lineMapper cact readIdx writeIdx $ y + 1
          | otherwise = do
             let px = f (x, y, unsafePixelAt vec readIdx)
             go (writePx writeIdx <$> cact <*> px)
                (readIdx + sourceComponentCount)
                (writeIdx + destComponentCount)
                (x + 1)

-- | Just like `pixelMap` only the function takes the pixel coordinates as
--   additional parameters.
pixelMapXY :: forall a b. (Pixel a, Pixel b)
           => (Int -> Int -> a -> b) -> Image a -> Image b
{-# SPECIALIZE INLINE pixelMapXY :: (Int -> Int -> PixelYCbCr8 -> PixelRGB8)
                                 -> Image PixelYCbCr8 -> Image PixelRGB8 #-}
{-# SPECIALIZE INLINE pixelMapXY :: (Int -> Int -> PixelRGB8 -> PixelYCbCr8)
                                 -> Image PixelRGB8 -> Image PixelYCbCr8 #-}
{-# SPECIALIZE INLINE pixelMapXY :: (Int -> Int -> PixelRGB8 -> PixelRGB8)
                                 -> Image PixelRGB8 -> Image PixelRGB8 #-}
{-# SPECIALIZE INLINE pixelMapXY :: (Int -> Int -> PixelRGB8 -> PixelRGBA8)
                                 -> Image PixelRGB8 -> Image PixelRGBA8 #-}
{-# SPECIALIZE INLINE pixelMapXY :: (Int -> Int -> PixelRGBA8 -> PixelRGBA8)
                                 -> Image PixelRGBA8 -> Image PixelRGBA8 #-}
{-# SPECIALIZE INLINE pixelMapXY :: (Int -> Int -> Pixel8 -> PixelRGB8)
                                 -> Image Pixel8 -> Image PixelRGB8 #-}
pixelMapXY f Image { imageWidth = w, imageHeight = h, imageData = vec } =
  Image w h pixels
    where sourceComponentCount = componentCount (undefined :: a)
          destComponentCount = componentCount (undefined :: b)

          pixels = runST $ do
            newArr <- M.new (w * h * destComponentCount)
            let lineMapper _ _ y | y >= h = return ()
                lineMapper readIdxLine writeIdxLine y = colMapper readIdxLine writeIdxLine 0
                  where colMapper readIdx writeIdx x
                            | x >= w = lineMapper readIdx writeIdx $ y + 1
                            | otherwise = do
                                unsafeWritePixel newArr writeIdx . f x y $ unsafePixelAt vec readIdx
                                colMapper (readIdx + sourceComponentCount)
                                          (writeIdx + destComponentCount)
                                          (x + 1)
            lineMapper 0 0 0

            -- unsafeFreeze avoids making a second copy and it will be
            -- safe because newArray can't be referenced as a mutable array
            -- outside of this where block
            V.unsafeFreeze newArr

-- | Combine, pixel by pixel and component by component
-- the values of 3 different images. Usage example:
--
-- > averageBrightNess c1 c2 c3 = clamp $ toInt c1 + toInt c2 + toInt c3
-- >   where clamp = fromIntegral . min 0 . max 255
-- >         toInt :: a -> Int
-- >         toInt = fromIntegral
-- > ziPixelComponent3 averageBrightNess img1 img2 img3
--
zipPixelComponent3
    :: forall px. ( V.Storable (PixelBaseComponent px))
    => (PixelBaseComponent px -> PixelBaseComponent px -> PixelBaseComponent px
            -> PixelBaseComponent px)
    -> Image px -> Image px -> Image px -> Image px
{-# INLINE zipPixelComponent3 #-}
zipPixelComponent3 f i1@(Image { imageWidth = w, imageHeight = h }) i2 i3
  | not isDimensionEqual = error "Different image size zipPairwisePixelComponent"
  | otherwise = Image { imageWidth = w
                      , imageHeight = h
                      , imageData = V.zipWith3 f data1 data2 data3
                      }
       where data1 = imageData i1
             data2 = imageData i2
             data3 = imageData i3

             isDimensionEqual =
                 w == imageWidth i2 && w == imageWidth i3 &&
                     h == imageHeight i2 && h == imageHeight i3

-- | Helper class to help extract a luma plane out
-- of an image or a pixel
class (Pixel a, Pixel (PixelBaseComponent a)) => LumaPlaneExtractable a where
    -- | Compute the luminance part of a pixel
    computeLuma      :: a -> PixelBaseComponent a

    -- | Extract a luma plane out of an image. This
    -- method is in the typeclass to help performant
    -- implementation.
    --
    -- > jpegToGrayScale :: FilePath -> FilePath -> IO ()
    -- > jpegToGrayScale source dest
    extractLumaPlane :: Image a -> Image (PixelBaseComponent a)
    extractLumaPlane = pixelMap computeLuma

instance LumaPlaneExtractable Pixel8 where
    {-# INLINE computeLuma #-}
    computeLuma = id
    extractLumaPlane = id

instance LumaPlaneExtractable Pixel16 where
    {-# INLINE computeLuma #-}
    computeLuma = id
    extractLumaPlane = id

instance LumaPlaneExtractable Pixel32 where
    {-# INLINE computeLuma #-}
    computeLuma = id
    extractLumaPlane = id

instance LumaPlaneExtractable PixelF where
    {-# INLINE computeLuma #-}
    computeLuma = id
    extractLumaPlane = id

instance LumaPlaneExtractable PixelRGBF where
    {-# INLINE computeLuma #-}
    computeLuma (PixelRGBF r g b) =
        0.3 * r + 0.59 * g + 0.11 * b

instance LumaPlaneExtractable PixelRGBA8 where
    {-# INLINE computeLuma #-}
    computeLuma (PixelRGBA8 r g b _) =
       floor $ (0.3 :: Double) * fromIntegral r
             + 0.59 * fromIntegral g
             + 0.11 * fromIntegral b

instance LumaPlaneExtractable PixelYCbCr8 where
    {-# INLINE computeLuma #-}
    computeLuma (PixelYCbCr8 y _ _) = y
    extractLumaPlane = extractComponent PlaneLuma

-- | Free promotion for identic pixel types
instance (Pixel a) => ColorConvertible a a where
    {-# INLINE promotePixel #-}
    promotePixel = id

    {-# INLINE promoteImage #-}
    promoteImage = id

--------------------------------------------------
----            Pixel8 instances
--------------------------------------------------
instance Pixel Pixel8 where
    type PixelBaseComponent Pixel8 = Word8

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f = f 0

    {-# INLINE colorMap #-}
    colorMap f = f

    {-# INLINE componentCount #-}
    componentCount _ = 1

    {-# INLINE pixelAt #-}
    pixelAt (Image { imageWidth = w, imageData = arr }) x y = arr ! (x + y * w)

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.read` mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.write` mutablePixelBaseIndex image x y

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt = V.unsafeIndex
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel = M.unsafeRead
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel = M.unsafeWrite

instance ColorConvertible Pixel8 PixelYA8 where
    {-# INLINE promotePixel #-}
    promotePixel c = PixelYA8 c 255

instance ColorConvertible Pixel8 PixelF where
    {-# INLINE promotePixel #-}
    promotePixel c = fromIntegral c / 255.0

instance ColorConvertible Pixel8 Pixel16 where
    {-# INLINE promotePixel #-}
    promotePixel c = fromIntegral c * 257

instance ColorConvertible Pixel8 PixelRGB8 where
    {-# INLINE promotePixel #-}
    promotePixel c = PixelRGB8 c c c

instance ColorConvertible Pixel8 PixelRGBA8 where
    {-# INLINE promotePixel #-}
    promotePixel c = PixelRGBA8 c c c 255

--------------------------------------------------
----            Pixel16 instances
--------------------------------------------------
instance Pixel Pixel16 where
    type PixelBaseComponent Pixel16 = Word16

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f = f 0

    {-# INLINE colorMap #-}
    colorMap f = f

    {-# INLINE componentCount #-}
    componentCount _ = 1
    {-# INLINE pixelAt #-}
    pixelAt (Image { imageWidth = w, imageData = arr }) x y = arr ! (x + y * w)

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.read` mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.write` mutablePixelBaseIndex image x y

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt = V.unsafeIndex
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel = M.unsafeRead
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel = M.unsafeWrite

instance ColorConvertible Pixel16 PixelYA16 where
    {-# INLINE promotePixel #-}
    promotePixel c = PixelYA16 c maxBound

instance ColorConvertible Pixel16 PixelRGB16 where
    {-# INLINE promotePixel #-}
    promotePixel c = PixelRGB16 c c c

instance ColorConvertible Pixel16 PixelRGBA16 where
    {-# INLINE promotePixel #-}
    promotePixel c = PixelRGBA16 c c c maxBound

--------------------------------------------------
----            Pixel32 instances
--------------------------------------------------
instance Pixel Pixel32 where
    type PixelBaseComponent Pixel32 = Word32

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f = f 0

    {-# INLINE colorMap #-}
    colorMap f = f

    {-# INLINE componentCount #-}
    componentCount _ = 1

    {-# INLINE pixelAt #-}
    pixelAt (Image { imageWidth = w, imageData = arr }) x y = arr ! (x + y * w)

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.read` mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.write` mutablePixelBaseIndex image x y

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt = V.unsafeIndex
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel = M.unsafeRead
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel = M.unsafeWrite

--------------------------------------------------
----            PixelF instances
--------------------------------------------------
instance Pixel PixelF where
    type PixelBaseComponent PixelF = Float

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const 1.0

    {-# INLINE mixWith #-}
    mixWith f = f 0

    {-# INLINE colorMap #-}
    colorMap f = f
    {-# INLINE componentCount #-}
    componentCount _ = 1
    {-# INLINE pixelAt #-}
    pixelAt (Image { imageWidth = w, imageData = arr }) x y =
        arr ! (x + y * w)

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.read` mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y =
        arr `M.write` mutablePixelBaseIndex image x y

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt = V.unsafeIndex
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel = M.unsafeRead
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel = M.unsafeWrite

instance ColorConvertible PixelF PixelRGBF where
    {-# INLINE promotePixel #-}
    promotePixel c = PixelRGBF c c c-- (c / 0.3) (c / 0.59)  (c / 0.11)

--------------------------------------------------
----            PixelYA8 instances
--------------------------------------------------
instance Pixel PixelYA8 where
    type PixelBaseComponent PixelYA8 = Word8

    {-# INLINE pixelOpacity #-}
    pixelOpacity (PixelYA8 _ a) = a

    {-# INLINE mixWith #-}
    mixWith f (PixelYA8 ya aa) (PixelYA8 yb ab) =
        PixelYA8 (f 0 ya yb) (f 1 aa ab)


    {-# INLINE colorMap #-}
    colorMap f (PixelYA8 y a) = PixelYA8 (f y) (f a)
    {-# INLINE componentCount #-}
    componentCount _ = 2
    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y =
        PixelYA8 (arr ! (baseIdx + 0)) (arr ! (baseIdx + 1))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        yv <- arr `M.read` baseIdx
        av <- arr `M.read` (baseIdx + 1)
        return $ PixelYA8 yv av
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelYA8 yv av) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) yv
        (arr `M.write` (baseIdx + 1)) av

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelYA8 (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1)
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        PixelYA8 `liftM` M.unsafeRead vec idx `ap` M.unsafeRead vec (idx + 1)
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelYA8 y a) =
        M.unsafeWrite v idx y >> M.unsafeWrite v (idx + 1) a

instance ColorConvertible PixelYA8 PixelRGB8 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelYA8 y _) = PixelRGB8 y y y

instance ColorConvertible PixelYA8 PixelRGBA8 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelYA8 y a) = PixelRGBA8 y y y a

instance ColorPlane PixelYA8 PlaneLuma where
    toComponentIndex _ _ = 0

instance ColorPlane PixelYA8 PlaneAlpha where
    toComponentIndex _ _ = 1

instance TransparentPixel PixelYA8 Pixel8 where
    {-# INLINE dropTransparency #-}
    dropTransparency (PixelYA8 y _) = y
    {-# INLINE getTransparency #-}
    getTransparency (PixelYA8 _ a) = a

instance LumaPlaneExtractable PixelYA8 where
    {-# INLINE computeLuma #-}
    computeLuma (PixelYA8 y _) = y
    extractLumaPlane = extractComponent PlaneLuma

--------------------------------------------------
----            PixelYA16 instances
--------------------------------------------------
instance Pixel PixelYA16 where
    type PixelBaseComponent PixelYA16 = Word16

    {-# INLINE pixelOpacity #-}
    pixelOpacity (PixelYA16 _ a) = a

    {-# INLINE mixWith #-}
    mixWith f (PixelYA16 ya aa) (PixelYA16 yb ab) =
        PixelYA16 (f 0 ya yb) (f 1 aa ab)

    {-# INLINE mixWithAlpha #-}
    mixWithAlpha f fa (PixelYA16 ya aa) (PixelYA16 yb ab) =
        PixelYA16 (f 0 ya yb) (fa aa ab)

    {-# INLINE colorMap #-}
    colorMap f (PixelYA16 y a) = PixelYA16 (f y) (f a)
    {-# INLINE componentCount #-}
    componentCount _ = 2
    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y = PixelYA16 (arr ! (baseIdx + 0))
                                                              (arr ! (baseIdx + 1))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        yv <- arr `M.read` baseIdx
        av <- arr `M.read` (baseIdx + 1)
        return $ PixelYA16 yv av
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelYA16 yv av) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) yv
        (arr `M.write` (baseIdx + 1)) av

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelYA16 (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1)
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        PixelYA16 `liftM` M.unsafeRead vec idx `ap` M.unsafeRead vec (idx + 1)
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelYA16 y a) =
        M.unsafeWrite v idx y >> M.unsafeWrite v (idx + 1) a

instance ColorConvertible PixelYA16 PixelRGBA16 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelYA16 y a) = PixelRGBA16 y y y a

instance ColorPlane PixelYA16 PlaneLuma where
    toComponentIndex _ _ = 0

instance ColorPlane PixelYA16 PlaneAlpha where
    toComponentIndex _ _ = 1

instance TransparentPixel PixelYA16 Pixel16 where
    {-# INLINE dropTransparency #-}
    dropTransparency (PixelYA16 y _) = y
    {-# INLINE getTransparency #-}
    getTransparency (PixelYA16 _ a) = a

--------------------------------------------------
----            PixelRGBF instances
--------------------------------------------------
instance Pixel PixelRGBF where
    type PixelBaseComponent PixelRGBF = PixelF

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const 1.0

    {-# INLINE mixWith #-}
    mixWith f (PixelRGBF ra ga ba) (PixelRGBF rb gb bb) =
        PixelRGBF (f 0 ra rb) (f 1 ga gb) (f 2 ba bb)

    {-# INLINE colorMap #-}
    colorMap f (PixelRGBF r g b) = PixelRGBF (f r) (f g) (f b)

    {-# INLINE componentCount #-}
    componentCount _ = 3

    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y = PixelRGBF (arr ! (baseIdx + 0))
                                                              (arr ! (baseIdx + 1))
                                                              (arr ! (baseIdx + 2))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr `M.read` baseIdx
        gv <- arr `M.read` (baseIdx + 1)
        bv <- arr `M.read` (baseIdx + 2)
        return $ PixelRGBF rv gv bv
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelRGBF rv gv bv) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) rv
        (arr `M.write` (baseIdx + 1)) gv
        (arr `M.write` (baseIdx + 2)) bv

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelRGBF (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1) (V.unsafeIndex v $ idx + 2)
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        PixelRGBF `liftM` M.unsafeRead vec idx
                  `ap` M.unsafeRead vec (idx + 1)
                  `ap` M.unsafeRead vec (idx + 2)
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelRGBF r g b) =
        M.unsafeWrite v idx r >> M.unsafeWrite v (idx + 1) g
                              >> M.unsafeWrite v (idx + 2) b

instance ColorPlane PixelRGBF PlaneRed where
    toComponentIndex _ _ = 0

instance ColorPlane PixelRGBF PlaneGreen where
    toComponentIndex _ _ = 1

instance ColorPlane PixelRGBF PlaneBlue where
    toComponentIndex _ _ = 2

--------------------------------------------------
----            PixelRGB16 instances
--------------------------------------------------
instance Pixel PixelRGB16 where
    type PixelBaseComponent PixelRGB16 = Pixel16

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f (PixelRGB16 ra ga ba) (PixelRGB16 rb gb bb) =
        PixelRGB16 (f 0 ra rb) (f 1 ga gb) (f 2 ba bb)

    {-# INLINE colorMap #-}
    colorMap f (PixelRGB16 r g b) = PixelRGB16 (f r) (f g) (f b)

    {-# INLINE componentCount #-}
    componentCount _ = 3

    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y = PixelRGB16 (arr ! (baseIdx + 0))
                                                               (arr ! (baseIdx + 1))
                                                               (arr ! (baseIdx + 2))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr `M.read` baseIdx
        gv <- arr `M.read` (baseIdx + 1)
        bv <- arr `M.read` (baseIdx + 2)
        return $ PixelRGB16 rv gv bv
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelRGB16 rv gv bv) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) rv
        (arr `M.write` (baseIdx + 1)) gv
        (arr `M.write` (baseIdx + 2)) bv

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelRGB16 (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1) (V.unsafeIndex v $ idx + 2)
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        PixelRGB16 `liftM` M.unsafeRead vec idx
                   `ap` M.unsafeRead vec (idx + 1)
                   `ap` M.unsafeRead vec (idx + 2)
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelRGB16 r g b) =
        M.unsafeWrite v idx r >> M.unsafeWrite v (idx + 1) g
                              >> M.unsafeWrite v (idx + 2) b

instance ColorPlane PixelRGB16 PlaneRed where
    toComponentIndex _ _ = 0

instance ColorPlane PixelRGB16 PlaneGreen where
    toComponentIndex _ _ = 1

instance ColorPlane PixelRGB16 PlaneBlue where
    toComponentIndex _ _ = 2

instance ColorSpaceConvertible PixelRGB16 PixelCMYK16 where
    {-# INLINE convertPixel #-}
    convertPixel (PixelRGB16 r g b) = integralRGBToCMYK PixelCMYK16 (r, g, b)

instance ColorConvertible PixelRGB16 PixelRGBA16 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelRGB16 r g b) = PixelRGBA16 r g b maxBound

instance LumaPlaneExtractable PixelRGB16 where
    {-# INLINE computeLuma #-}
    computeLuma (PixelRGB16 r g b) =
        floor $ (0.3 :: Double) * fromIntegral r
              + 0.59 * fromIntegral g
              + 0.11 * fromIntegral b

--------------------------------------------------
----            PixelRGB8 instances
--------------------------------------------------
instance Pixel PixelRGB8 where
    type PixelBaseComponent PixelRGB8 = Word8

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f (PixelRGB8 ra ga ba) (PixelRGB8 rb gb bb) =
        PixelRGB8 (f 0 ra rb) (f 1 ga gb) (f 2 ba bb)

    {-# INLINE colorMap #-}
    colorMap f (PixelRGB8 r g b) = PixelRGB8 (f r) (f g) (f b)

    {-# INLINE componentCount #-}
    componentCount _ = 3

    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y = PixelRGB8 (arr ! (baseIdx + 0))
                                                              (arr ! (baseIdx + 1))
                                                              (arr ! (baseIdx + 2))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr `M.read` baseIdx
        gv <- arr `M.read` (baseIdx + 1)
        bv <- arr `M.read` (baseIdx + 2)
        return $ PixelRGB8 rv gv bv
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelRGB8 rv gv bv) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) rv
        (arr `M.write` (baseIdx + 1)) gv
        (arr `M.write` (baseIdx + 2)) bv

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelRGB8 (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1) (V.unsafeIndex v $ idx + 2)
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        PixelRGB8 `liftM` M.unsafeRead vec idx
                  `ap` M.unsafeRead vec (idx + 1)
                  `ap` M.unsafeRead vec (idx + 2)
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelRGB8 r g b) =
        M.unsafeWrite v idx r >> M.unsafeWrite v (idx + 1) g
                              >> M.unsafeWrite v (idx + 2) b

instance ColorConvertible PixelRGB8 PixelRGBA8 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelRGB8 r g b) = PixelRGBA8 r g b maxBound

instance ColorConvertible PixelRGB8 PixelRGBF where
    {-# INLINE promotePixel #-}
    promotePixel (PixelRGB8 r g b) = PixelRGBF (toF r) (toF g) (toF b)
        where toF v = fromIntegral v / 255.0

instance ColorConvertible PixelRGB8 PixelRGB16 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelRGB8 r g b) = PixelRGB16 (promotePixel r) (promotePixel g) (promotePixel b)

instance ColorConvertible PixelRGB8 PixelRGBA16 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelRGB8 r g b) = PixelRGBA16 (promotePixel r) (promotePixel g) (promotePixel b) maxBound

instance ColorPlane PixelRGB8 PlaneRed where
    toComponentIndex _ _ = 0

instance ColorPlane PixelRGB8 PlaneGreen where
    toComponentIndex _ _ = 1

instance ColorPlane PixelRGB8 PlaneBlue where
    toComponentIndex _ _ = 2

instance LumaPlaneExtractable PixelRGB8 where
    {-# INLINE computeLuma #-}
    computeLuma (PixelRGB8 r g b) =
        floor $ (0.3 :: Double) * fromIntegral r
              + 0.59 * fromIntegral g
              + 0.11 * fromIntegral b

--------------------------------------------------
----            PixelRGBA8 instances
--------------------------------------------------
instance Pixel PixelRGBA8 where
    type PixelBaseComponent PixelRGBA8 = Word8

    {-# INLINE pixelOpacity #-}
    pixelOpacity (PixelRGBA8 _ _ _ a) = a

    {-# INLINE mixWith #-}
    mixWith f (PixelRGBA8 ra ga ba aa) (PixelRGBA8 rb gb bb ab) =
        PixelRGBA8 (f 0 ra rb) (f 1 ga gb) (f 2 ba bb) (f 3 aa ab)

    {-# INLINE mixWithAlpha #-}
    mixWithAlpha f fa (PixelRGBA8 ra ga ba aa) (PixelRGBA8 rb gb bb ab) =
        PixelRGBA8 (f 0 ra rb) (f 1 ga gb) (f 2 ba bb) (fa aa ab)

    {-# INLINE colorMap #-}
    colorMap f (PixelRGBA8 r g b a) = PixelRGBA8 (f r) (f g) (f b) (f a)

    {-# INLINE componentCount #-}
    componentCount _ = 4

    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y = PixelRGBA8 (arr ! (baseIdx + 0))
                                                               (arr ! (baseIdx + 1))
                                                               (arr ! (baseIdx + 2))
                                                               (arr ! (baseIdx + 3))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr `M.read` baseIdx
        gv <- arr `M.read` (baseIdx + 1)
        bv <- arr `M.read` (baseIdx + 2)
        av <- arr `M.read` (baseIdx + 3)
        return $ PixelRGBA8 rv gv bv av
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelRGBA8 rv gv bv av) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) rv
        (arr `M.write` (baseIdx + 1)) gv
        (arr `M.write` (baseIdx + 2)) bv
        (arr `M.write` (baseIdx + 3)) av

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelRGBA8 (V.unsafeIndex v idx)
                   (V.unsafeIndex v $ idx + 1)
                   (V.unsafeIndex v $ idx + 2)
                   (V.unsafeIndex v $ idx + 3)

    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        PixelRGBA8 `liftM` M.unsafeRead vec idx
                   `ap` M.unsafeRead vec (idx + 1)
                   `ap` M.unsafeRead vec (idx + 2)
                   `ap` M.unsafeRead vec (idx + 3)

    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelRGBA8 r g b a) =
        M.unsafeWrite v idx r >> M.unsafeWrite v (idx + 1) g
                              >> M.unsafeWrite v (idx + 2) b
                              >> M.unsafeWrite v (idx + 3) a

instance ColorConvertible PixelRGBA8 PixelRGBA16 where
    {-# INLINE promotePixel #-}
    promotePixel (PixelRGBA8 r g b a) = PixelRGBA16 (promotePixel r) (promotePixel g) (promotePixel b) (promotePixel a)

instance ColorPlane PixelRGBA8 PlaneRed where
    toComponentIndex _ _ = 0

instance ColorPlane PixelRGBA8 PlaneGreen where
    toComponentIndex _ _ = 1

instance ColorPlane PixelRGBA8 PlaneBlue where
    toComponentIndex _ _ = 2

instance ColorPlane PixelRGBA8 PlaneAlpha where
    toComponentIndex _ _ = 3

--------------------------------------------------
----            PixelRGBA16 instances
--------------------------------------------------
instance Pixel PixelRGBA16 where
    type PixelBaseComponent PixelRGBA16 = Pixel16

    {-# INLINE pixelOpacity #-}
    pixelOpacity (PixelRGBA16 _ _ _ a) = a

    {-# INLINE mixWith #-}
    mixWith f (PixelRGBA16 ra ga ba aa) (PixelRGBA16 rb gb bb ab) =
        PixelRGBA16 (f 0 ra rb) (f 1 ga gb) (f 2 ba bb) (f 3 aa ab)

    {-# INLINE mixWithAlpha #-}
    mixWithAlpha f fa (PixelRGBA16 ra ga ba aa) (PixelRGBA16 rb gb bb ab) =
        PixelRGBA16 (f 0 ra rb) (f 1 ga gb) (f 2 ba bb) (fa aa ab)

    {-# INLINE colorMap #-}
    colorMap f (PixelRGBA16 r g b a) = PixelRGBA16 (f r) (f g) (f b) (f a)

    {-# INLINE componentCount #-}
    componentCount _ = 4

    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y =
                PixelRGBA16 (arr ! (baseIdx + 0)) (arr ! (baseIdx + 1))
                            (arr ! (baseIdx + 2)) (arr ! (baseIdx + 3))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr `M.read` baseIdx
        gv <- arr `M.read` (baseIdx + 1)
        bv <- arr `M.read` (baseIdx + 2)
        av <- arr `M.read` (baseIdx + 3)
        return $ PixelRGBA16 rv gv bv av
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelRGBA16 rv gv bv av) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) rv
        (arr `M.write` (baseIdx + 1)) gv
        (arr `M.write` (baseIdx + 2)) bv
        (arr `M.write` (baseIdx + 3)) av

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelRGBA16 (V.unsafeIndex v idx)
                    (V.unsafeIndex v $ idx + 1)
                    (V.unsafeIndex v $ idx + 2)
                    (V.unsafeIndex v $ idx + 3)
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        PixelRGBA16 `liftM` M.unsafeRead vec idx
                    `ap` M.unsafeRead vec (idx + 1)
                    `ap` M.unsafeRead vec (idx + 2)
                    `ap` M.unsafeRead vec (idx + 3)
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelRGBA16 r g b a) =
        M.unsafeWrite v idx r >> M.unsafeWrite v (idx + 1) g
                              >> M.unsafeWrite v (idx + 2) b
                              >> M.unsafeWrite v (idx + 3) a


instance TransparentPixel PixelRGBA16 PixelRGB16 where
    {-# INLINE dropTransparency #-}
    dropTransparency (PixelRGBA16 r g b _) = PixelRGB16 r g b
    {-# INLINE getTransparency #-}
    getTransparency (PixelRGBA16 _ _ _ a) = a

instance ColorPlane PixelRGBA16 PlaneRed where
    toComponentIndex _ _ = 0

instance ColorPlane PixelRGBA16 PlaneGreen where
    toComponentIndex _ _ = 1

instance ColorPlane PixelRGBA16 PlaneBlue where
    toComponentIndex _ _ = 2

instance ColorPlane PixelRGBA16 PlaneAlpha where
    toComponentIndex _ _ = 3

--------------------------------------------------
----            PixelYCbCr8 instances
--------------------------------------------------
instance Pixel PixelYCbCr8 where
    type PixelBaseComponent PixelYCbCr8 = Word8

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f (PixelYCbCr8 ya cba cra) (PixelYCbCr8 yb cbb crb) =
        PixelYCbCr8 (f 0 ya yb) (f 1 cba cbb) (f 2 cra crb)

    {-# INLINE colorMap #-}
    colorMap f (PixelYCbCr8 y cb cr) = PixelYCbCr8 (f y) (f cb) (f cr)
    {-# INLINE componentCount #-}
    componentCount _ = 3
    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y = PixelYCbCr8 (arr ! (baseIdx + 0))
                                                                (arr ! (baseIdx + 1))
                                                                (arr ! (baseIdx + 2))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        yv <- arr `M.read` baseIdx
        cbv <- arr `M.read` (baseIdx + 1)
        crv <- arr `M.read` (baseIdx + 2)
        return $ PixelYCbCr8 yv cbv crv
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelYCbCr8 yv cbv crv) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) yv
        (arr `M.write` (baseIdx + 1)) cbv
        (arr `M.write` (baseIdx + 2)) crv

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelYCbCr8 (V.unsafeIndex v idx) (V.unsafeIndex v $ idx + 1) (V.unsafeIndex v $ idx + 2)
    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        PixelYCbCr8 `liftM` M.unsafeRead vec idx
                    `ap` M.unsafeRead vec (idx + 1)
                    `ap` M.unsafeRead vec (idx + 2)
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelYCbCr8 y cb cr) =
        M.unsafeWrite v idx y >> M.unsafeWrite v (idx + 1) cb
                              >> M.unsafeWrite v (idx + 2) cr

instance (Pixel a) => ColorSpaceConvertible a a where
    convertPixel = id
    convertImage = id

scaleBits, oneHalf :: Int
scaleBits = 16
oneHalf = 1 `unsafeShiftL` (scaleBits - 1)

fix :: Float -> Int
fix x = floor $ x * fromIntegral ((1 :: Int) `unsafeShiftL` scaleBits) + 0.5


rYTab, gYTab, bYTab, rCbTab, gCbTab, bCbTab, gCrTab, bCrTab :: V.Vector Int
rYTab = V.fromListN 256 [fix 0.29900 * i | i <- [0..255] ]
gYTab = V.fromListN 256 [fix 0.58700 * i | i <- [0..255] ]
bYTab = V.fromListN 256 [fix 0.11400 * i + oneHalf | i <- [0..255] ]
rCbTab = V.fromListN 256 [(- fix 0.16874) * i | i <- [0..255] ]
gCbTab = V.fromListN 256 [(- fix 0.33126) * i | i <- [0..255] ]
bCbTab = V.fromListN 256 [fix 0.5 * i + (128 `unsafeShiftL` scaleBits) + oneHalf - 1| i <- [0..255] ]
gCrTab = V.fromListN 256 [(- fix 0.41869) * i | i <- [0..255] ]
bCrTab = V.fromListN 256 [(- fix 0.08131) * i | i <- [0..255] ]


instance ColorSpaceConvertible PixelRGB8 PixelYCbCr8 where
    {-# INLINE convertPixel #-}
    convertPixel (PixelRGB8 r g b) = PixelYCbCr8 (fromIntegral y) (fromIntegral cb) (fromIntegral cr)
      where ri = fromIntegral r
            gi = fromIntegral g
            bi = fromIntegral b

            y  = (rYTab `V.unsafeIndex` ri + gYTab `V.unsafeIndex` gi + bYTab `V.unsafeIndex` bi) `unsafeShiftR` scaleBits
            cb = (rCbTab `V.unsafeIndex` ri + gCbTab `V.unsafeIndex` gi + bCbTab `V.unsafeIndex` bi) `unsafeShiftR` scaleBits
            cr = (bCbTab `V.unsafeIndex` ri + gCrTab `V.unsafeIndex` gi + bCrTab `V.unsafeIndex` bi) `unsafeShiftR` scaleBits

    convertImage Image { imageWidth = w, imageHeight = h, imageData = d } = Image w h newData
        where maxi = w * h

              rY  = fix 0.29900
              gY  = fix 0.58700
              bY  = fix 0.11400
              rCb = - fix 0.16874
              gCb = - fix 0.33126
              bCb = fix 0.5
              gCr = - fix 0.41869
              bCr = - fix 0.08131

              newData = runST $ do
                block <- M.new $ maxi * 3
                let traductor _ idx | idx >= maxi = return block
                    traductor readIdx idx = do
                        let ri = fromIntegral $ d `V.unsafeIndex` readIdx
                            gi = fromIntegral $ d `V.unsafeIndex` (readIdx + 1)
                            bi = fromIntegral $ d `V.unsafeIndex` (readIdx + 2)

                            y  = (rY * ri + gY * gi + bY * bi + oneHalf) `unsafeShiftR` scaleBits
                            cb = (rCb * ri + gCb * gi + bCb * bi + (128 `unsafeShiftL` scaleBits) + oneHalf - 1) `unsafeShiftR` scaleBits
                            cr = (bCb * ri + (128 `unsafeShiftL` scaleBits) + oneHalf - 1+ gCr * gi + bCr * bi) `unsafeShiftR` scaleBits

                        (block `M.unsafeWrite` (readIdx + 0)) $ fromIntegral y
                        (block `M.unsafeWrite` (readIdx + 1)) $ fromIntegral cb
                        (block `M.unsafeWrite` (readIdx + 2)) $ fromIntegral cr
                        traductor (readIdx + 3) (idx + 1)

                traductor 0 0 >>= V.freeze

crRTab, cbBTab, crGTab, cbGTab :: V.Vector Int
crRTab = V.fromListN 256 [(fix 1.40200 * x + oneHalf) `unsafeShiftR` scaleBits | x <- [-128 .. 127]]
cbBTab = V.fromListN 256 [(fix 1.77200 * x + oneHalf) `unsafeShiftR` scaleBits | x <- [-128 .. 127]]
crGTab = V.fromListN 256 [negate (fix 0.71414) * x | x <- [-128 .. 127]]
cbGTab = V.fromListN 256 [negate (fix 0.34414) * x + oneHalf | x <- [-128 .. 127]]

instance ColorSpaceConvertible PixelYCbCr8 PixelRGB8 where
    {-# INLINE convertPixel #-}
    convertPixel (PixelYCbCr8 y cb cr) = PixelRGB8 (clampWord8 r) (clampWord8 g) (clampWord8 b)
        where clampWord8 = fromIntegral . max 0 . min 255
              yi = fromIntegral y
              cbi = fromIntegral cb
              cri = fromIntegral cr

              r = yi +  crRTab `V.unsafeIndex` cri
              g = yi + (cbGTab `V.unsafeIndex` cbi + crGTab `V.unsafeIndex` cri) `unsafeShiftR` scaleBits
              b = yi +  cbBTab `V.unsafeIndex` cbi

    convertImage Image { imageWidth = w, imageHeight = h, imageData = d } = Image w h newData
        where maxi = w * h
              clampWord8 v | v < 0 = 0
                           | v > 255 = 255
                           | otherwise = fromIntegral v

              newData = runST $ do
                block <- M.new $ maxi * 3
                let traductor _ idx | idx >= maxi = return block
                    traductor readIdx idx = do
                        let yi =  fromIntegral $ d `V.unsafeIndex` readIdx
                            cbi = fromIntegral $ d `V.unsafeIndex` (readIdx + 1)
                            cri = fromIntegral $ d `V.unsafeIndex` (readIdx + 2)

                            r = yi +  crRTab `V.unsafeIndex` cri
                            g = yi + (cbGTab `V.unsafeIndex` cbi + crGTab `V.unsafeIndex` cri) `unsafeShiftR` scaleBits
                            b = yi +  cbBTab `V.unsafeIndex` cbi

                        (block `M.unsafeWrite` (readIdx + 0)) $ clampWord8 r
                        (block `M.unsafeWrite` (readIdx + 1)) $ clampWord8 g
                        (block `M.unsafeWrite` (readIdx + 2)) $ clampWord8 b
                        traductor (readIdx + 3) (idx + 1)

                traductor 0 0 >>= V.freeze

instance ColorPlane PixelYCbCr8 PlaneLuma where
    toComponentIndex _ _ = 0

instance ColorPlane PixelYCbCr8 PlaneCb where
    toComponentIndex _ _ = 1

instance ColorPlane PixelYCbCr8 PlaneCr where
    toComponentIndex _ _ = 2

--------------------------------------------------
----            PixelCMYK8 instances
--------------------------------------------------
instance Pixel PixelCMYK8 where
    type PixelBaseComponent PixelCMYK8 = Word8

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f (PixelCMYK8 ca ma ya ka) (PixelCMYK8 cb mb yb kb) =
        PixelCMYK8 (f 0 ca cb) (f 1 ma mb) (f 2 ya yb) (f 3 ka kb)

    {-# INLINE colorMap #-}
    colorMap f (PixelCMYK8 c m y k) = PixelCMYK8 (f c) (f m) (f y) (f k)

    {-# INLINE componentCount #-}
    componentCount _ = 4

    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y = PixelCMYK8 (arr ! (baseIdx + 0))
                                                               (arr ! (baseIdx + 1))
                                                               (arr ! (baseIdx + 2))
                                                               (arr ! (baseIdx + 3))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr `M.read` baseIdx
        gv <- arr `M.read` (baseIdx + 1)
        bv <- arr `M.read` (baseIdx + 2)
        av <- arr `M.read` (baseIdx + 3)
        return $ PixelCMYK8 rv gv bv av
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelCMYK8 rv gv bv av) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) rv
        (arr `M.write` (baseIdx + 1)) gv
        (arr `M.write` (baseIdx + 2)) bv
        (arr `M.write` (baseIdx + 3)) av

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelCMYK8 (V.unsafeIndex v idx)
                   (V.unsafeIndex v $ idx + 1)
                   (V.unsafeIndex v $ idx + 2)
                   (V.unsafeIndex v $ idx + 3)

    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        PixelCMYK8 `liftM` M.unsafeRead vec idx
                   `ap` M.unsafeRead vec (idx + 1)
                   `ap` M.unsafeRead vec (idx + 2)
                   `ap` M.unsafeRead vec (idx + 3)

    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelCMYK8 r g b a) =
        M.unsafeWrite v idx r >> M.unsafeWrite v (idx + 1) g
                              >> M.unsafeWrite v (idx + 2) b
                              >> M.unsafeWrite v (idx + 3) a

instance ColorSpaceConvertible PixelCMYK8 PixelRGB8 where
  convertPixel (PixelCMYK8 c m y k) =
      PixelRGB8 (clampWord8 r) (clampWord8 g) (clampWord8 b)
    where
      clampWord8 = fromIntegral . max 0 . min 255 . (`div` 255)
      ik :: Int
      ik = 255 - fromIntegral k

      r = (255 - fromIntegral c) * ik
      g = (255 - fromIntegral m) * ik
      b = (255 - fromIntegral y) * ik

--------------------------------------------------
----            PixelYCbCrK8 instances
--------------------------------------------------
instance Pixel PixelYCbCrK8 where
    type PixelBaseComponent PixelYCbCrK8 = Word8

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f (PixelYCbCrK8 ya cba cra ka) (PixelYCbCrK8 yb cbb crb kb) =
        PixelYCbCrK8 (f 0 ya yb) (f 1 cba cbb) (f 2 cra crb) (f 3 ka kb)

    {-# INLINE colorMap #-}
    colorMap f (PixelYCbCrK8 y cb cr k) = PixelYCbCrK8 (f y) (f cb) (f cr) (f k)

    {-# INLINE componentCount #-}
    componentCount _ = 4

    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y =
        PixelYCbCrK8 (arr ! (baseIdx + 0)) (arr ! (baseIdx + 1))
                     (arr ! (baseIdx + 2)) (arr ! (baseIdx + 3))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        yv <- arr `M.read` baseIdx
        cbv <- arr `M.read` (baseIdx + 1)
        crv <- arr `M.read` (baseIdx + 2)
        kv <- arr `M.read` (baseIdx + 3)
        return $ PixelYCbCrK8 yv cbv crv kv
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelYCbCrK8 yv cbv crv kv) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) yv
        (arr `M.write` (baseIdx + 1)) cbv
        (arr `M.write` (baseIdx + 2)) crv
        (arr `M.write` (baseIdx + 3)) kv

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelYCbCrK8 (V.unsafeIndex v idx)
                     (V.unsafeIndex v $ idx + 1)
                     (V.unsafeIndex v $ idx + 2)
                     (V.unsafeIndex v $ idx + 3)

    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
      PixelYCbCrK8 `liftM` M.unsafeRead vec idx
                   `ap` M.unsafeRead vec (idx + 1)
                   `ap` M.unsafeRead vec (idx + 2)
                   `ap` M.unsafeRead vec (idx + 3)

    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelYCbCrK8 y cb cr k) =
        M.unsafeWrite v idx y >> M.unsafeWrite v (idx + 1) cb
                              >> M.unsafeWrite v (idx + 2) cr
                              >> M.unsafeWrite v (idx + 3) k

instance ColorSpaceConvertible PixelYCbCrK8 PixelRGB8 where
  convertPixel (PixelYCbCrK8 y cb cr _k) = PixelRGB8 (clamp r) (clamp g) (clamp b)
    where
      tof :: Word8 -> Float
      tof = fromIntegral

      clamp :: Float -> Word8
      clamp = floor . max 0 . min 255

      yf = tof y

      r = yf + 1.402 * tof cr - 179.456
      g = yf - 0.3441363 * tof cb - 0.71413636 * tof cr + 135.4589
      b = yf + 1.772 * tof cb - 226.816

instance ColorSpaceConvertible PixelYCbCrK8 PixelCMYK8 where
  convertPixel (PixelYCbCrK8 y cb cr k) = PixelCMYK8 c m ye k
    where
      tof :: Word8 -> Float
      tof = fromIntegral

      clamp :: Float -> Word8
      clamp = floor . max 0 . min 255

      yf = tof y

      r = yf + 1.402 * tof cr - 179.456
      g = yf - 0.3441363 * tof cb - 0.71413636 * tof cr + 135.4589
      b = yf + 1.772 * tof cb - 226.816

      c = clamp $ 255 - r
      m = clamp $ 255 - g
      ye = clamp $ 255 - b

{-# SPECIALIZE integralRGBToCMYK :: (Word8 -> Word8 -> Word8 -> Word8 -> b)
                                 -> (Word8, Word8, Word8) -> b #-}
{-# SPECIALIZE integralRGBToCMYK :: (Word16 -> Word16 -> Word16 -> Word16 -> b)
                                 -> (Word16, Word16, Word16) -> b #-}
integralRGBToCMYK :: (Bounded a, Integral a)
                  => (a -> a -> a -> a -> b)    -- ^ Pixel building function
                  -> (a, a, a)                  -- ^ RGB sample
                  -> b                          -- ^ Resulting sample
integralRGBToCMYK build (r, g, b) =
  build (clamp c) (clamp m) (clamp y) (fromIntegral kInt)
    where maxi = maxBound

          ir = fromIntegral $ maxi - r :: Int
          ig = fromIntegral $ maxi - g
          ib = fromIntegral $ maxi - b

          kInt = minimum [ir, ig, ib]
          ik = fromIntegral maxi - kInt

          c = (ir - kInt) `div` ik
          m = (ig - kInt) `div` ik
          y = (ib - kInt) `div` ik

          clamp = fromIntegral . max 0

instance ColorSpaceConvertible PixelRGB8 PixelCMYK8 where
  convertPixel (PixelRGB8 r g b) = integralRGBToCMYK PixelCMYK8 (r, g, b)

instance ColorPlane PixelCMYK8 PlaneCyan where
    toComponentIndex _ _ = 0

instance ColorPlane PixelCMYK8 PlaneMagenta where
    toComponentIndex _ _ = 1

instance ColorPlane PixelCMYK8 PlaneYellow where
    toComponentIndex _ _ = 2

instance ColorPlane PixelCMYK8 PlaneBlack where
    toComponentIndex _ _ = 3

--------------------------------------------------
----            PixelCMYK16 instances
--------------------------------------------------
instance Pixel PixelCMYK16 where
    type PixelBaseComponent PixelCMYK16 = Word16

    {-# INLINE pixelOpacity #-}
    pixelOpacity = const maxBound

    {-# INLINE mixWith #-}
    mixWith f (PixelCMYK16 ca ma ya ka) (PixelCMYK16 cb mb yb kb) =
        PixelCMYK16 (f 0 ca cb) (f 1 ma mb) (f 2 ya yb) (f 3 ka kb)

    {-# INLINE colorMap #-}
    colorMap f (PixelCMYK16 c m y k) = PixelCMYK16 (f c) (f m) (f y) (f k)

    {-# INLINE componentCount #-}
    componentCount _ = 4

    {-# INLINE pixelAt #-}
    pixelAt image@(Image { imageData = arr }) x y = PixelCMYK16 (arr ! (baseIdx + 0))
                                                               (arr ! (baseIdx + 1))
                                                               (arr ! (baseIdx + 2))
                                                               (arr ! (baseIdx + 3))
        where baseIdx = pixelBaseIndex image x y

    {-# INLINE readPixel #-}
    readPixel image@(MutableImage { mutableImageData = arr }) x y = do
        rv <- arr `M.read` baseIdx
        gv <- arr `M.read` (baseIdx + 1)
        bv <- arr `M.read` (baseIdx + 2)
        av <- arr `M.read` (baseIdx + 3)
        return $ PixelCMYK16 rv gv bv av
        where baseIdx = mutablePixelBaseIndex image x y

    {-# INLINE writePixel #-}
    writePixel image@(MutableImage { mutableImageData = arr }) x y (PixelCMYK16 rv gv bv av) = do
        let baseIdx = mutablePixelBaseIndex image x y
        (arr `M.write` (baseIdx + 0)) rv
        (arr `M.write` (baseIdx + 1)) gv
        (arr `M.write` (baseIdx + 2)) bv
        (arr `M.write` (baseIdx + 3)) av

    {-# INLINE unsafePixelAt #-}
    unsafePixelAt v idx =
        PixelCMYK16 (V.unsafeIndex v idx)
                   (V.unsafeIndex v $ idx + 1)
                   (V.unsafeIndex v $ idx + 2)
                   (V.unsafeIndex v $ idx + 3)

    {-# INLINE unsafeReadPixel #-}
    unsafeReadPixel vec idx =
        PixelCMYK16 `liftM` M.unsafeRead vec idx
                   `ap` M.unsafeRead vec (idx + 1)
                   `ap` M.unsafeRead vec (idx + 2)
                   `ap` M.unsafeRead vec (idx + 3)
    {-# INLINE unsafeWritePixel #-}
    unsafeWritePixel v idx (PixelCMYK16 r g b a) =
        M.unsafeWrite v idx r >> M.unsafeWrite v (idx + 1) g
                              >> M.unsafeWrite v (idx + 2) b
                              >> M.unsafeWrite v (idx + 3) a

instance ColorSpaceConvertible PixelCMYK16 PixelRGB16 where
  convertPixel (PixelCMYK16 c m y k) =
      PixelRGB16 (clampWord16 r) (clampWord16 g) (clampWord16 b)
    where
          clampWord16 = fromIntegral . (`unsafeShiftR` 16)
          ik :: Int
          ik = 65535 - fromIntegral k

          r = (65535 - fromIntegral c) * ik
          g = (65535 - fromIntegral m) * ik
          b = (65535 - fromIntegral y) * ik

instance ColorPlane PixelCMYK16 PlaneCyan where
    toComponentIndex _ _ = 0

instance ColorPlane PixelCMYK16 PlaneMagenta where
    toComponentIndex _ _ = 1

instance ColorPlane PixelCMYK16 PlaneYellow where
    toComponentIndex _ _ = 2

instance ColorPlane PixelCMYK16 PlaneBlack where
    toComponentIndex _ _ = 3

-- | Perform a gamma correction for an image with HDR pixels.
gammaCorrection :: PixelF          -- ^ Gamma value, should be between 0.5 and 3.0
                -> Image PixelRGBF -- ^ Image to treat.
                -> Image PixelRGBF
gammaCorrection gammaVal = pixelMap gammaCorrector
  where gammaExponent = 1.0 / gammaVal
        fixVal v = v ** gammaExponent
        gammaCorrector (PixelRGBF r g b) =
            PixelRGBF (fixVal r) (fixVal g) (fixVal b)

-- | Perform a tone mapping operation on an High dynamic range image.
toneMapping :: PixelF          -- ^ Exposure parameter
            -> Image PixelRGBF -- ^ Image to treat.
            -> Image PixelRGBF
toneMapping exposure img = Image (imageWidth img) (imageHeight img) scaledData
 where coeff = exposure * (exposure / maxBrightness + 1.0) / (exposure + 1.0);
       maxBrightness = pixelFold (\luma _ _ px -> max luma $ computeLuma px) 0 img
       scaledData = V.map (* coeff) $ imageData img

--------------------------------------------------
----            Packable pixel
--------------------------------------------------

-- | This typeclass exist for performance reason, it allow
-- to pack a pixel value to a simpler "primitive" data
-- type to allow faster writing to moemory.
class PackeablePixel a where
    -- | Primitive type asociated to the current pixel
    -- It's Word32 for PixelRGBA8 for instance
    type PackedRepresentation a

    -- | The packing function, allowing to transform
    -- to a primitive.
    packPixel :: a -> PackedRepresentation a

    -- | Inverse transformation, to speed up
    -- reading
    unpackPixel :: PackedRepresentation a -> a

instance PackeablePixel Pixel8 where
    type PackedRepresentation Pixel8 = Pixel8
    packPixel = id
    {-# INLINE packPixel #-}
    unpackPixel = id
    {-# INLINE unpackPixel #-}

instance PackeablePixel Pixel16 where
    type PackedRepresentation Pixel16 = Pixel16
    packPixel = id
    {-# INLINE packPixel #-}
    unpackPixel = id
    {-# INLINE unpackPixel #-}

instance PackeablePixel Pixel32 where
    type PackedRepresentation Pixel32 = Pixel32
    packPixel = id
    {-# INLINE packPixel #-}
    unpackPixel = id
    {-# INLINE unpackPixel #-}

instance PackeablePixel PixelF where
    type PackedRepresentation PixelF = PixelF
    packPixel = id
    {-# INLINE packPixel #-}
    unpackPixel = id
    {-# INLINE unpackPixel #-}


instance PackeablePixel PixelRGBA8 where
    type PackedRepresentation PixelRGBA8 = Word32
    {-# INLINE packPixel #-}
    packPixel (PixelRGBA8 r g b a) =
        (fi r `unsafeShiftL` (0 * bitCount)) .|.
        (fi g `unsafeShiftL` (1 * bitCount)) .|.
        (fi b `unsafeShiftL` (2 * bitCount)) .|.
        (fi a `unsafeShiftL` (3 * bitCount))
      where fi = fromIntegral
            bitCount = 8

    {-# INLINE unpackPixel #-}
    unpackPixel w =
        PixelRGBA8 (low w)
                   (low $ w `unsafeShiftR` bitCount)
                   (low $ w `unsafeShiftR` (2 * bitCount))
                   (low $ w `unsafeShiftR` (3 * bitCount))
      where
        low v = fromIntegral (v .&. 0xFF)
        bitCount = 8

instance PackeablePixel PixelRGBA16 where
    type PackedRepresentation PixelRGBA16 = Word64
    {-# INLINE packPixel #-}
    packPixel (PixelRGBA16 r g b a) =
        (fi r `unsafeShiftL` (0 * bitCount)) .|.
        (fi g `unsafeShiftL` (1 * bitCount)) .|.
        (fi b `unsafeShiftL` (2 * bitCount)) .|.
        (fi a `unsafeShiftL` (3 * bitCount))
      where fi = fromIntegral
            bitCount = 16

    {-# INLINE unpackPixel #-}
    unpackPixel w =
        PixelRGBA16 (low w)
                    (low $ w `unsafeShiftR` bitCount)
                    (low $ w `unsafeShiftR` (2 * bitCount))
                    (low $ w `unsafeShiftR` (3 * bitCount))
      where
        low v = fromIntegral (v .&. 0xFFFF)
        bitCount = 16

instance PackeablePixel PixelCMYK8 where
    type PackedRepresentation PixelCMYK8 = Word32
    {-# INLINE packPixel #-}
    packPixel (PixelCMYK8 c m y k) =
        (fi c `unsafeShiftL` (0 * bitCount)) .|.
        (fi m `unsafeShiftL` (1 * bitCount)) .|.
        (fi y `unsafeShiftL` (2 * bitCount)) .|.
        (fi k `unsafeShiftL` (3 * bitCount))
      where fi = fromIntegral
            bitCount = 8

    {-# INLINE unpackPixel #-}
    unpackPixel w =
        PixelCMYK8 (low w)
                   (low $ w `unsafeShiftR` bitCount)
                   (low $ w `unsafeShiftR` (2 * bitCount))
                   (low $ w `unsafeShiftR` (3 * bitCount))
      where
        low v = fromIntegral (v .&. 0xFF)
        bitCount = 8

instance PackeablePixel PixelCMYK16 where
    type PackedRepresentation PixelCMYK16 = Word64
    {-# INLINE packPixel #-}
    packPixel (PixelCMYK16 c m y k) =
        (fi c `unsafeShiftL` (0 * bitCount)) .|.
        (fi m `unsafeShiftL` (1 * bitCount)) .|.
        (fi y `unsafeShiftL` (2 * bitCount)) .|.
        (fi k `unsafeShiftL` (3 * bitCount))
      where fi = fromIntegral
            bitCount = 16

    {-# INLINE unpackPixel #-}
    unpackPixel w =
        PixelCMYK16 (low w)
                    (low $ w `unsafeShiftR` bitCount)
                    (low $ w `unsafeShiftR` (2 * bitCount))
                    (low $ w `unsafeShiftR` (3 * bitCount))
      where
        low v = fromIntegral (v .&. 0xFFFF)
        bitCount = 16

instance PackeablePixel PixelYA16 where
    type PackedRepresentation PixelYA16 = Word32
    {-# INLINE packPixel #-}
    packPixel (PixelYA16 y a) =
        (fi y `unsafeShiftL` (0 * bitCount)) .|.
        (fi a `unsafeShiftL` (1 * bitCount))
      where fi = fromIntegral
            bitCount = 16

    {-# INLINE unpackPixel #-}
    unpackPixel w = PixelYA16 (low w) (low $ w `unsafeShiftR` bitCount)
      where
        low v = fromIntegral (v .&. 0xFFFF)
        bitCount = 16

instance PackeablePixel PixelYA8 where
    type PackedRepresentation PixelYA8 = Word16
    {-# INLINE packPixel #-}
    packPixel (PixelYA8 y a) =
        (fi y `unsafeShiftL` (0 * bitCount)) .|.
        (fi a `unsafeShiftL` (1 * bitCount))
      where fi = fromIntegral
            bitCount = 8

    {-# INLINE unpackPixel #-}
    unpackPixel w = PixelYA8 (low w) (low $ w `unsafeShiftR` bitCount)
      where
        low v = fromIntegral (v .&. 0xFF)
        bitCount = 8

-- | This function will fill an image with a simple packeable
-- pixel. It will be faster than any unsafeWritePixel.
fillImageWith :: ( Pixel px, PackeablePixel px
                 , PrimMonad m
                 , M.Storable (PackedRepresentation px))
              => MutableImage (PrimState m) px -> px -> m ()
fillImageWith img px = M.set converted $ packPixel px
  where
    (ptr, s, s2) = M.unsafeToForeignPtr $ mutableImageData img
    !packedPtr = castForeignPtr ptr
    !converted =
        M.unsafeFromForeignPtr packedPtr s (s2 `div` componentCount px)

-- | Fill a packeable pixel between two bounds.
unsafeWritePixelBetweenAt
    :: ( PrimMonad m
       , Pixel px, PackeablePixel px
       , M.Storable (PackedRepresentation px))
    => MutableImage (PrimState m) px -- ^ Image to write into
    -> px                -- ^ Pixel to write
    -> Int               -- ^ Start index in pixel base component
    -> Int               -- ^ pixel count of pixel to write
    -> m ()
unsafeWritePixelBetweenAt img px start count = M.set converted packed
  where
    !packed = packPixel px
    !pixelData = mutableImageData img

    !toSet = M.slice start count pixelData
    (ptr, s, s2) = M.unsafeToForeignPtr toSet
    !packedPtr = castForeignPtr ptr
    !converted =
        M.unsafeFromForeignPtr packedPtr s s2

-- | Read a packeable pixel from an image. Equivalent to
-- unsafeReadPixel
readPackedPixelAt :: forall m px.
                     ( Pixel px, PackeablePixel px
                     , M.Storable (PackedRepresentation px)
                     , PrimMonad m
                     )
                  => MutableImage (PrimState m) px -- ^ Image to read from
                  -> Int  -- ^ Index in (PixelBaseComponent px) count
                  -> m px
{-# INLINE readPackedPixelAt #-}
readPackedPixelAt img idx = do
    unpacked <- M.unsafeRead converted (idx `div` compCount)
    return $ unpackPixel unpacked
    where
    !compCount = componentCount (undefined :: px)
    (ptr, s, s2) = M.unsafeToForeignPtr $ mutableImageData img
    !packedPtr = castForeignPtr ptr
    !converted =
        M.unsafeFromForeignPtr packedPtr s s2


-- | Write a packeable pixel into an image. equivalent to unsafeWritePixel.
writePackedPixelAt :: ( Pixel px, PackeablePixel px
                      , M.Storable (PackedRepresentation px)
                      , PrimMonad m
                      )
                   => MutableImage (PrimState m) px -- ^ Image to write into
                   -> Int  -- ^ Index in (PixelBaseComponent px) count
                   -> px   -- ^ Pixel to write
                   -> m ()
{-# INLINE writePackedPixelAt #-}
writePackedPixelAt img idx px =
    M.unsafeWrite converted (idx `div` compCount) packed
  where
    !packed = packPixel px
    !compCount = componentCount px

    (ptr, s, s2) = M.unsafeToForeignPtr $ mutableImageData img
    !packedPtr = castForeignPtr ptr
    !converted =
        M.unsafeFromForeignPtr packedPtr s s2

{-# ANN module "HLint: ignore Reduce duplication" #-}

