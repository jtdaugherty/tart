{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
module Types
  ( Mode(..)
  , Name(..)
  , Coord
  , Pixel
  , Tool(..)
  , PaletteEntry(..)

  , AppState(..)
  , drawing
  , canvasSize
  , mode
  , tool
  , showHud
  , drawFgPaletteIndex
  , drawBgPaletteIndex
  , palette
  , drawCharacter
  , fgPaletteSelectorExtent
  , bgPaletteSelectorExtent
  , toolSelectorExtent

  , blankPixel
  , encodePixel
  , decodePixel
  )
where

import Brick (Extent)
import Data.Bits
import Data.Word (Word64)
import Lens.Micro.TH
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

data Mode = Main
          | CharacterSelect
          | FgPaletteEntrySelect
          | BgPaletteEntrySelect
          | ToolSelect
          deriving (Eq, Show)

data Name = Canvas
          | Hud
          | ToolSelector
          | ToolSelectorEntry Tool
          | CharSelector
          | FgSelector
          | BgSelector
          | FgPaletteEntry Int
          | BgPaletteEntry Int
          deriving (Eq, Show, Ord)

data Tool = FreeHand
          | Eraser
          deriving (Eq, Show, Ord)

type Coord = (Int, Int)

type Pixel = (Char, V.Attr)

data PaletteEntry =
    PaletteEntry { paletteFg :: V.Attr -> V.Attr
                 , paletteBg :: V.Attr -> V.Attr
                 }

blankPixel :: Word64
blankPixel = encodePixel ' ' V.defAttr

encodePixel :: Char -> V.Attr -> Word64
encodePixel c a =
    -- Convert char to word32
    -- Convert attr color slots to 10-bit sequences (set bit, type bit, color bits)
    let low32Mask = 2 ^ (32::Integer) - 1
        c64 = fromIntegral $ fromEnum c
    in (c64 .&. low32Mask) .|.
       (encodeAttribute a `shiftL` 32)

decodePixel :: Word64 -> (Char, V.Attr)
decodePixel v =
    let chBits = v .&. (2 ^ (32::Integer) - 1)
        attrBits = v `shiftR` 32
    in (toEnum $ fromIntegral chBits, decodeAttribute attrBits)

encodeAttribute :: V.Attr -> Word64
encodeAttribute attr =
    (encodeAttrColor (V.attrForeColor attr) `shiftL` 10) .|.
    (encodeAttrColor (V.attrBackColor attr))

decodeAttribute :: Word64 -> V.Attr
decodeAttribute v =
    let attrMask = 2 ^ (10::Integer) - 1
    in V.defAttr { V.attrForeColor = decodeAttrColor $ (v `shiftR` 10) .&. attrMask
                 , V.attrBackColor = decodeAttrColor $ v .&. attrMask
                 }

encodeAttrColor :: V.MaybeDefault V.Color -> Word64
encodeAttrColor V.Default = 0
encodeAttrColor V.KeepCurrent = 0
encodeAttrColor (V.SetTo c) =
    let (ty, color) = case c of
          V.ISOColor w -> (0, fromIntegral w)
          V.Color240 w -> (1, fromIntegral w)
    in (1 `shiftL` 9) .|.
       (ty `shiftL` 8) .|.
       color

decodeAttrColor :: Word64 -> V.MaybeDefault V.Color
decodeAttrColor 0 = V.Default
decodeAttrColor v =
    let ty = (v `shiftR` 8) .&. 0b1
        color = fromIntegral $ v .&. 0b11111111
    in if ty == 1
       then V.SetTo $ V.Color240 color
       else V.SetTo $ V.ISOColor color

data AppState =
    AppState { _drawing                 :: Vec.Vector (Vec.Vector Word64)
             , _canvasSize              :: (Int, Int)
             , _mode                    :: Mode
             , _drawFgPaletteIndex      :: Int
             , _drawBgPaletteIndex      :: Int
             , _drawCharacter           :: Char
             , _tool                    :: Tool
             , _showHud                 :: Bool
             , _palette                 :: Vec.Vector PaletteEntry
             , _fgPaletteSelectorExtent :: Maybe (Extent Name)
             , _bgPaletteSelectorExtent :: Maybe (Extent Name)
             , _toolSelectorExtent      :: Maybe (Extent Name)
             }

makeLenses ''AppState
