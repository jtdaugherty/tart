{-# LANGUAGE BinaryLiterals #-}
module Canvas
  ( blankPixel
  , encodePixel
  , decodePixel
  )
where

import Data.Bits
import Data.Word (Word64)
import qualified Graphics.Vty as V

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
