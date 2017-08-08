{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BinaryLiterals #-}
module Canvas
  ( Canvas
  , newCanvas
  , canvasSize
  , canvasSetPixel
  , canvasGetPixel
  , resizeFrom
  , writeCanvas

  , blankPixel
  , encodePixel
  , decodePixel
  )
where

import Control.Monad (forM_)
import Data.Bits
import Data.Word (Word64)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V
import qualified Data.Array.IArray as I
import qualified Data.Array.MArray as A
import qualified Data.Array.Unsafe as A
import Data.Array.IO (IOUArray)
import Data.Array.Unboxed (UArray)
import Lens.Micro.Platform

data Canvas =
    Canvas { mut   :: IOUArray (Int, Int) Word64
           , immut :: UArray   (Int, Int) Word64
           , size  :: (Int, Int)
           }

newCanvas :: (Int, Int) -> IO Canvas
newCanvas sz = do
    let arrayBounds = ((0, 0), sz & each %~ pred)
    draw <- A.newArray arrayBounds blankPixel
    drawFreeze <- A.freeze draw
    return $ Canvas draw drawFreeze sz

writeCanvas :: FilePath -> Canvas -> IO ()
writeCanvas path c = writeFile path $ ppCanvas c

ppCanvas :: Canvas -> String
ppCanvas c =
    let ppLine pairs = unlines $ ppPair <$> pairs
        ppChange NoChange    = "keep"
        ppChange (Set color) = colorName color
        ppChange Clear       = "clear"
        ppPair ((fChange, bChange), str) =
            unlines [ ppChange fChange
                    , ppChange bChange
                    , str
                    ]
    in unlines $ ppLine <$> rleEncode c

colorName :: V.Color -> String
colorName (V.Color240 w) = "color240 " <> show w
colorName (V.ISOColor w) =
    case w of
        0  -> "black"
        1  -> "red"
        2  -> "green"
        3  -> "yellow"
        4  -> "blue"
        5  -> "magenta"
        6  -> "cyan"
        7  -> "white"
        8  -> "brightBlack"
        9  -> "brightRed"
        10 -> "brightGreen"
        11 -> "brightYellow"
        12 -> "brightBlue"
        13 -> "brightMagenta"
        14 -> "brightCyan"
        15 -> "brightWhite"
        _  -> "unknown"

decodeCanvas :: Canvas -> [[(Char, V.Attr)]]
decodeCanvas c =
    decodeRow <$> [0..height-1]
    where
        (width, height) = canvasSize c
        decodeRow row = canvasGetPixel c <$> (, row) <$> [0..width-1]

rleEncode :: Canvas -> [[((ColorChange, ColorChange), [Char])]]
rleEncode c =
    rleEncodeLine <$> decodeCanvas c

rleEncodeLine :: [(Char, V.Attr)] -> [((ColorChange, ColorChange), [Char])]
rleEncodeLine row =
    let go (changes, _, s) [] = [(changes, s)]
        go (changes, prevAttr, prevChunk) ((curChar, curAttr):rest) =
            let (f, b) = colorChanges prevAttr curAttr
            in if f == NoChange && b == NoChange
               then go (changes, prevAttr, prevChunk <> [curChar]) rest
               else (changes, prevChunk) : go ((f, b), curAttr, [curChar]) rest
    in go ((NoChange, NoChange), V.defAttr, "") row

data ColorChange =
    NoChange
    | Clear
    | Set V.Color
    deriving (Show, Eq)

colorChanges :: V.Attr -> V.Attr -> (ColorChange, ColorChange)
colorChanges a b =
    let fg = colorChange (V.attrForeColor a) (V.attrForeColor b)
        bg = colorChange (V.attrBackColor a) (V.attrBackColor b)
    in (fg, bg)

colorChange ::  V.MaybeDefault V.Color
            -> V.MaybeDefault V.Color
            -> ColorChange
colorChange V.Default V.Default       = NoChange
colorChange _ V.KeepCurrent           = NoChange
colorChange V.KeepCurrent V.Default   = Clear
colorChange (V.SetTo _) V.Default     = Clear
colorChange (V.SetTo a) (V.SetTo b)   = if a == b then NoChange else Set b
colorChange V.Default (V.SetTo b)     = Set b
colorChange V.KeepCurrent (V.SetTo b) = Set b

canvasSize :: Canvas -> (Int, Int)
canvasSize = size

canvasGetPixel :: Canvas -> (Int, Int) -> (Char, V.Attr)
canvasGetPixel c point =
    decodePixel $ (immut c) I.! point

canvasSetPixel :: Canvas -> (Int, Int) -> Char -> V.Attr -> IO Canvas
canvasSetPixel c point ch attr = do
    A.writeArray (mut c) point $ encodePixel ch attr
    f <- A.freeze (mut c)
    return $ c { immut = f
               }

blankPixel :: Word64
blankPixel = encodePixel ' ' V.defAttr

resizeFrom :: Canvas -> (Int, Int) -> IO Canvas
resizeFrom old newSz = do
    -- If the new bounds are different than the old, create a new array
    -- and copy.
    case newSz /= canvasSize old of
        False -> return old
        True -> do
            new <- newCanvas newSz

            -- Use the difference in size to determine the range of data
            -- to copy to the new canvas
            let (maxW, maxH) = ( min (newSz^._1) ((canvasSize old)^._1)
                               , min (newSz^._2) ((canvasSize old)^._2)
                               )

            forM_ [0..maxW-1] $ \w ->
                forM_ [0..maxH-1] $ \h ->
                    A.writeArray (mut new) (w, h) $
                        (immut old) I.! (w, h)

            f <- A.unsafeFreeze $ mut new
            return $ new { immut = f
                         }

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
