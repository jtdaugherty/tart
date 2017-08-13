{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BinaryLiterals #-}
module Tart.Canvas
  ( Canvas
  , newCanvas
  , canvasSize
  , canvasSetPixel
  , canvasSetMany
  , canvasGetPixel
  , resizeFrom
  , writeCanvas
  , writeCanvasForTerminal
  , writeCanvasPlain
  , readCanvas
  , merge
  , clearCanvas
  , canvasFromText

  , blankPixel
  , encodePixel
  , decodePixel
  )
where

import Control.Monad (forM_, replicateM, when)
import Data.Bits
import Data.Word (Word64)
import Data.Monoid ((<>))
import Data.List (reverse)
import Data.Char (isSpace)
import qualified Graphics.Vty as V
import qualified Data.Array.IArray as I
import qualified Data.Array.MArray as A
import qualified Data.Array.Unsafe as A
import qualified Data.Binary as B
import qualified Data.Binary.Put as B
import qualified Data.Binary.Get as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
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

canvasFromText :: String -> IO Canvas
canvasFromText s = do
    let ls = convertTab <$> lines s
        convertTab l = concat $ convertTabChar <$> l
        convertTabChar '\t' = replicate 8 ' '
        convertTabChar c = [c]
        height = length ls
        width = maximum $ length <$> ls
        pixs = concat $ mkRowPixels <$> zip [0..] ls
        mkRowPixels (rowNum, row) =
            mkPixel rowNum <$> zip [0..] row
        mkPixel rowNum (colNum, ch) =
            ((colNum, rowNum), ch, V.defAttr)

    c <- newCanvas (width, height)
    canvasSetMany c pixs

writeCanvasForTerminal :: FilePath -> Canvas -> IO ()
writeCanvasForTerminal path c = writeFile path $ ppCanvas True c

writeCanvasPlain :: FilePath -> Canvas -> IO ()
writeCanvasPlain path c = writeFile path $ ppCanvas False c

clearCanvas :: Canvas -> IO Canvas
clearCanvas c = do
    let (width, height) = canvasSize c
    forM_ [0..width-1] $ \w ->
        forM_ [0..height-1] $ \h -> do
            A.writeArray (mut c) (w, h) blankPixel
    f <- A.unsafeFreeze (mut c)
    return $ c { immut = f }

readCanvas :: FilePath -> IO (Either String Canvas)
readCanvas path = do
    bytes <- BS.readFile path

    let parser :: B.Get ((Int, Int), [Word64])
        parser = do
            (w, h) <- B.get
            ps <- replicateM (w * h) B.get
            return ((w, h), ps)
    case B.runGetOrFail parser $ BSL.fromStrict bytes of
        Left (_, _, s) -> return $ Left s
        Right (remaining, _, val) -> do
            let (sz, pixels) = val

            if | not $ BSL.null remaining ->
                   return $ Left $ "File contained " <> show (BSL.length remaining) <>
                                   " extra bytes"
               | length pixels /= fst sz * snd sz ->
                   return $ Left "File did not contain expected amount of data"
               | otherwise -> do
                   c <- newCanvas sz

                   let (width, height) = sz
                       idxs = [(w, h) | w <- [0..width-1], h <- [0..height-1]]

                   forM_ (zip idxs pixels) $ uncurry $ A.writeArray (mut c)

                   f <- A.unsafeFreeze $ mut c
                   return $ Right $ c { immut = f }

writeCanvas :: FilePath -> Canvas -> IO ()
writeCanvas path c = do
    let bytes = B.runPut $ do
          B.put $ canvasSize c

          let (width, height) = canvasSize c
          forM_ [0..width-1] $ \w ->
              forM_ [0..height-1] $ \h ->
                  B.put $ (immut c) I.! (w, h)

    BS.writeFile path $ BSL.toStrict bytes

ppCanvas :: Bool -> Canvas -> String
ppCanvas emitSequences c =
    let maybeStrip = if emitSequences
                     then id
                     else strip
        strip = reverse . dropWhile isSpace . reverse
        ppLine pairs = concat $ ppPair <$> pairs
        ppChange _ _ | not emitSequences = ""
        ppChange _ NoChange              = ""
        ppChange b (Set color)           = colorCode b color
        ppChange _ Clear                 = "\ESC[0m"
        ppPair ((fChange, bChange), str) =
            -- If both are to be cleared, emit a single clear.
            let fg = ppChange True  fChange
                bg = ppChange False bChange
                ctrlseq = if fChange == bChange && fChange == Clear
                             then fg
                             else if fChange == Clear
                                  then fg <> bg
                                  else if bChange == Clear
                                       then bg <> fg
                                       else fg <> bg
            in ctrlseq <> str
    in unlines $ maybeStrip <$> ppLine <$> rleEncode c

colorCode :: Bool -> V.Color -> String
colorCode f (V.Color240 w) =
    "\ESC[" <> if f then "38" else "48" <> ";5;" <> show w <> "m"
colorCode f (V.ISOColor w) =
    let c = if f then "38" else "48"
    in case w of
        0  -> "\ESC[" <> c <> ";5;" <> show w <> "m" -- "black"
        1  -> "\ESC[" <> c <> ";5;" <> show w <> "m" -- "red"
        2  -> "\ESC[" <> c <> ";5;" <> show w <> "m" -- "green"
        3  -> "\ESC[" <> c <> ";5;" <> show w <> "m" -- "yellow"
        4  -> "\ESC[" <> c <> ";5;" <> show w <> "m" -- "blue"
        5  -> "\ESC[" <> c <> ";5;" <> show w <> "m" -- "magenta"
        6  -> "\ESC[" <> c <> ";5;" <> show w <> "m" -- "cyan"
        7  -> "\ESC[" <> c <> ";5;" <> show w <> "m" -- "white"
        8  -> "\ESC[" <> c <> ";5;" <> show w <> "m" -- "brightBlack"
        9  -> "\ESC[" <> c <> ";5;" <> show w <> "m" -- "brightRed"
        10 -> "\ESC[" <> c <> ";5;" <> show w <> "m" -- "brightGreen"
        11 -> "\ESC[" <> c <> ";5;" <> show w <> "m" -- "brightYellow"
        12 -> "\ESC[" <> c <> ";5;" <> show w <> "m" -- "brightBlue"
        13 -> "\ESC[" <> c <> ";5;" <> show w <> "m" -- "brightMagenta"
        14 -> "\ESC[" <> c <> ";5;" <> show w <> "m" -- "brightCyan"
        15 -> "\ESC[" <> c <> ";5;" <> show w <> "m" -- "brightWhite"
        _  -> "" -- "unknown"

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

canvasSetMany :: Canvas -> [((Int, Int), Char, V.Attr)] -> IO Canvas
canvasSetMany c pixels = do
    forM_ pixels $ \(point, ch, attr) -> do
        A.writeArray (mut c) point $ encodePixel ch attr

    f <- A.freeze (mut c)
    return $ c { immut = f
               }

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
            merge new old

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

merge :: Canvas -> Canvas -> IO Canvas
merge dest src = do
    let (width, height) = (min srcW destW, min srcH destH)
        (srcW, srcH) = canvasSize src
        (destW, destH) = canvasSize dest
    forM_ [0..width-1] $ \w ->
        forM_ [0..height-1] $ \h -> do
            let pix = (immut src) I.! (w, h)
            when (pix /= blankPixel) $
                A.writeArray (mut dest) (w, h) pix

    f <- A.unsafeFreeze $ mut dest
    return $ dest { immut = f }
