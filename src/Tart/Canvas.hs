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
  , canvasToImage
  )
where

import Control.Monad (forM_, forM, replicateM, when)
import Control.Monad.State
import Data.Bits
import Data.Word (Word64)
import Data.Monoid ((<>))
import Data.Maybe (catMaybes)
import Data.List (intercalate)
import qualified Graphics.Vty as V
import qualified Data.Array.IArray as I
import qualified Data.Array.MArray as A
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
    f <- A.freeze (mut c)
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

                   f <- A.freeze $ mut c
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

type RLE a = State RLEState a

data RLEState =
    RLEState { content       :: [(String, V.Attr)]
             , currentString :: String
             , currentAttr   :: V.Attr
             }

runRLE :: RLE () -> [(String, V.Attr)]
runRLE act =
    let s = execState (act >> sealFinalToken) (RLEState [] "" V.defAttr)
    in content s

rleNext :: (Char, V.Attr) -> RLE ()
rleNext (ch, attr) = do
    -- If the attribute matches the current attribute, just append the
    -- character.
    cur <- gets currentAttr
    case cur == attr of
        True -> appendCharacter ch
        False -> newToken ch attr

appendCharacter :: Char -> RLE ()
appendCharacter c =
    modify $ \s -> s { currentString = currentString s <> [c]
                     }

sealFinalToken :: RLE ()
sealFinalToken =
    modify $ \s -> s { content = if null $ currentString s
                                 then content s
                                 else content s <> [(currentString s, currentAttr s)]
                     }

newToken :: Char -> V.Attr -> RLE ()
newToken c a =
    modify $ \s -> s { currentString = [c]
                     , currentAttr = a
                     , content = if null $ currentString s
                                 then content s
                                 else content s <> [(currentString s, currentAttr s)]
                     }

ppCanvas :: Bool -> Canvas -> String
ppCanvas emitSequences c =
    let pairs = runRLE (mkRLE c)
        mkOutput (s, attr) =
            if emitSequences
            then ctrlSequence attr <> s
            else s
        ctrlSequence a =
            "\ESC[0m" <> attrSequence a
    in concat $ mkOutput <$> pairs

mkRLE :: Canvas -> RLE ()
mkRLE c = do
    let (w, h) = canvasSize c
    forM_ [0..h-1] $ \row -> do
        forM_ [0..w-1] $ \col ->
            rleNext $ canvasGetPixel c (col, row)
        rleNext ('\n', V.defAttr)

attrSequence :: V.Attr -> String
attrSequence a =
    let fg = colorCode True (V.attrForeColor a)
        bg = colorCode False (V.attrBackColor a)
        sty = styleCode (V.attrStyle a)
    in fg <> bg <> sty

styleCode :: V.MaybeDefault V.Style -> String
styleCode V.KeepCurrent = ""
styleCode V.Default = ""
styleCode (V.SetTo s) = styleCode' s

styles :: [V.Style]
styles =
    [ V.bold
    , V.underline
    , V.blink
    , V.reverseVideo
    ]

styleCode' :: V.Style -> String
styleCode' s =
    let present = filter (V.hasStyle s) styles
    in if null present
       then ""
       else "\ESC[" <> intercalate ";" (styleToCode <$> present) <> "m"

styleToCode :: V.Style -> String
styleToCode s =
    let mapping = [ (V.bold,         "1")
                  , (V.underline,    "4")
                  , (V.blink,        "5")
                  , (V.reverseVideo, "7")
                  ]
    in maybe "" id $ lookup s mapping

colorCode :: Bool -> V.MaybeDefault V.Color -> String
colorCode _ V.KeepCurrent = ""
colorCode _ V.Default = ""
colorCode f (V.SetTo c) = colorCode' f c

colorCode' :: Bool -> V.Color -> String
colorCode' f (V.Color240 w) =
    "\ESC[" <> if f then "38" else "48" <> ";5;" <> show w <> "m"
colorCode' f (V.ISOColor w) =
    let c = if f then "38" else "48"
        valid v = v >= 0 && v <= 15
    in if valid w
       then "\ESC[" <> c <> ";5;" <> show w <> "m"
       else ""

canvasSize :: Canvas -> (Int, Int)
canvasSize = size

canvasGetPixel :: Canvas -> (Int, Int) -> (Char, V.Attr)
canvasGetPixel c point =
    decodePixel $ (immut c) I.! point

canvasSetMany :: Canvas -> [((Int, Int), Char, V.Attr)] -> IO Canvas
canvasSetMany c pixels = do
    forM_ pixels $ \(point, ch, attr) -> do
        valid <- isValidPoint point (mut c)
        when valid $ A.writeArray (mut c) point $ encodePixel ch attr

    f <- A.freeze (mut c)
    return $ c { immut = f
               }

isValidPoint :: (Int, Int) -> IOUArray (Int, Int) Word64 -> IO Bool
isValidPoint (c, r) arr = do
    ((loC, loR), (hiC, hiR)) <- A.getBounds arr
    return $ r >= loR && c >= loC &&
             r <= hiR && c <= hiC

canvasSetPixel :: Canvas -> (Int, Int) -> Char -> V.Attr -> IO Canvas
canvasSetPixel c point ch attr = canvasSetMany c [(point, ch, attr)]

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
            (c, _) <- merge new old
            return c

encodePixel :: Char -> V.Attr -> Word64
encodePixel c a =
    -- Convert char to word32
    -- Convert attr color slots to 10-bit sequences (set bit, type bit, color bits)
    let low32Mask = 2 ^ (32::Integer) - 1
        c64 = fromIntegral $ fromEnum c
        a' = normalizeAttr c a
    in (c64 .&. low32Mask) .|.
       (encodeAttribute a' `shiftL` 32)

decodePixel :: Word64 -> (Char, V.Attr)
decodePixel v =
    let chBits = v .&. (2 ^ (32::Integer) - 1)
        attrBits = v `shiftR` 32
        attr = decodeAttribute attrBits
        ch = toEnum $ fromIntegral chBits
    in (ch, normalizeAttr ch attr)

normalizeAttr :: Char -> V.Attr -> V.Attr
normalizeAttr ch attr =
    if ch == ' ' && (not $ hasForegroundStyle $ V.attrStyle attr)
    then attr { V.attrForeColor = V.Default
              , V.attrStyle = V.Default
              }
    else attr

hasForegroundStyle :: V.MaybeDefault V.Style -> Bool
hasForegroundStyle (V.SetTo s) =
    or [ V.hasStyle s V.underline
       , V.hasStyle s V.reverseVideo
       ]
hasForegroundStyle _ = False

encodeAttribute :: V.Attr -> Word64
encodeAttribute attr =
    (encodeAttrStyle (V.attrStyle attr) `shiftL` 20) .|.
    (encodeAttrColor (V.attrForeColor attr) `shiftL` 10) .|.
    (encodeAttrColor (V.attrBackColor attr))

encodeAttrStyle :: V.MaybeDefault V.Style -> Word64
encodeAttrStyle V.Default = 0
encodeAttrStyle V.KeepCurrent = 0
encodeAttrStyle (V.SetTo s) = fromIntegral s

decodeAttrStyle :: Word64 -> V.MaybeDefault V.Style
decodeAttrStyle 0 = V.Default
decodeAttrStyle v = V.SetTo $ fromIntegral v

decodeAttribute :: Word64 -> V.Attr
decodeAttribute v =
    let attrColorMask = 2 ^ (10::Integer) - 1
        attrStyleMask = 2 ^ (8::Integer) - 1
    in V.defAttr { V.attrStyle     = decodeAttrStyle $ (v `shiftR` 20) .&. attrStyleMask
                 , V.attrForeColor = decodeAttrColor $ (v `shiftR` 10) .&. attrColorMask
                 , V.attrBackColor = decodeAttrColor $ v .&. attrColorMask
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

merge :: Canvas -> Canvas -> IO (Canvas, [((Int, Int), (Char, V.Attr))])
merge dest src = do
    let (width, height) = (min srcW destW, min srcH destH)
        (srcW, srcH) = canvasSize src
        (destW, destH) = canvasSize dest

    undoBuf <- forM [0..width-1] $ \w ->
        forM [0..height-1] $ \h -> do
            let pix = (immut src) I.! (w, h)
            case pix /= blankPixel of
                True -> do
                    old <- A.readArray (mut dest) (w, h)
                    A.writeArray (mut dest) (w, h) pix
                    return $ Just ((w, h), decodePixel old)
                False ->
                    return Nothing

    f <- A.freeze $ mut dest
    return (dest { immut = f }, catMaybes $ concat undoBuf)

-- | Create a Vty image from a list of canvas layers, with the topmost
-- layer being the first canvas in the list. A pixel in the final image
-- is set by looking for the first non-blank pixel in the canvas list,
-- starting at the beginning.
--
-- The result will be as high as the least tall input canvas, and as
-- wide as the least wide input canvas.
canvasToImage :: [Canvas] -> V.Image
canvasToImage [] = V.emptyImage
canvasToImage cs =
    let sizes = canvasSize <$> cs
        smallestSize = ( minimum $ fst <$> sizes
                       , minimum $ snd <$> sizes
                       )
        (lastCol, lastRow) = smallestSize & each %~ pred
        blank = decodePixel blankPixel
        rows = getRow <$> [0..lastRow]
        getRow r = V.horizCat $ (uncurry $ flip V.char) <$> getCol r <$> [0..lastCol]
        getCol r c = findPixel (c, r) cs
        findPixel _ [] = error "BUG: canvasToImage got no layers"
        findPixel point [l] = canvasGetPixel l point
        findPixel point (l:ls) =
            let pix = canvasGetPixel l point
            in if pix == blank
               then findPixel point ls
               else pix
    in V.vertCat rows
