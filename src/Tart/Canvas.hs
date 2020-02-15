{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
module Tart.Canvas
  ( Canvas
  , CanvasData
  , canvasFromData
  , canvasToData
  , newCanvas
  , canvasSize
  , canvasSetPixel
  , canvasSetMany
  , canvasGetPixel
  , resizeFrom
  , prettyPrintCanvas
  , merge
  , clearCanvas
  , canvasFromString
  , canvasFromText
  , canvasLayersToImage
  , normalizeAttr
  )
where

import Control.Monad (forM_, forM, replicateM, when)
import Control.Monad.State
import Data.Bits
import Data.Word (Word64)
import Data.Monoid ((<>))
import Data.Maybe (catMaybes)
import qualified Graphics.Vty as V
import qualified Data.Array.IArray as I
import qualified Data.Array.MArray as A
import qualified Data.Binary as B
import Data.Array.IO (IOUArray)
import Data.Array.Unboxed (UArray)
import Lens.Micro.Platform
import qualified Data.Text as T

data Canvas =
    Canvas { mut   :: IOUArray (Int, Int) Word64
           , immut :: UArray   (Int, Int) Word64
           , size  :: (Int, Int)
           }

data CanvasData =
    CanvasData { canvasDataSize :: (Int, Int)
               , canvasData :: [Word64]
               }

instance B.Binary CanvasData where
    put cd = do
        B.put $ canvasDataSize cd
        mapM_ B.put $ canvasData cd

    get = do
        (w, h) <- B.get
        CanvasData <$> (pure (w, h))
                   <*> replicateM (w * h) B.get

canvasFromData :: CanvasData -> IO (Either String Canvas)
canvasFromData cd = do
    let (w, h) = canvasDataSize cd
    if w * h /= length (canvasData cd)
       then return $ Left "Canvas data entries do not match dimensions"
       else do
           c <- newCanvas (w, h)
           let idxs = [(w', h') | w' <- [0..w-1], h' <- [0..h-1]]
           forM_ (zip idxs (canvasData cd)) $ \(point, word) ->
               A.writeArray (mut c) point word
           f <- A.freeze $ mut c
           return $ Right $ c { immut = f }

canvasToData :: Canvas -> CanvasData
canvasToData c =
    CanvasData sz canvasPixels
    where
        sz@(w, h) = canvasSize c
        canvasPixels =
           [ canvasGetPixelRaw c (w', h')
           | w' <- [0..w-1], h' <- [0..h-1]
           ]

newCanvas :: (Int, Int) -> IO Canvas
newCanvas sz = do
    let arrayBounds = ((0, 0), sz & each %~ pred)
    draw <- A.newArray arrayBounds blankPixel
    drawFreeze <- A.freeze draw
    return $ Canvas draw drawFreeze sz

canvasFromString :: String -> IO Canvas
canvasFromString = canvasFromText . T.pack

canvasFromText :: T.Text -> IO Canvas
canvasFromText t = do
    let ls = convertTab <$> T.lines t
        convertTab = T.concatMap convertTabChar
        convertTabChar '\t' = T.replicate 8 " "
        convertTabChar c = T.singleton c
        height = length ls
        width = maximum $ T.length <$> ls
        pixs = concat $ mkRowPixels <$> zip [0..] ls
        mkRowPixels (rowNum, row) =
            mkPixel rowNum <$> zip [0..] (T.unpack row)
        mkPixel rowNum (colNum, ch) =
            ((colNum, rowNum), ch, V.defAttr)

    c <- newCanvas (width, height)
    canvasSetMany c pixs

clearCanvas :: Canvas -> IO Canvas
clearCanvas c = do
    let (width, height) = canvasSize c
    forM_ [0..width-1] $ \w ->
        forM_ [0..height-1] $ \h -> do
            A.writeArray (mut c) (w, h) blankPixel
    f <- A.freeze (mut c)
    return $ c { immut = f }

type RLE a = State RLEState a

data RLEState =
    RLEState { content       :: [(T.Text, V.Attr)]
             , currentString :: T.Text
             , currentAttr   :: V.Attr
             }

runRLE :: RLE () -> [(T.Text, V.Attr)]
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
    modify $ \s -> s { currentString = currentString s <> T.singleton c
                     }

sealFinalToken :: RLE ()
sealFinalToken =
    modify $ \s -> s { content = if T.null $ currentString s
                                 then content s
                                 else content s <> [(currentString s, currentAttr s)]
                     }

newToken :: Char -> V.Attr -> RLE ()
newToken c a =
    modify $ \s -> s { currentString = T.singleton c
                     , currentAttr = a
                     , content = if T.null $ currentString s
                                 then content s
                                 else content s <> [(currentString s, currentAttr s)]
                     }

prettyPrintCanvas :: Bool -> [Canvas] -> T.Text
prettyPrintCanvas emitSequences cs =
    let pairs = runRLE (mkRLE cs)
        mkOutput (s, attr) =
            if emitSequences
            then ctrlSequence attr <> s
            else s
        ctrlSequence a =
            "\ESC[0m" <> attrSequence a
    in T.concat $ mkOutput <$> pairs

mkRLE :: [Canvas] -> RLE ()
mkRLE [] = return ()
mkRLE cs@(c:_) = do
    let (w, h) = canvasSize c
    forM_ [0..h-1] $ \row -> do
        forM_ [0..w-1] $ \col ->
            rleNext $ findPixel cs (col, row)
        rleNext ('\n', V.defAttr)

attrSequence :: V.Attr -> T.Text
attrSequence a =
    let fg = colorCode True (V.attrForeColor a)
        bg = colorCode False (V.attrBackColor a)
        sty = styleCode (V.attrStyle a)
    in fg <> bg <> sty

styleCode :: V.MaybeDefault V.Style -> T.Text
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

styleCode' :: V.Style -> T.Text
styleCode' s =
    let present = filter (V.hasStyle s) styles
    in if null present
       then ""
       else "\ESC[" <> T.intercalate ";" (styleToCode <$> present) <> "m"

styleToCode :: V.Style -> T.Text
styleToCode s =
    let mapping = [ (V.bold,         "1")
                  , (V.underline,    "4")
                  , (V.blink,        "5")
                  , (V.reverseVideo, "7")
                  ]
    in maybe "" id $ lookup s mapping

colorCode :: Bool -> V.MaybeDefault V.Color -> T.Text
colorCode _ V.KeepCurrent = ""
colorCode _ V.Default = ""
colorCode f (V.SetTo c) = colorCode' f c

colorCode' :: Bool -> V.Color -> T.Text
colorCode' f (V.Color240 w) =
    "\ESC[" <> if f then "38" else "48" <> ";5;" <> T.pack (show w) <> "m"
colorCode' f (V.ISOColor w) =
    let c = if f then "38" else "48"
        valid v = v >= 0 && v <= 15
    in if valid w
       then "\ESC[" <> c <> ";5;" <> T.pack (show w) <> "m"
       else ""

canvasSize :: Canvas -> (Int, Int)
canvasSize = size

canvasGetPixel :: Canvas -> (Int, Int) -> (Char, V.Attr)
canvasGetPixel c p = decodePixel $ canvasGetPixelRaw c p

canvasGetPixelRaw :: Canvas -> (Int, Int) -> Word64
canvasGetPixelRaw c point = (immut c) I.! point

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
canvasLayersToImage :: [Canvas] -> V.Image
canvasLayersToImage [] = V.emptyImage
canvasLayersToImage cs =
    let sizes = canvasSize <$> cs
        smallestSize = ( minimum $ fst <$> sizes
                       , minimum $ snd <$> sizes
                       )
        (lastCol, lastRow) = smallestSize & each %~ pred
        rows = getRow <$> [0..lastRow]
        getRow r = V.horizCat $ (uncurry $ flip V.char) <$> getCol r <$> [0..lastCol]
        getCol r c = findPixel cs (c, r)
    in V.vertCat rows

findPixel :: [Canvas] -> (Int, Int) -> (Char, V.Attr)
findPixel [] _ = error "BUG: canvasLayersToImage got no layers"
findPixel [l] point = canvasGetPixel l point
findPixel (l:ls) point =
    let pix = canvasGetPixel l point
        blank = decodePixel blankPixel
    in if pix == blank
       then findPixel ls point
       else pix
