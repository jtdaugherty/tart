{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Util
  ( checkForMouseSupport
  , setTool
  , setFgPaletteIndex
  , setBgPaletteIndex
  , beginFgPaletteSelect
  , beginBgPaletteSelect
  , beginToolSelect
  , setMode
  , increaseCanvasSize
  , decreaseCanvasSize
  , beginCanvasSizePrompt
  , beginTextEntry
  , tryResizeCanvas
  , quit
  , currentPaletteAttribute
  , handleDragFinished
  , getBoxBorderStyle
  , beginBoxStyleSelect
  , beginStyleSelect
  , writeCanvasFiles
  , resizeCanvas
  , increaseEraserSize
  , decreaseEraserSize
  , increaseRepaintSize
  , decreaseRepaintSize
  , pushUndo
  , toggleStyleFromKey
  , isStyleKey
  , styleBindings

  , canvasMoveDown
  , canvasMoveUp
  , canvasMoveLeft
  , canvasMoveRight

  , tools
  , boxStyles

  , beginCharacterSelect
  , cancelCharacterSelect
  , selectCharacter
  )
where

import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import qualified Data.Text as T
import System.Exit (exitFailure)
import Lens.Micro.Platform
import Data.Text.Zipper (gotoEOL)
import Text.Read (readMaybe)
import Data.Maybe (isJust)

import Brick
import Brick.Focus
import Brick.Widgets.Edit (editor, applyEdit, getEditContents)
import Brick.Widgets.Border.Style

import Types
import Tart.Canvas

tools :: [(Tool, Int)]
tools =
    [ (Freehand  , 1)
    , (Box       , 2)
    , (FloodFill , 3)
    , (TextString, 4)
    , (Repaint   , 5)
    , (Eyedropper, 6)
    , (Eraser    , 0)
    ]

styleBindings :: [(Char, V.Style)]
styleBindings =
    [ ('!', V.bold)
    , ('@', V.underline)
    , ('$', V.blink)
    , ('^', V.reverseVideo)
    ]

isStyleKey :: V.Event -> Bool
isStyleKey (V.EvKey (V.KChar c) []) =
    isJust $ lookup c styleBindings
isStyleKey _ = False

toggleStyleFromKey :: V.Event -> AppState -> AppState
toggleStyleFromKey e s =
    if isStyleKey e
    then let V.EvKey (V.KChar c) _ = e
             Just sty = lookup c styleBindings
         in s & drawStyle %~ toggleStyle sty
    else s

boxStyles :: [(String, BorderStyle)]
boxStyles =
    [ ("ASCII", ascii)
    , ("Unicode", unicode)
    , ("Unicode rounded", unicodeRounded)
    ]

getBoxBorderStyle :: AppState -> (String, BorderStyle)
getBoxBorderStyle s = boxStyles !! (s^.boxStyleIndex)

increaseEraserSize :: AppState -> AppState
increaseEraserSize = (& eraserSize %~ succ)

decreaseEraserSize :: AppState -> AppState
decreaseEraserSize = (& eraserSize %~ (max 1 . pred))

increaseRepaintSize :: AppState -> AppState
increaseRepaintSize = (& repaintSize %~ succ)

decreaseRepaintSize :: AppState -> AppState
decreaseRepaintSize = (& repaintSize %~ (max 1 . pred))

pushUndo :: [Action] -> AppState -> AppState
pushUndo [] s = s
pushUndo l s = s & undoStack %~ (l:)
                 & redoStack .~ []

quit :: Bool -> AppState -> EventM Name (Next AppState)
quit ask s = do
    case (s^.canvasDirty) of
        True ->
            case s^.canvasPath of
                Nothing ->
                    case ask of
                        True -> continue $ askToSave s
                        False -> halt s
                Just p ->
                    if ask
                    then continue $ askToSave s
                    else do
                        liftIO $ writeCanvasFiles p (s^.drawing)
                        halt s
        False -> halt s

writeCanvasFiles :: FilePath -> Canvas -> IO ()
writeCanvasFiles f c = do
    writeCanvas f c
    writeCanvasPlain (f <> ".plain.txt") c
    writeCanvasForTerminal (f <> ".color.txt") c

askToSave :: AppState -> AppState
askToSave s =
    setMode AskToSave $
        s & askToSaveFilenameEdit .~ applyEdit gotoEOL (editor AskToSaveFilenameEdit (Just 1) $
                                           T.pack $ maybe "" id $ s^.canvasPath)

beginTextEntry :: (Int, Int) -> AppState -> AppState
beginTextEntry start s =
    setMode TextEntry $ s & textEntryStart .~ start
                          & textEntered .~ mempty

handleDragFinished :: AppState -> Name -> EventM Name AppState
handleDragFinished s n =
    case n of
        Canvas ->
            case s^.tool of
                Box -> do
                    (c', old) <- liftIO $ merge (s^.drawing) (s^.drawingOverlay)
                    o' <- liftIO $ clearCanvas (s^.drawingOverlay)
                    return $ pushUndo [SetPixels old] $
                             s & drawing .~ c'
                               & drawingOverlay .~ o'
                _ -> return s
        _ -> return s

increaseCanvasSize :: AppState -> EventM Name AppState
increaseCanvasSize s =
    resizeCanvas s
        (canvasSize (s^.drawing) & _1 %~ (\w -> if w == 1 then 4 else w + 4)
                                 & _2 %~ (\h -> if h == 1 then 2 else h + 2))

decreaseCanvasSize :: AppState -> EventM Name AppState
decreaseCanvasSize s =
    resizeCanvas s
        (canvasSize (s^.drawing) & _1 %~ (max 1 . (subtract 4))
                                 & _2 %~ (max 1 . (subtract 2)))

setMode :: Mode -> AppState -> AppState
setMode m s = s & mode .~ m
                & dragging .~ Nothing

beginCanvasSizePrompt :: AppState -> AppState
beginCanvasSizePrompt s =
    setMode CanvasSizePrompt $
        s & canvasSizeFocus .~ focusRing [ CanvasSizeWidthEdit
                                         , CanvasSizeHeightEdit
                                         ]
          & canvasSizeWidthEdit  .~ applyEdit gotoEOL (editor CanvasSizeWidthEdit (Just 1) $
                                           T.pack $ show $ fst $ canvasSize $ s^.drawing)
          & canvasSizeHeightEdit .~ applyEdit gotoEOL (editor CanvasSizeHeightEdit (Just 1) $
                                           T.pack $ show $ snd $ canvasSize $ s^.drawing)

canvasMoveDown :: AppState -> AppState
canvasMoveDown s =
    s & canvasOffset._2 %~ pred

canvasMoveUp :: AppState -> AppState
canvasMoveUp s =
    s & canvasOffset._2 %~ succ

canvasMoveLeft :: AppState -> AppState
canvasMoveLeft s =
    s & canvasOffset._1 %~ pred

canvasMoveRight :: AppState -> AppState
canvasMoveRight s =
    s & canvasOffset._1 %~ succ

tryResizeCanvas :: AppState -> EventM Name AppState
tryResizeCanvas s = do
    -- If the canvas size prompt inputs are valid, resize the canvas and
    -- exit prompt mode. Otherwise stay in prompt mode.
    let [wStr] = getEditContents $ s^.canvasSizeWidthEdit
        [hStr] = getEditContents $ s^.canvasSizeHeightEdit
        result = (,) <$> (readMaybe $ T.unpack wStr)
                     <*> (readMaybe $ T.unpack hStr)
    case result of
        Just (w, h) | w > 0 && h > 0 -> do
            resizeCanvas (setMode Main s) (w, h)
        _ -> return s

beginToolSelect :: AppState -> AppState
beginToolSelect = setMode ToolSelect

beginBoxStyleSelect :: AppState -> AppState
beginBoxStyleSelect = setMode BoxStyleSelect

beginStyleSelect :: AppState -> AppState
beginStyleSelect = setMode StyleSelect

beginFgPaletteSelect :: AppState -> AppState
beginFgPaletteSelect = setMode FgPaletteEntrySelect

beginBgPaletteSelect :: AppState -> AppState
beginBgPaletteSelect = setMode BgPaletteEntrySelect

setTool :: AppState -> Tool -> AppState
setTool s t = s & tool .~ t

setFgPaletteIndex :: AppState -> Int -> AppState
setFgPaletteIndex s i = setMode Main $ s & drawFgPaletteIndex .~ i

setBgPaletteIndex :: AppState -> Int -> AppState
setBgPaletteIndex s i = setMode Main $ s & drawBgPaletteIndex .~ i

beginCharacterSelect :: AppState -> AppState
beginCharacterSelect = setMode CharacterSelect

cancelCharacterSelect :: AppState -> AppState
cancelCharacterSelect = setMode Main

selectCharacter :: Char -> AppState -> AppState
selectCharacter c s = setMode Main $ s & drawCharacter .~ c

checkForMouseSupport :: IO ()
checkForMouseSupport = do
    vty <- V.mkVty =<< V.standardIOConfig

    when (not $ V.supportsMode (V.outputIface vty) V.Mouse) $ do
        putStrLn "Error: this terminal does not support mouse interaction"
        exitFailure

    V.shutdown vty

resizeCanvas :: AppState -> (Int, Int) -> EventM n AppState
resizeCanvas s newSz = do
    c <- liftIO $ resizeFrom (s^.drawing) newSz
    o <- liftIO $ resizeFrom (s^.drawingOverlay) newSz
    return $ s & drawing .~ c
               & drawingOverlay .~ o
               & canvasOffset .~ (Location $ newSz & each %~ (`div` 2))
               & canvasDirty .~ (canvasSize c /= canvasSize (s^.drawing))

currentPaletteAttribute :: AppState -> V.Attr
currentPaletteAttribute s =
    let fgEntry = Vec.unsafeIndex (s^.palette) (s^.drawFgPaletteIndex)
        bgEntry = Vec.unsafeIndex (s^.palette) (s^.drawBgPaletteIndex)
        applyFg Nothing = id
        applyFg (Just c) = (`V.withForeColor` c)
        applyBg Nothing = id
        applyBg (Just c) = (`V.withBackColor` c)
    in (applyFg fgEntry $ applyBg bgEntry V.defAttr) `V.withStyle` (s^.drawStyle)
