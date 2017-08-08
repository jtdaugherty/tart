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
  , tryResizeCanvas
  , quit

  , canvasMoveDown
  , canvasMoveUp
  , canvasMoveLeft
  , canvasMoveRight

  , tools

  , beginCharacterSelect
  , cancelCharacterSelect
  , selectCharacter
  )
where

import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import qualified Graphics.Vty as V
import qualified Data.Text as T
import System.Exit (exitFailure)
import Lens.Micro.Platform
import Data.Text.Zipper (gotoEOL)
import Text.Read (readMaybe)

import Brick
import Brick.Focus
import Brick.Widgets.Edit (editor, applyEdit, getEditContents)

import Types
import Canvas

tools :: [(Tool, Int)]
tools =
    [ (FreeHand, 1)
    , (Eraser, 0)
    ]

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
                        liftIO $ writeCanvas p $ s^.drawing
                        halt s
        False -> halt s

askToSave :: AppState -> AppState
askToSave s =
    setMode AskToSave $
        s & askToSaveFilenameEdit .~ applyEdit gotoEOL (editor AskToSaveFilenameEdit (Just 1) $
                                           T.pack $ maybe "" id $ s^.canvasPath)

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
beginToolSelect s =
    if s^.showHud
    then setMode ToolSelect s
    else s

beginFgPaletteSelect :: AppState -> AppState
beginFgPaletteSelect s =
    if s^.showHud
    then setMode FgPaletteEntrySelect s
    else s

beginBgPaletteSelect :: AppState -> AppState
beginBgPaletteSelect s =
    if s^.showHud
    then setMode BgPaletteEntrySelect s
    else s

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
    return $ s & drawing .~ c
               & canvasOffset .~ (Location $ newSz & each %~ (`div` 2))
