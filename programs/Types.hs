{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Types
  ( Mode(..)
  , Name(..)
  , Tool(..)
  , AppEvent(..)
  , Action(..)
  , toolName
  , isSelectionMode

  , LayerInfo(..)
  , layerName
  , layerVisible

  , noStyle
  , setStyle
  , clearStyle
  , toggleStyle
  , hasStyle

  , AppState(..)
  , layers
  , currentLayer
  , layerAt
  , layerInfoFor
  , layerOrder
  , layerInfo
  , layerNameEditor
  , selectedLayerIndex
  , drawingOverlay
  , modes
  , currentMode
  , tool
  , drawFgPaletteIndex
  , drawBgPaletteIndex
  , layerListVisible
  , appCanvasSize
  , palette
  , drawCharacter
  , fgPaletteSelectorExtent
  , bgPaletteSelectorExtent
  , toolSelectorExtent
  , boxStyleSelectorExtent
  , styleSelectorExtent
  , canvasExtent
  , dragging
  , canvasSizeWidthEdit
  , canvasSizeHeightEdit
  , canvasSizeFocus
  , canvasOffset
  , canvasPath
  , canvasDirty
  , askToSaveFilenameEdit
  , appEventChannel
  , textEntered
  , textEntryStart
  , boxStyleIndex
  , eraserSize
  , repaintSize
  , restyleSize
  , undoStack
  , redoStack
  , drawStyle
  )
where

import Data.Bits ((.&.), (.|.), complement)
import Data.Word (Word8)
import Data.Maybe (fromJust)
import Brick (Extent, Location)
import Brick.BChan (BChan)
import Brick.Focus
import Brick.Widgets.Edit (Editor)
import qualified Data.Text as T
import qualified Data.Map as M
import Lens.Micro.Platform
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

import Tart.Canvas

data AppEvent =
    DragFinished Name Location Location
    deriving (Eq)

data Action =
    SetPixels Int [((Int, Int), (Char, V.Attr))]
    | ClearCanvasDirty
    | InsertLayer Canvas Int Int String
    | RemoveLayer Int
    | ChangeLayerName Int T.Text
    | MoveLayerBy Int Bool
    | ToggleLayer Int

data Mode = Main
          | CharacterSelect
          | FgPaletteEntrySelect
          | BgPaletteEntrySelect
          | ToolSelect
          | StyleSelect
          | BoxStyleSelect
          | CanvasSizePrompt
          | AskToSave
          | TextEntry
          | RenameLayer
          deriving (Eq, Show)

selectionModes :: [Mode]
selectionModes =
    [ CharacterSelect
    , FgPaletteEntrySelect
    , BgPaletteEntrySelect
    , ToolSelect
    , StyleSelect
    , BoxStyleSelect
    ]

isSelectionMode :: Mode -> Bool
isSelectionMode = (`elem` selectionModes)

data Name = Canvas
          | TopHud
          | BottomHud
          | ToolSelector
          | ToolSelectorEntry Tool
          | CharSelector
          | FgSelector
          | BgSelector
          | StyleSelector
          | StyleSelectorEntry V.Style
          | FgPaletteEntry Int
          | BgPaletteEntry Int
          | BoxStyleSelectorEntry Int
          | ResizeCanvas
          | CanvasSizeWidthEdit
          | CanvasSizeHeightEdit
          | AskToSaveFilenameEdit
          | TextEntryCursor
          | BoxStyleSelector
          | IncreaseToolSize
          | DecreaseToolSize
          | SelectLayer Int
          | LayerName
          | LayerNameEditor
          | AddLayer
          | DeleteLayer
          | MoveLayerUp
          | MoveLayerDown
          | ToggleLayerVisible
          deriving (Eq, Show, Ord)

data Tool = Freehand
          | Box
          | Repaint
          | Restyle
          | Eyedropper
          | FloodFill
          | Eraser
          | TextString
          deriving (Eq, Show, Ord)

toolName :: Tool -> String
toolName Freehand   = "Freehand"
toolName Box        = "Box"
toolName Repaint    = "Repaint"
toolName Restyle    = "Restyle"
toolName Eraser     = "Eraser"
toolName Eyedropper = "Eyedropper"
toolName FloodFill  = "Flood fill"
toolName TextString = "Text string"

newtype DrawStyle =
    DrawStyle Word8
    deriving (Eq, Show)

setStyle :: V.Style -> V.Style -> V.Style
setStyle a b = a .|. b

toggleStyle :: V.Style -> V.Style -> V.Style
toggleStyle a b =
    if hasStyle a b
    then clearStyle a b
    else setStyle a b

hasStyle :: V.Style -> V.Style -> Bool
hasStyle a b = a .&. b /= 0

clearStyle :: V.Style -> V.Style -> V.Style
clearStyle old dest = dest .&. complement old

noStyle :: V.Style
noStyle = 0

data LayerInfo =
    LayerInfo { _layerName :: String
              , _layerVisible :: Bool
              }

makeLenses ''LayerInfo

data AppState =
    AppState { _layers                  :: M.Map Int Canvas
             , _layerOrder              :: [Int]
             , _layerInfo               :: M.Map Int LayerInfo
             , _layerListVisible        :: Bool
             , _drawingOverlay          :: Canvas
             , _selectedLayerIndex      :: Int
             , _appCanvasSize           :: (Int, Int)
             , _modes                   :: [Mode]
             , _drawFgPaletteIndex      :: Int
             , _drawBgPaletteIndex      :: Int
             , _drawStyle               :: V.Style
             , _drawCharacter           :: Char
             , _tool                    :: Tool
             , _palette                 :: Vec.Vector (Maybe V.Color)
             , _fgPaletteSelectorExtent :: Maybe (Extent Name)
             , _bgPaletteSelectorExtent :: Maybe (Extent Name)
             , _toolSelectorExtent      :: Maybe (Extent Name)
             , _boxStyleSelectorExtent  :: Maybe (Extent Name)
             , _styleSelectorExtent     :: Maybe (Extent Name)
             , _canvasExtent            :: Maybe (Extent Name)
             , _dragging                :: Maybe (Name, Location, Location)
             , _layerNameEditor         :: Editor T.Text Name
             , _canvasSizeWidthEdit     :: Editor T.Text Name
             , _canvasSizeHeightEdit    :: Editor T.Text Name
             , _canvasSizeFocus         :: FocusRing Name
             , _canvasOffset            :: Location
             , _canvasPath              :: Maybe FilePath
             , _canvasDirty             :: Bool
             , _askToSaveFilenameEdit   :: Editor T.Text Name
             , _appEventChannel         :: BChan AppEvent
             , _textEntered             :: [(Char, V.Attr)]
             , _textEntryStart          :: (Int, Int)
             , _boxStyleIndex           :: Int
             , _eraserSize              :: Int
             , _repaintSize             :: Int
             , _restyleSize             :: Int
             , _undoStack               :: [[Action]]
             , _redoStack               :: [[Action]]
             }

makeLenses ''AppState

currentLayer :: Lens' AppState Canvas
currentLayer =
    lens (\s   -> fromJust $ s^.layers.at (s^.selectedLayerIndex))
         (\s c -> s & layers.at (s^.selectedLayerIndex) .~ Just c)

layerAt :: Int -> Lens' AppState Canvas
layerAt i =
    lens (\s   -> fromJust $ s^.layers.at i)
         (\s c -> s & layers.at i .~ Just c)

layerInfoFor :: Int -> Lens' AppState LayerInfo
layerInfoFor i =
    lens (\s   -> fromJust $ s^.layerInfo.at i)
         (\s v -> s & layerInfo.at i .~ Just v)

currentMode :: AppState -> Mode
currentMode s = case _modes s of
    (m:_) -> m
    _ -> error "BUG: currentMode: no modes!"
