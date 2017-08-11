module UI
  ( drawUI
  )
where

import Brick
import Lens.Micro.Platform

import Types
import UI.Main
import UI.CharacterSelect
import UI.PaletteEntrySelect
import UI.ToolSelect
import UI.CanvasSizePrompt
import UI.AskToSave
import UI.TextEntry

drawUI :: AppState -> [Widget Name]
drawUI s =
    case s^.mode of
        Main                 -> drawMainUI s
        FgPaletteEntrySelect -> drawPaletteEntrySelectUI s
        BgPaletteEntrySelect -> drawPaletteEntrySelectUI s
        ToolSelect           -> drawToolSelectUI s
        CharacterSelect      -> drawCharacterSelectUI s
        CanvasSizePrompt     -> drawCanvasSizePromptUI s
        AskToSave            -> drawAskToSaveUI s
        TextEntry            -> drawTextEntryUI s
