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
import UI.AskForSaveFilename
import UI.TextEntry
import UI.BoxStyleSelect
import UI.StyleSelect

drawUI :: AppState -> [Widget Name]
drawUI s =
    concat $ drawMode s <$> s^.modes

drawMode :: AppState -> Mode -> [Widget Name]
drawMode s m =
    case m of
        Main                 -> drawMainUI s
        RenameLayer          -> []
        FgPaletteEntrySelect -> drawPaletteEntrySelectUI s
        BgPaletteEntrySelect -> drawPaletteEntrySelectUI s
        ToolSelect           -> drawToolSelectUI s
        CharacterSelect      -> drawCharacterSelectUI s
        CanvasSizePrompt     -> drawCanvasSizePromptUI s
        AskToSave            -> drawAskToSaveUI s
        AskForSaveFilename q -> drawAskForSaveFilenameUI q s
        TextEntry            -> drawTextEntryUI s
        BoxStyleSelect       -> drawBoxStyleSelectUI s
        StyleSelect          -> drawStyleSelectUI s
