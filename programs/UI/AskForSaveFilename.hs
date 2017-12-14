{-# LANGUAGE OverloadedStrings #-}
module UI.AskForSaveFilename
  ( drawAskForSaveFilenameUI
  )
where

import qualified Data.Text as T
import Lens.Micro.Platform

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Edit

import Types
import Theme

drawAskForSaveFilenameUI :: Bool -> AppState -> [Widget Name]
drawAskForSaveFilenameUI isQuitting s = [drawPromptWindow isQuitting s]

drawPromptWindow :: Bool -> AppState -> Widget Name
drawPromptWindow isQuitting s =
    centerLayer $
    borderWithLabel (str "Save") $
        hLimit 60 $
        padLeftRight 2 $ padTopBottom 1 body
    where
        help = if isQuitting
                  then hBox [ str "("
                            , withDefAttr keybindingAttr $ str "Enter"
                            , str " to save and quit, "
                            , withDefAttr keybindingAttr $ str "Esc"
                            , str " to quit without saving)"
                            ]
                  else hBox [ str "("
                            , withDefAttr keybindingAttr $ str "Enter"
                            , str " to save, "
                            , withDefAttr keybindingAttr $ str "Esc"
                            , str " to cancel)"
                            ]
        body = maybeError <=>
               (hCenter $ str "Save changes to:") <=>
               (hCenter help) <=>
               padTopBottom 1 fn
        maybeError = maybe emptyWidget mkSaveError (s^.saveError)
        mkSaveError msg = withDefAttr errorAttr $
                          (hCenter $ txt "Error saving:") <=>
                          (padBottom (Pad 1) $ txtWrap msg)
        renderString = txt . T.unlines
        fn = str "Path: " <+> renderEditor renderString True (s^.askToSaveFilenameEdit)
