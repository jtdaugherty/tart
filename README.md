tart - terminal art program
===========================

**NOTE: This is unreleased software and is under heavy development. Use at your own risk.**

```
 _____  _    ____ _____  _
(_   _)/ \  |    (_   _)| |
  | | / ^ \ |  O  || |  |_|
  | |/ ___ \|  _ < | |   _
  |_|_/   \_|_| \_\|_|  |_|
```

Tart is a program that provides an image-editor-like interface to
creating ASCII art - in the terminal, with your mouse!

![](screenshots/2.png)

Building
========

`tart` is a Haskell project. You'll need
[GHC](https://www.haskell.org/ghc/) (preferably at least 8.2) and
[cabal-install](http://hackage.haskell.org/package/cabal-install)
(preferably at least 2.0). Then:

```
$ cabal update
$ git clone https://github.com/jtdaugherty/tart.git
$ cd tart
$ cabal new-build
$ $(find . -name tart -type f)
```

Features
========

- Drawing tools: freehand, line, box, flood fill, text string
- Utility tools: repaint, restyle, eyedropper, eraser
- Multiple graphical styles for boxes
- Named image layers with reordering, visibility toggling
- Character selection for freehand and flood fill tools
- Set foreground color, background color, and text style independently
- Full mouse interaction and keyboard shortcuts
- Paste text from clipboard into canvas
- Undo and redo
- Text styles: bold, blink, underline, reverse video
- Load and save ASCII art files (binary)
- Save plain versions of ASCII art for embedding in documents
- Save color versions of ASCII art with terminal escape sequences for
  printing to terminals
- Import existing plaintext files as the basis for new ASCII art files
- Set arbitrary canvas size

Terminal Emulator Support
=========================

`tart` has been tested extensively with the following terminal emulators
and is known to work well with them:

 * OS X: `iTerm2`
 * OS X: `Terminal.app`

Please let me know if you use `tart` with another emulator and let me
know how well it works!

Keybindings
===========

Tools / styles:
- `0`..`9`: select tool
- `y`: open the attribute style selector
- `!`/`@`/`#`/`$`: select attribute style
- `f`/`b`: open foreground / background palette selectors
- `c`: set tool drawing character (where applicable)
- `<`/`>`: decrease / increase tool size (where applicable)
- `Esc`: cancel tool drag (e.g. box)

Canvas:
- `w`/`a`/`s`/`d`: move canvas
- `C`: re-center canvas
- `v`: set canvas size
- `-`/`+`: decrease / increase canvas size

Layers:
- `C-a`: add new layer
- `C-r`: rename current layer
- `C-n`/`C-p`: select next/previous layer
- `C-x`: delete selected layer
- `C-u`/`C-d`: move current layer up / down
- `C-v`: toggle selected layer's visibility
- `C-l`: toggle visibility of layer list

General:
- `q`: quit
- `u`: undo
- `r`: redo
- OS paste: paste text into canvas

How It Works
============

Tart requires a terminal with mouse support. You use various tools (such
as freehand drawing, boxes, etc.) to draw ASCII pictures. You can set a
current foreground and background color. You can also resize the drawing
canvas to get the desired output size. When you're finished, you can
save to disk, at which point Tart creates three files:

 * A binary file (say `foo`) suitable for reloading with Tart for
   further editing later
 * A text file `foo.color.txt` containing the ASCII art with terminal
   color escape sequences, suitable for emitting to terminals
 * A text file `foo.plain.txt` containing the ASCII art without terminal
   color escape sequences, suitable for embedding in documentation

Contributing
============

If you decide to contribute, that's great! Here are some guidelines you
should consider to make submitting patches easier for all concerned:

 - If you want to take on big things, talk to me first; let's have a
   design/vision discussion before you start coding. Create a GitHub
   issue and we can use that as the place to hash things out.
 - Please make changes consistent with the conventions I've used in the
   codebase.
 - Please adjust or provide Haddock and/or user guide documentation
   relevant to any changes you make.
