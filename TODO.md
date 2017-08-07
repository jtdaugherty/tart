
- More efficient pixel updates
- Let user pick RGB colors
- Support "no background color" (i.e., use the default background)

- Guide lines?

- Save to disk, load from disk.
  - What format(s)? Are there any others? Probably needs to be RLE.
  - How to save so that the output can be used by other tools?
  - Need converter from this format to raw terminal output with escapes,
    as well as to version without color escapes

- Extra drawing primitives:
  - circles
  - rectangles (click and drag)
  - lines (click and drag)
  - fill?!
  - eyedropper (attr/character picker)

- Undo / redo

- Layers

- Scroll around rather than truncating to terminal size (WASD), resize
  the canvas array only when the user draws a pixel outside the current
  range
- Set canvas size (up to terminal size)?
