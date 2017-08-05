
- Hide-able HUD with indicators of drawing mode, character, attributes
- Change character used to draw: let user enter any character
- Palette (let user pick color of current drawing primitive) including
  RGB colors (maybe let the user enter RGB colors)
- Set canvas size (up to terminal size)?

- Eraser
- extra drawing primitives:
  - circles
  - lines (click, then click again)
  - fill?!
  - eyedropper (attr/character picker)
- Guide lines
- Save to disk, load from disk.
  - What format(s)? Are there any others? Probably needs to be RLE.
  - How to save so that the output can be used by other tools?
  - Need converter from this format to raw terminal output with escapes
- scroll around rather than truncating to terminal size
