
- Better quit/save UX ('q' -> 'y/n' instead of 'q' -> 'Esc')

- Reuse drawing code for overlay (draw into separate canvas, render with
  transparency, then merge) - requires having a separate buffer that
  gets used for such operations and cleared in between updates

- Let user pick RGB colors

- Extra drawing primitives:
  - unicode style box in addition to ascii box
  - circles
  - lines (click and drag)
  - fill?!
  - eyedropper (attr/character picker)
  - text strings

- Undo / redo

- Layers
