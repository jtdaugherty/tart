
0.3
---

Package changes:
 * Added `libonly` flag (defaults to `False`) to control whether to
   build the `tart` executable.
 * Raised the `brick` upper bound.
 * Raised the `vty` upper bound.

0.2
---

`tart` tool changes:
 * When inserting a new layer, also select it and make that selection
   part of the undo action.
 * Attribute style selector now uses more descriptive labels for style
   options.

Tart library changes:
 * Added a dependency on the `text` package.
 * The type of some fields was changed from `String` to
   `Text` (`TartFile` / `tartFileCanvasNames`, `TartFileDataV1`
   / `tartFileDataV1CanvasNames`, `TartFileDataV2` /
   `tartFileDataV2CanvasNames`)
 * Added `Tart.Canvas.canvasFromString` and changed `canvasFromText` to
   take a `Text` input rather than a `String`.
 * Relaxed bounds on `vty`.

0.1.2
-----

Bug fixes:
 * Exceptions triggered during an attempt to save to an invalid path no
   longer cause a crash and are now reported to the user (#4)
 * Canvas size dialog fields are now clickable (#5)

0.1.1
-----

 * Added a new saving UI and keybinding to trigger it (C-s). Previously
   the only opportunity to save was on quitting.

0.1
---

* First version.
