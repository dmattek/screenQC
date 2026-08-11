# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

screenQC is a single-page R Shiny app that teaches quantitative concepts of high-content screening (z-score, NPI, Z-factor, Z-prime, SSMD). It generates **synthetic** plate data on the fly — there are no data files, no database, and no user upload path.

## Commands

```r
# Run locally (from the app folder)
R -e "shiny::runApp('.')"

# Or in RStudio: open screenQC.Rproj, then Run App from server.R / ui.R
```

There is no build step, test suite, or linter. Dependencies are auto-installed on startup by the check block at the top of `global.R`; add any new package to `required_packages` there *and* to the `install.packages` snippet in `README.md`.

Deployment target is shinyapps.io (`rsconnect/shinyapps.io/macdobry/screenQC.dcf`), published from RStudio.

## Architecture

Classic three-file Shiny layout, auto-sourced by Shiny in the order `global.R` → `ui.R` → `server.R`. Nothing is explicitly `source()`d, so anything `ui.R` needs (e.g. the `helpTextServer` list of tooltip/popover HTML) must be defined in `global.R`.

- **`global.R`** — package bootstrap, `helpTextServer` help strings, all statistics (`calcZfactor`, `calcZprime`, `calcSSMD` quality indices; `calcZscore`, `calcNPI` normalisations; `calcHits`), and the shared view helpers `plotPlateHM` / `clickableValueBox`.
- **`ui.R`** — `shinydashboard` page: config row, three parameter boxes (neg control / pos control / hits), a `tabBox` of plots, and two clickable `valueBox`es.
- **`server.R`** — reactive data generation and all renderers.

### Reactive data flow

```
dataCtrlNeg()  ─┐
dataCtrlPos()  ─┼─ rbind ─> dataMain() ─> dataPlotHM() ─> the 4 plotly heatmaps
dataSamples()  ─┘              │
                               └─> valueBoxes (Z-prime, SSMD), density & box plots
```

`dataMain()` is a two-column `data.table` (`type`, `meas`). Samples are drawn with the **negative control's** mean/sd; "hits" then overwrite `slHitsN` randomly chosen sample rows with the hit mean/sd. Sample count is `plateFormat - nCtrlPos - nCtrlNeg`, so control well counts eat into the sample wells.

`dataPlotHM()` `cbind`s a plate grid (`expand.grid` over row letters × column numbers, per 96/384/1536 format) onto `dataMain()` in row order. Because controls are `rbind`ed first, they always land in the first grid positions — the layout is positional, not label-driven.

### Conventions that cut across files

- **Well-type strings** `"Ctrl Neg"`, `"Ctrl Pos"`, `"Sample"` are the contract between `server.R` (which writes them into `type`) and every `calc*` function in `global.R` (which defaults `inWellType` to that list). Change one place and you must change both.
- **Shared `calc*` signature**: `inDT`, `inColMeas = "meas"`, `inColType = "type"`, `inWellType`, `inScreenType`, `inRobust`. `inRobust = TRUE` swaps mean/sd for median/mad throughout. `inScreenType` is `"inhibition"` (antagonist) or `"activation"` (agonist), validated with `match.arg`; it flips which control is the reference and the sign of the hit threshold comparison.
- **Naming**: input IDs are prefixed by widget type — `sl` slider, `sel` select, `rB` radio, `chB` checkbox, `al` actionLink, `vB` valueBox. Function arguments use `in*`, function-local variables use `loc*`.

### Shared view helpers

- **`plotPlateHM(inDT, inColVal, inHoverLabel, ...)`** renders any column of a `dataPlotHM()` table as a plotly heatmap (matrix reshape, row reversal, `type` hover matrix). All four heatmap tabs go through it. Extra `...` args pass straight to `plot_ly`; pass `inColors = NULL` to supply a custom `colorscale` instead of the default `hmColors` palette, as the Hits tab does.
- **`clickableValueBox(value, subtitle, color, id)`** builds a `valueBox` that doubles as an action button: it patches `children[[1]]$attribs` to `class = "action-button"` with id `button_<id>`. `observeEvent(input$button_<id>)` in `server.R` then opens the matching `bsModal` (`modal_<id>`) declared in `ui.R` — adding a clickable box means touching all three pieces.

### Gotchas

- `calcZscore`, `calcNPI`, and `calcHits` `copy()` their input and return a **new** table with the computed column added; always use the return value. They deliberately do not modify the caller, so the `dataPlotHM()` value shared by the four heatmap tabs stays clean.
- The quality indices are `NaN` when both controls have zero spread *and* equal means. The value boxes guard with `is.nan()` and show "n/a"; an unguarded `if (locZp > 0.5)` on `NaN` aborts the renderer with *"missing value where TRUE/FALSE needed"*.
- `output$vBzfactor` is still computed in `server.R`, but its `valueBoxOutput` is commented out in `ui.R` — the Z-factor box is not currently shown. It reads about -56 with defaults, because `dataSamples()` draws samples with the negative control's mean and SD, leaving no sample-vs-control separation.
- Every reactive/renderer starts with a `cat("server:<name>\n")` trace to stdout; keep that pattern when adding new ones.
