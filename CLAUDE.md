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

- **`global.R`** — package bootstrap, `helpTextServer` help strings, all statistics (`calcZfactor`, `calcZprime`, `calcSSMD` quality indices; `calcZscore`, `calcNPI` normalisations; `calcHits`, `calcHitPerf`), and the shared view helpers `plotPlateHM` / `clickableValueBox`.
- **`ui.R`** — `shinydashboard` page: config row, three parameter boxes (neg control / pos control / hits), a `tabBox` of plots, and two clickable `valueBox`es.
- **`server.R`** — reactive data generation and all renderers.

### Reactive data flow

```
seedBase() ─> dataCtrlNeg()  ─┐
           ─> dataCtrlPos()  ─┼─ rbind ─> dataMain() ─> dataPlate() ─> raw/zScore/NPI heatmaps
           ─> dataSamples()  ─┘                             │
                                                            ├─> dataHits() ─> hits heatmap
                                                            │            └─> confusion + rate tables
                                                            └─> valueBoxes (Z-prime, SSMD),
                                                                density & box plots
```

`dataPlate()` is the plate every consumer works from: it takes the stacked draws, positions the wells via `assignPlatePositions()` according to the chosen control layout, applies `addPlateArtifacts()`, then rounds. **Positioning lives here, not further downstream, on purpose** — the artifacts are position-dependent and have to reach the quality indices and distribution plots, not only the heatmaps. Anything that adds a position-dependent effect belongs in this reactive.

`dataMain()` is a three-column `data.table` (`type`, `meas`, `spiked`). Samples are drawn with the **negative control's** mean/sd; "hits" then overwrite `slHitsN` randomly chosen sample rows with the hit mean/sd and set `spiked = TRUE`. Sample count is `plateFormat - nCtrlPos - nCtrlNeg`, so control well counts eat into the sample wells — and on a 96-well plate with both control counts maxed there are fewer sample wells than the `#hits` slider allows, hence the `min()` clamp in `dataSamples()`.

`spiked` is the **ground truth** for hit calling, kept separate from `hits` (the *called* wells, thresholded on z-score by `calcHits`). `calcHitPerf()` scores one against the other on the Hits tab. It counts sample wells only — scoring the controls would credit the assay for recovering positive controls it was calibrated on — and returns `NA` rather than `NaN` for rates with an empty denominator, which the tables print as "n/a".

**Seeding.** `seedBase()` returns `10 * (input$nISeed + input$abResample)`, and each of the three draws calls `set.seed(seedBase() + k)` with its own `k`. Consequences worth knowing before touching it: the app is deterministic given (seed, parameters); clicking Resample bumps the base, yielding a fresh plate; and because `rnorm(n, m, s)` is `m + s*z` off a fixed `z` stream, changing a mean or SD slider *preserves the noise pattern* and shows only that parameter's effect. The `* 10` keeps the per-draw offsets from colliding with a neighbouring click's base — don't drop it, and keep `k` below 10. Note `set.seed()` sets global RNG state; all randomness in the app lives in these three reactives.

**Artifacts and layout — the non-obvious behaviour.** `addPlateArtifacts()` adds row/column gradients and an edge effect (ring 1 is the outer frame, decaying inwards) to *every* well, controls included. `ctrlPositions()` decides where controls sit: `first`, `edge`, or `spread`, each returning a full position ordering so an oversized control count spills over rather than failing — the edge columns of a 96-well plate cannot hold 64 controls.

What matters when reasoning about this: **Z-prime is a difference of two control means, so a bias shared equally by both controls cancels out of it entirely.** Controls at a uniform ring depth can be badly offset with Z-prime not moving at all; degradation tracks how many *distinct* rings the controls span. Measured on a 384 plate with 64 controls and `edge = 0.5`: `edge` layout spans rings {1,2} and loses 0.02, `first` spans {1,2,3,4} and loses 0.25, `spread` spans {1..8} and loses 0.57. The same trap applies per-axis — with `first` stacking controls down column 1, a *column* gradient cancels while a *row* gradient (negative control in rows 1-8, positive in 9-16) drives Z-prime from 0.52 to -0.22.

Meanwhile the artifact still inflates the sample spread, which is the z-score denominator, so hits stop clearing the threshold: at `edge = 0.5` with `first` layout, Z-prime holds at 0.515 while sensitivity drops from 92% to 38%. Don't "fix" a test that shows a flat Z-prime under an artifact — that is the app being correct, and it's the lesson the panel exists to teach.

`assignPlatePositions()` `setorder`s the result column-major (`colIdx`, `rowIdx`), which is the order `plotPlateHM()` reshapes into a matrix. Breaking that ordering silently scrambles every heatmap.

Row labels come from `plateRowLabels()`: `A`–`Z` then `AA`–`AF`, so a 1536-well plate's 32 rows end at `AF`. Note `LETTERS` only holds 26 entries — `LETTERS[1:32]` silently yields six `NA`s, which is an easy way to corrupt a 1536 plate or a test fixture.

### Help pop-overs

Use `helpLink(id, label, title, content)` from `global.R` for a clickable help label, with `helpLinkScript` included once in the body. It emits Bootstrap `data-*` attributes on the link itself.

**Do not go back to `shinyBS::addPopover` for these.** Its JS resolves the target as `$("#id").closest(".shiny-input-container, .shiny-bound-output, .btn, ...")`, which has two failure modes seen in this app: a link used as an input label binds the popover to the *whole control*, so clicking a dropdown to change its value pops the help open; and a link outside any input container (a `box(title = )`, an `h4()` in a `tabPanel`) matches nothing, so the popover silently never attaches — `alArtifacts` and `alHitPerf` were dead this way. `bsTooltip` shares the same target resolution, which is why it stays on plain inputs (`rBscreenType`, `chBrobust`, `abResample`) where binding to the whole control is what you want.

Pass the title via `data-original-title`, not `title`, or the browser shows a native tooltip on hover before Bootstrap initialises.

### Conventions that cut across files

- **Well-type strings** `"Ctrl Neg"`, `"Ctrl Pos"`, `"Sample"` are the contract between `server.R` (which writes them into `type`) and every `calc*` function in `global.R` (which defaults `inWellType` to that list). Change one place and you must change both.
- **Shared `calc*` signature**: `inDT`, `inColMeas = "meas"`, `inColType = "type"`, `inWellType`, `inScreenType`, `inRobust`. `inRobust = TRUE` swaps mean/sd for median/mad throughout. `inScreenType` is `"inhibition"` (antagonist) or `"activation"` (agonist), validated with `match.arg`; it flips which control is the reference and the sign of the hit threshold comparison.
- **Naming**: input IDs are prefixed by widget type — `sl` slider, `sel` select, `rB` radio, `chB` checkbox, `al` actionLink, `vB` valueBox. Function arguments use `in*`, function-local variables use `loc*`.

### Shared view helpers

- **`plotPlateHM(inDT, inColVal, inHoverLabel, ...)`** renders any column of a `dataPlotHM()` table as a plotly heatmap (matrix reshape, row reversal, `type` hover matrix). All four heatmap tabs go through it. Extra `...` args pass straight to `plot_ly`; pass `inColors = NULL` to supply a custom `colorscale` instead of the default `hmColors` palette, as the Hits tab does.
  - `inColMark` names a logical column whose wells get a marker overlaid as a second scatter trace, letting one plate carry two properties at once. The Hits tab uses it for `spiked`, so cell colour is the *called* hit and the circle is the *true* one. The trace needs `inherit = FALSE` or it picks up the heatmap's `z`/`text`; `%in% TRUE` guards against `NA`; and `inMarkSize` must shrink as the plate format grows (11/6/3 for 96/384/1536) because the cells do. The marker is a white fill with a dark rim deliberately — the two-colour hit palette is mid-dark blue and orange, so a plain dark outline would disappear on the blue.
- **`clickableValueBox(value, subtitle, color, id)`** builds a `valueBox` that doubles as an action button: it patches `children[[1]]$attribs` to `class = "action-button"` with id `button_<id>`. `observeEvent(input$button_<id>)` in `server.R` then opens the matching `bsModal` (`modal_<id>`) declared in `ui.R` — adding a clickable box means touching all three pieces.

### Gotchas

- `calcZscore`, `calcNPI`, and `calcHits` `copy()` their input and return a **new** table with the computed column added; always use the return value. They deliberately do not modify the caller, so the `dataPlotHM()` value shared by the four heatmap tabs stays clean.
- The quality indices are `NaN` when both controls have zero spread *and* equal means. The value boxes guard with `is.nan()` and show "n/a"; an unguarded `if (locZp > 0.5)` on `NaN` aborts the renderer with *"missing value where TRUE/FALSE needed"*.
- `output$vBzfactor` is still computed in `server.R`, but its `valueBoxOutput` is commented out in `ui.R` — the Z-factor box is not currently shown. It reads about -56 with defaults, because `dataSamples()` draws samples with the negative control's mean and SD, leaving no sample-vs-control separation.
- Every reactive/renderer starts with a `cat("server:<name>\n")` trace to stdout; keep that pattern when adding new ones.
