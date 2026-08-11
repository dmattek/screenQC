# Tests

```
Rscript tests/run_all.R          # all of them, one line per file
Rscript tests/run_all.R -v       # with each file's full output
Rscript tests/test_layout.R      # a single file
```

Run from the repository root. Exit status is 1 if anything fails.

These are plain R scripts: they assert with `stopifnot()` and print what they
checked, so a run reads as a report rather than a row of dots. No test
framework is needed beyond the packages the app already requires.
`helper.R` loads `global.R` while skipping its package-installer block, and
supplies `appInputs()`, one complete set of inputs so a test states only what
it varies — `testServer()` does not apply the defaults declared in `ui.R`.

## What each file guards

| file | covers |
| --- | --- |
| `test_layout.R` | plate geometry: row labels, every layout using each position once, edge layout spilling inwards when controls exceed the border, column-major ordering, gradient/edge-effect magnitudes |
| `test_perf.R` | `calcHitPerf()` counts against hand-built tables with known TP/FP/FN/TN, controls excluded, `NA` on empty denominators |
| `test_metric.R` | the three hit metrics select identical wells at equivalent thresholds, and the NPI comparison is inverted in both screen types |
| `test_mark.R` | spiked-well markers: one per well, correct cell alignment on 96/384/1536, `NA` never marks, heatmap layer unchanged by the overlay |
| `test_equiv.R` | `plotPlateHM()` reproduces the pre-refactor inline heatmap code exactly in `x`/`y`/`z`/`text` |
| `test_app.R` | the degenerate zero-spread plate renders "n/a" instead of crashing the value boxes; no by-reference leakage into `dataPlate()` |
| `test_seed.R` | determinism: same seed reproduces a plate, Resample advances it, and changing a parameter preserves the noise realisation |
| `test_hits.R` | ground truth recorded, the 96-well clamp, confusion counts covering exactly the sample wells, threshold sweep monotonicity |
| `test_int2.R` | artifacts and layouts end to end, including that Z-prime tracks how unevenly an artifact hits the controls and can stay flat while sensitivity collapses |
| `test_axes.R` | the Hits heatmap axis order — columns `1..n` not `1,10,11`, row `A` on top — and that the other tabs get no category override |
| `test_style.R` | marker styling actually emitted per plate format, and legend placement |
| `test_mark_int.R` | markers on the plate reconcile with the confusion matrix across formats and thresholds |
| `test_metric_int.R` | the metric selector re-ranges the threshold slider, and every metric renders |
| `test_csv.R` | the download handler: long format, one row per well, both normalisations present whatever metric is selected, file name records seed and resample |

## Notes

Two things these tests learned the hard way, worth keeping in mind when
adding more:

- Checking the plotly **trace data** is not enough. `x`, `y`, `z` and the
  marker coordinates all stayed mutually consistent while the Hits plate
  rendered with scrambled axes; the corruption was in `layout$xaxis` /
  `layout$yaxis`. `test_axes.R` asserts on those.
- `testServer()` supplies no defaults from `ui.R`. When a new input is added
  and `appInputs()` is not updated, tests fail inside the server with an
  obscure error about the missing value rather than a clear message.
