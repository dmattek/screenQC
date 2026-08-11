#
# screenQC: shared setup for the test scripts
# Author: Maciej Dobrzynski
#
# Run from the repository root:
#
#   Rscript tests/run_all.R           # every test file
#   Rscript tests/test_layout.R       # a single one
#
# The tests are plain R: they assert with stopifnot() and print what they
# checked, so a run is readable as a report. No test framework is required
# beyond the packages the app itself already needs.
#

## Locating the app ----

findAppDir = function() {

  locDir = normalizePath(getwd())

  # tolerate being run from the repository root or from tests/
  for (locUp in 1:3) {
    if (file.exists(file.path(locDir, "global.R")))
      return(locDir)

    locDir = dirname(locDir)
  }

  stop("cannot find global.R; run the tests from the repository root")
}

APP_DIR = findAppDir()


## Loading the app ----

# global.R installs any missing packages when it is sourced, which a test run
# has no business doing. Skip that bootstrap block and evaluate from the first
# library() call onwards. Anchoring on library() rather than on a section
# heading keeps this working when the sections are renamed or moved, which is
# what broke an earlier version of this helper.
loadApp = function() {

  locSrc = readLines(file.path(APP_DIR, "global.R"))
  locStart = min(grep("^library\\(", locSrc))

  eval(parse(text = paste(locSrc[locStart:length(locSrc)], collapse = "\n")),
       envir = globalenv())

  invisible(TRUE)
}

suppressMessages({
  library(shiny)
  library(jsonlite)
  loadApp()
})


## Default inputs ----

# testServer does not apply the defaults declared in ui.R, so every input the
# server reads has to be named here. Tests then state only what they vary,
# and a newly added input needs updating in one place rather than in each
# file -- forgetting that is what silently broke several tests when the plate
# artifact sliders were introduced.
appInputs = function(...) {

  locDefaults = list(
    selPlateFormat = "384",
    selSSMDctrl    = "strong",
    rBscreenType   = "inhibition",
    chBrobust      = FALSE,
    selCtrlLayout  = "first",

    nISeed     = 1,
    abResample = 0,

    slCtrlNegMn = 1.5, slCtrlNegSd = 0.1, slCtrlNegN = 8,
    slCtrlPosMn = 0.2, slCtrlPosSd = 0.1, slCtrlPosN = 8,
    slHitsMn    = 0.5, slHitsSd    = 0.1, slHitsN    = 0,

    slGradRow = 0, slGradCol = 0, slEdge = 0,

    selHitMetric = "zscore",
    slHitsThr    = -3
  )

  return(modifyList(locDefaults, list(...)))
}


## Small assertion helpers ----

okMark = function(inOk) {
  if (inOk) return("ok") else return("FAIL")
}
