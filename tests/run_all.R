#!/usr/bin/env Rscript
#
# screenQC: run every test file
#
#   Rscript tests/run_all.R          # summary only
#   Rscript tests/run_all.R -v       # full output of each file
#
# Each file runs in its own R process. The tests define throwaway helpers at
# top level (mk, run, probe, ...) and several stub session methods, so
# sourcing them into one session would let them clash.
#

locArgs = commandArgs(trailingOnly = TRUE)
locVerbose = any(locArgs %in% c("-v", "--verbose"))

locDir = if (file.exists("tests/helper.R")) "tests" else "."

locFiles = sort(list.files(locDir,
                           pattern = "^test_.*\\.R$",
                           full.names = TRUE))

if (length(locFiles) == 0)
  stop("no test files found; run from the repository root")

cat(sprintf("running %d test files\n\n", length(locFiles)))

locFail = character(0)
locStart = Sys.time()

for (locF in locFiles) {

  locT0 = Sys.time()
  locOut = suppressWarnings(
    system2("Rscript", locF, stdout = TRUE, stderr = TRUE))

  locStatus = attr(locOut, "status")
  locOk = is.null(locStatus) || locStatus == 0

  # a test file can exit 0 and still have printed an error
  if (any(grepl("^Error", locOut)))
    locOk = FALSE

  if (!locOk)
    locFail = c(locFail, basename(locF))

  cat(sprintf("  %-4s %-22s %5.1fs\n",
              if (locOk) "PASS" else "FAIL",
              basename(locF),
              as.numeric(difftime(Sys.time(), locT0, units = "secs"))))

  if (locVerbose || !locOk) {
    locShow = if (locVerbose) locOut else grep("Error|not TRUE|FAIL", locOut, value = TRUE)
    cat(paste0("       ", locShow, collapse = "\n"), "\n")
  }
}

cat(sprintf("\n%d passed, %d failed  (%.0fs)\n",
            length(locFiles) - length(locFail),
            length(locFail),
            as.numeric(difftime(Sys.time(), locStart, units = "secs"))))

if (length(locFail) > 0) {
  cat("failed:", paste(locFail, collapse = ", "), "\n")
  quit(status = 1)
}
