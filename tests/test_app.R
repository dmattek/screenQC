source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")

setInputsFor <- function(session, ...) {
  base <- appInputs(selPlateFormat = "384", selSSMDctrl = "strong",
               rBscreenType = "inhibition", chBrobust = FALSE,
               slCtrlNegMn = 1.5, slCtrlNegSd = 0.1, slCtrlNegN = 8,
               slCtrlPosMn = 0.2, slCtrlPosSd = 0.1, slCtrlPosN = 8,
               slHitsMn = 0.5, slHitsSd = 0.1, slHitsN = 0, slHitsThr = -3)
  do.call(session$setInputs, modifyList(base, list(...)))
}

grab <- function(x) tryCatch({ paste(unlist(x), collapse=" ") },
                             error = function(e) paste("ERROR:", conditionMessage(e)))

testServer(APP_DIR, {
  cat("\n########## A. DEGENERATE CASE (the old crash) ##########\n")
  cat("both control SDs = 0, both means = 0.2\n\n")
  setInputsFor(session, slCtrlNegMn = 0.2, slCtrlNegSd = 0,
                        slCtrlPosMn = 0.2, slCtrlPosSd = 0)

  zp <- grab(output$vBzprime); ss <- grab(output$vBssmd)
  cat("vBzprime:", if (grepl("ERROR", zp)) zp else "rendered OK\n")
  cat("  contains 'n/a':", grepl("n/a", zp), "\n")
  cat("vBssmd  :", if (grepl("ERROR", ss)) ss else "rendered OK\n")
  cat("  contains 'n/a':", grepl("n/a", ss), "\n")
  stopifnot(!grepl("ERROR", zp), !grepl("ERROR", ss))
  stopifnot(grepl("n/a", zp), grepl("n/a", ss))
  cat("=> PASS: no crash, both boxes show n/a\n")

  cat("\n########## B. NORMAL CASE ##########\n")
  setInputsFor(session, slHitsN = 12)
  zp <- grab(output$vBzprime); ss <- grab(output$vBssmd)
  cat("vBzprime value:", regmatches(zp, regexpr("[-0-9.]+ (excellent|marginal|overlap)", zp)), "\n")
  cat("vBssmd   value:", regmatches(ss, regexpr("[-0-9.]+ (excellent|good|inferior|poor)", ss)), "\n")
  stopifnot(!grepl("ERROR", zp), !grepl("ERROR", ss), !grepl("n/a", zp), !grepl("n/a", ss))
  cat("=> PASS: normal values render with labels\n")

  cat("\n########## C. ALL FOUR HEATMAPS, ALL PLATE FORMATS ##########\n")
  for (fmt in c("96", "384", "1536")) {
    setInputsFor(session, selPlateFormat = fmt, slHitsN = 12)
    for (o in c("plotlyHMraw", "plotlyHMzscore", "plotlyHMnpi", "plotlyHMhits")) {
      r <- tryCatch({ p <- output[[o]]; nchar(paste(unlist(p), collapse="")) > 100 },
                    error = function(e) paste("ERROR:", conditionMessage(e)))
      cat(sprintf("  %-5s %-16s -> %s\n", fmt, o, r))
      stopifnot(isTRUE(r))
    }
  }
  cat("=> PASS: 12/12 heatmaps rendered\n")

  cat("\n########## D. ROBUST STATS + AGONIST ##########\n")
  setInputsFor(session, chBrobust = TRUE, rBscreenType = "activation", slHitsN = 20)
  for (o in c("vBzprime","vBssmd","plotlyHMraw","plotlyHMzscore","plotlyHMnpi","plotlyHMhits")) {
    r <- grab(output[[o]])
    cat(sprintf("  %-16s -> %s\n", o, if (grepl("ERROR", r)) r else "OK"))
    stopifnot(!grepl("ERROR", r))
  }
  cat("=> PASS\n")

  cat("\n########## E. dataPlate() NOT mutated across tabs ##########\n")
  setInputsFor(session, chBrobust = FALSE, rBscreenType = "inhibition", slHitsN = 12)
  invisible(output$plotlyHMzscore); invisible(output$plotlyHMnpi); invisible(output$plotlyHMhits)
  cat("columns of dataPlate() after rendering all tabs:", names(dataPlate()), "\n")
  # asserted as an absence rather than an exact column list, so that adding a
  # column to dataPlate() does not fail a test about by-reference leakage
  stopifnot(!any(c("zScore", "NPI", "hits") %in% names(dataPlate())))
  cat("=> PASS: no zScore/NPI/hits leaked into the shared reactive\n")
})
cat("\nALL CHECKS PASSED\n")
