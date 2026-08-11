source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")

base <- appInputs(selCtrlLayout="first", slGradRow=0, slGradCol=0, slEdge=0,
             selPlateFormat="384", selSSMDctrl="strong", rBscreenType="inhibition",
             chBrobust=FALSE, slCtrlNegMn=1.5, slCtrlNegSd=0.1, slCtrlNegN=8,
             slCtrlPosMn=0.2, slCtrlPosSd=0.1, slCtrlPosN=8,
             slHitsMn=0.5, slHitsSd=0.1, slHitsN=24, slHitsThr=-3,
             nISeed=1, abResample=0)
setIn <- function(session, ...) do.call(session$setInputs, modifyList(base, list(...)))
chr <- function(v) vapply(v, as.character, character(1))

testServer(APP_DIR, {
  cat("### markers on the plate must reconcile with the confusion matrix\n")
  cat("   fmt   thr    TP   FP   FN | mark-on-T  mark-on-F  T-unmarked\n")
  for (fmt in c("96","384","1536")) for (thr in c(-4,-3,-2,0)) {
    setIn(session, selPlateFormat=fmt, slHitsThr=thr, slHitsN=24)
    p <- calcHitPerf(dataHits())

    w  <- fromJSON(as.character(output$plotlyHMhits), simplifyVector=FALSE)
    tr <- w$x$data
    hm <- tr[[1]]
    xs <- chr(hm$x); ys <- chr(hm$y)
    z  <- do.call(rbind, lapply(hm$z, function(r) vapply(r, function(e) if (is.null(e)) NA_real_ else as.numeric(e), numeric(1))))
    mk <- if (length(tr) > 1) tr[[2]] else list(x=list(), y=list())

    onT <- 0; onF <- 0
    for (k in seq_along(mk$x)) {
      zc <- z[match(as.character(mk$y[[k]]), ys), match(as.character(mk$x[[k]]), xs)]
      if (isTRUE(zc == 1)) onT <- onT + 1 else onF <- onF + 1
    }
    d <- dataHits(); nTsample <- nrow(d[type=="Sample" & hits %in% TRUE])
    tNoMark <- nTsample - onT

    cat(sprintf("  %-5s %4d  %4d %4d %4d | %8d  %9d  %10d\n",
                fmt, thr, p$TP, p$FP, p$FN, onT, onF, tNoMark))
    stopifnot(onT == p$TP, onF == p$FN, tNoMark == p$FP)
  }
  cat("\n  PASS: mark-on-T == TP, mark-on-F == FN, unmarked T == FP, in every case\n")
})
cat("\nMARKER/TABLE RECONCILIATION PASSED\n")
