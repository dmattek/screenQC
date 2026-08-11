source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")

base <- appInputs(selPlateFormat="384", selSSMDctrl="strong", rBscreenType="inhibition",
             chBrobust=FALSE, slCtrlNegMn=1.5, slCtrlNegSd=0.1, slCtrlNegN=8,
             slCtrlPosMn=0.2, slCtrlPosSd=0.1, slCtrlPosN=8,
             slHitsMn=0.5, slHitsSd=0.1, slHitsN=24, slHitsThr=-3,
             nISeed=1, abResample=0, selCtrlLayout="first",
             slGradRow=0, slGradCol=0, slEdge=0)
setIn <- function(session,...) do.call(session$setInputs, modifyList(base, list(...)))
chr <- function(v) vapply(v, as.character, character(1))

testServer(APP_DIR, {
  cat("\n### A. all outputs render across formats x layouts\n")
  for (fmt in c("96","384","1536")) for (lay in c("first","edge","spread")) {
    setIn(session, selPlateFormat=fmt, selCtrlLayout=lay, slHitsN=12, slGradCol=0.3, slEdge=0.2)
    ok <- TRUE
    for (o in c("vBzprime","vBssmd","plotDist","plotBox","plotlyHMraw","plotlyHMzscore",
                "plotlyHMnpi","plotlyHMhits","tabHitsConf","tabHitsPerf")) {
      r <- tryCatch(nchar(paste(unlist(output[[o]]),collapse=""))>50,
                    error=function(e) paste("ERR",o,conditionMessage(e)))
      if (!isTRUE(r)) { cat("   FAIL", fmt, lay, o, r, "\n"); ok <- FALSE }
    }
    cat(sprintf("  %-5s %-6s -> %s\n", fmt, lay, ok)); stopifnot(ok)
  }

  cat("\n### B. plate is complete and correctly sized\n")
  for (fmt in c("96","384","1536")) {
    setIn(session, selPlateFormat=fmt)
    d <- dataPlate()
    cat(sprintf("  %-5s rows=%-5d unique positions=%-5d labels ok=%s\n", fmt, nrow(d),
        length(unique(paste(d$row,d$colIdx))), !any(is.na(d$row))))
    stopifnot(nrow(d)==as.integer(fmt), length(unique(paste(d$row,d$colIdx)))==nrow(d), !any(is.na(d$row)))
  }

  cat("\n### C. Z-prime responds to how UNEVENLY the artifact hits the controls\n")
  cat("   (a bias shared equally by both controls cancels in a difference of means)\n")
  cat("   layout  ctrl rings          Zprime(no edge)  Zprime(edge=0.5)\n")
  res <- list()
  for (lay in c("first","edge","spread")) {
    setIn(session, selPlateFormat="384", selCtrlLayout=lay, slCtrlNegN=32, slCtrlPosN=32,
          slEdge=0, slHitsN=0)
    z0 <- calcZprime(dataPlate())
    d  <- dataPlate(); rings <- sort(unique(d[type!="Sample", pmin(rowIdx,16-rowIdx+1,colIdx,24-colIdx+1)]))
    setIn(session, selPlateFormat="384", selCtrlLayout=lay, slCtrlNegN=32, slCtrlPosN=32,
          slEdge=0.5, slHitsN=0)
    z1 <- calcZprime(dataPlate())
    res[[lay]] <- list(z0=z0, z1=z1, nring=length(rings))
    cat(sprintf("   %-6s  {%-18s} %14.3f %17.3f\n", lay, paste(rings,collapse=","), z0, z1))
  }
  # baseline identical across layouts
  stopifnot(abs(res$first$z0 - res$edge$z0) < 1e-9, abs(res$first$z0 - res$spread$z0) < 1e-9)
  # degradation is ordered by how many distinct rings the controls span
  ord <- order(sapply(res, function(r) r$nring))
  degr <- sapply(res, function(r) r$z0 - r$z1)[ord]
  cat("  ring spans ascending:", paste(names(degr), collapse=" < "), "\n")
  cat("  Z-prime loss, same order:", paste(sprintf("%.3f", degr), collapse=" < "), "\n")
  stopifnot(all(diff(degr) > 0))
  cat("  PASS: the more rings the controls span, the more Z-prime falls\n")

  cat("\n### C2. artifacts cost sensitivity even where Z-prime is untouched\n")
  setIn(session, selPlateFormat="384", selCtrlLayout="first", slCtrlNegN=8, slCtrlPosN=8,
        slHitsN=24, slHitsThr=-3, slEdge=0)
  zpA <- calcZprime(dataPlate()); sdA <- dataPlate()[type=="Sample", sd(meas)]; pA <- calcHitPerf(dataHits())
  setIn(session, selPlateFormat="384", selCtrlLayout="first", slCtrlNegN=8, slCtrlPosN=8,
        slHitsN=24, slHitsThr=-3, slEdge=0.5)
  zpB <- calcZprime(dataPlate()); sdB <- dataPlate()[type=="Sample", sd(meas)]; pB <- calcHitPerf(dataHits())
  cat(sprintf("  edge=0.0  Zprime=%.6f  sampleSD=%.4f  TPR=%.1f%%\n", zpA, sdA, 100*pA$TPR))
  cat(sprintf("  edge=0.5  Zprime=%.6f  sampleSD=%.4f  TPR=%.1f%%\n", zpB, sdB, 100*pB$TPR))
  cat(sprintf("  Zprime delta = %.2e (signif(meas,4) rounds differently once values shift)\n",
              abs(zpA - zpB)))
  stopifnot(abs(zpA - zpB) < 1e-3)   # Z-prime says nothing is wrong
  stopifnot(sdB > sdA)               # but the sample spread has grown
  stopifnot(pB$TPR < pA$TPR)         # and real hits are being missed
  cat("  PASS: Z-prime unchanged, sample spread up, sensitivity down\n")

  cat("\n### D. artifacts reach the quality indices (not just the heatmaps)\n")
  cat("   'first' layout stacks the controls down column 1, negative in rows 1-8 and\n")
  cat("   positive in rows 9-16, so a ROW gradient pulls the two groups apart while a\n")
  cat("   COLUMN gradient moves them together and cancels.\n")
  setIn(session, selCtrlLayout="first", slCtrlNegN=8, slCtrlPosN=8,
        slGradRow=0, slGradCol=0, slEdge=0)
  z0 <- calcZprime(dataPlate())
  setIn(session, selCtrlLayout="first", slCtrlNegN=8, slCtrlPosN=8,
        slGradRow=0, slGradCol=0.8, slEdge=0)
  zCol <- calcZprime(dataPlate())
  setIn(session, selCtrlLayout="first", slCtrlNegN=8, slCtrlPosN=8,
        slGradRow=0.8, slGradCol=0, slEdge=0)
  zRow <- calcZprime(dataPlate())
  cat(sprintf("  Z-prime  flat=%.4f   column gradient=%.4f   row gradient=%.4f\n", z0, zCol, zRow))
  stopifnot(abs(zCol - z0) < 1e-3)          # uniform across the controls -> cancels
  stopifnot(abs(zRow - z0) > 0.05)          # varies across the controls -> bites
  cat("  PASS: a position-dependent effect reaches an index computed off dataPlate()\n")

  cat("\n### E. colour scales are pinned and stable\n")
  getz <- function(o) { w <- fromJSON(as.character(output[[o]]), simplifyVector=FALSE)$x$data[[1]]
                        c(w$zmin, w$zmax) }
  setIn(session, slHitsN=24, slHitsThr=-3, abResample=0)
  a <- lapply(c("plotlyHMraw","plotlyHMzscore","plotlyHMnpi","plotlyHMhits"), getz)
  setIn(session, slHitsN=24, slHitsThr=-5, abResample=3)   # resample + move threshold
  b <- lapply(c("plotlyHMraw","plotlyHMzscore","plotlyHMnpi","plotlyHMhits"), getz)
  for (i in seq_along(a)) cat(sprintf("  %-16s [%s, %s] -> [%s, %s]  stable=%s\n",
      c("raw","zScore","NPI","hits")[i], a[[i]][1], a[[i]][2], b[[i]][1], b[[i]][2],
      identical(a[[i]], b[[i]])))
  stopifnot(identical(a,b))
  cat("  PASS: resampling and moving the threshold no longer rescale the colours\n")

  cat("\n### F. #hits slider cap tracks available sample wells\n")
  setIn(session, selPlateFormat="96", slCtrlNegN=32, slCtrlPosN=32, slHitsN=36)
  d <- dataPlate()
  cat("  96-well, 64 controls -> sample wells:", nrow(d[type=="Sample"]),
      " spiked:", sum(d$spiked), "\n")
  stopifnot(sum(d$spiked) == 32)

  cat("\n### G. spiked markers still land on the right cells after reordering\n")
  for (fmt in c("96","384","1536")) for (lay in c("first","edge","spread")) {
    setIn(session, selPlateFormat=fmt, selCtrlLayout=lay, slCtrlNegN=8, slCtrlPosN=8,
          slHitsN=12, slHitsThr=-2, slEdge=0.2)
    p  <- calcHitPerf(dataHits())
    w  <- fromJSON(as.character(output$plotlyHMhits), simplifyVector=FALSE)
    tr <- w$x$data; hm <- tr[[1]]
    xs <- chr(hm$x); ys <- chr(hm$y)
    z  <- do.call(rbind, lapply(hm$z, function(r) vapply(r, function(e) if (is.null(e)) NA_real_ else as.numeric(e), numeric(1))))
    mk <- if (length(tr)>1) tr[[2]] else list(x=list(),y=list())
    onT <- 0
    for (k in seq_along(mk$x)) {
      zc <- z[match(as.character(mk$y[[k]]), ys), match(as.character(mk$x[[k]]), xs)]
      if (isTRUE(zc==1)) onT <- onT + 1
    }
    ok <- (onT == p$TP) && (length(mk$x) == sum(dataPlate()$spiked))
    cat(sprintf("  %-5s %-6s markers=%-3d TP=%-3d aligned=%s\n", fmt, lay, length(mk$x), p$TP, ok))
    stopifnot(ok)
  }
})
cat("\nALL INTEGRATION CHECKS PASSED\n")
