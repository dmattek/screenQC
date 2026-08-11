source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")

base <- appInputs(selSSMDctrl="strong", rBscreenType="inhibition", chBrobust=FALSE,
             slCtrlNegMn=1.5, slCtrlNegSd=0.1, slCtrlNegN=8,
             slCtrlPosMn=0.2, slCtrlPosSd=0.1, slCtrlPosN=8,
             slHitsMn=0.5, slHitsSd=0.1, slHitsN=8, slHitsThr=-2,
             nISeed=1, abResample=0, selCtrlLayout="first",
             slGradRow=0, slGradCol=0, slEdge=0, selPlateFormat="96")
setIn <- function(session,...) do.call(session$setInputs, modifyList(base, list(...)))
u <- function(x) unlist(x)

testServer(APP_DIR, {
  cat("### axis order on the Hits tab, per plate format\n")
  for (fmt in c("96","384","1536")) {
    for (lay in c("first","edge","spread")) {
      setIn(session, selPlateFormat=fmt, selCtrlLayout=lay, slHitsN=12)
      w  <- fromJSON(as.character(output$plotlyHMhits), simplifyVector=FALSE)
      hm <- w$x$data[[1]]
      xa <- u(w$x$layout$xaxis$categoryarray); ya <- u(w$x$layout$yaxis$categoryarray)
      hx <- u(hm$x); hy <- u(hm$y)
      nR <- length(hy); nC <- length(hx)

      colsOK <- identical(xa, as.character(seq_len(nC)))       # 1,2,3,... not 1,10,11
      rowsOK <- identical(ya, hy)                              # same order the z matrix assumes
      topA   <- tail(ya, 1) == "A"                             # last category = top of the y axis
      full   <- length(xa) == nC && length(ya) == nR           # every category present
      if (fmt == "96" && lay == "first") {
        cat("  x categoryarray:", paste(xa, collapse=","), "\n")
        cat("  y categoryarray:", paste(ya, collapse=","), " (last entry is the TOP of the axis)\n")
      }
      cat(sprintf("  %-5s %-6s cols 1..n=%s  rows match z=%s  A on top=%s  complete=%s\n",
                  fmt, lay, colsOK, rowsOK, topA, full))
      stopifnot(colsOK, rowsOK, topA, full)
    }
  }

  cat("\n### the other three tabs must be untouched (no category override)\n")
  setIn(session, selPlateFormat="96", slHitsN=12)
  for (o in c("plotlyHMraw","plotlyHMzscore","plotlyHMnpi")) {
    w <- fromJSON(as.character(output[[o]]), simplifyVector=FALSE)
    has <- !is.null(w$x$layout$xaxis$categoryarray) || !is.null(w$x$layout$yaxis$categoryarray)
    cat(sprintf("  %-16s categoryarray present: %s (want FALSE)\n", o, has))
    stopifnot(!has)
  }

  cat("\n### markers still land on the right cells\n")
  for (fmt in c("96","384","1536")) {
    setIn(session, selPlateFormat=fmt, slHitsN=12, slHitsThr=-2)
    p  <- calcHitPerf(dataHits())
    w  <- fromJSON(as.character(output$plotlyHMhits), simplifyVector=FALSE)
    hm <- w$x$data[[1]]; mk <- w$x$data[[2]]
    xs <- u(hm$x); ys <- u(hm$y)
    z  <- do.call(rbind, lapply(hm$z, function(r) vapply(r, function(e) if (is.null(e)) NA_real_ else as.numeric(e), numeric(1))))
    onT <- sum(vapply(seq_along(mk$x), function(k)
      isTRUE(z[match(as.character(mk$y[[k]]), ys), match(as.character(mk$x[[k]]), xs)] == 1), logical(1)))
    cat(sprintf("  %-5s markers-on-T=%d TP=%d ok=%s\n", fmt, onT, p$TP, onT==p$TP))
    stopifnot(onT == p$TP)
  }
})
cat("\nAXIS ORDER CHECKS PASSED\n")
