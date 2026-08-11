source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")

base <- appInputs(selPlateFormat="384", selSSMDctrl="strong", rBscreenType="inhibition",
             chBrobust=FALSE, slCtrlNegMn=1.5, slCtrlNegSd=0.1, slCtrlNegN=8,
             slCtrlPosMn=0.2, slCtrlPosSd=0.1, slCtrlPosN=8,
             slHitsMn=0.5, slHitsSd=0.1, slHitsN=24, slHitsThr=-2,
             nISeed=1, abResample=0)
setIn <- function(session,...) do.call(session$setInputs, modifyList(base, list(...)))

testServer(APP_DIR, {
  cat("### marker styling actually emitted, per plate format\n")
  for (fmt in c("96","384","1536")) {
    setIn(session, selPlateFormat=fmt)
    w  <- fromJSON(as.character(output$plotlyHMhits), simplifyVector=FALSE)
    mk <- w$x$data[[2]]; m <- mk$marker
    cat(sprintf("  %-5s symbol=%-7s size=%-3s fill=%-22s rim=%s/%s  legend='%s' show=%s\n",
        fmt, m$symbol, m$size, m$color, m$line$color, m$line$width, mk$name, mk$showlegend))
  }
  cat("\n### underlying cell colours the marker must contrast against\n")
  cs <- fromJSON(as.character(output$plotlyHMhits), simplifyVector=FALSE)$x$data[[1]]$colorscale
  cat("  colorscale stops:", paste(sapply(cs, function(s) paste0(s[[1]],"=",s[[2]])), collapse="  "), "\n")

  cat("\n### legend placement (must clear the colourbar on the right)\n")
  lg <- fromJSON(as.character(output$plotlyHMhits), simplifyVector=FALSE)$x$layout$legend
  cat(sprintf("  orientation=%s  x=%s  y=%s\n", lg$orientation, lg$x, lg$y))

  cat("\n### other tabs must NOT have gained a marker trace\n")
  for (o in c("plotlyHMraw","plotlyHMzscore","plotlyHMnpi")) {
    n <- length(fromJSON(as.character(output[[o]]), simplifyVector=FALSE)$x$data)
    cat(sprintf("  %-16s traces=%d %s\n", o, n, if (n==1) "OK" else "UNEXPECTED"))
    stopifnot(n == 1)
  }
})
