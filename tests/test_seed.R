source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")

base <- appInputs(selCtrlLayout="first", slGradRow=0, slGradCol=0, slEdge=0,
             selPlateFormat = "384", selSSMDctrl = "strong",
             rBscreenType = "inhibition", chBrobust = FALSE,
             slCtrlNegMn = 1.5, slCtrlNegSd = 0.1, slCtrlNegN = 8,
             slCtrlPosMn = 0.2, slCtrlPosSd = 0.1, slCtrlPosN = 8,
             slHitsMn = 0.5, slHitsSd = 0.1, slHitsN = 0, slHitsThr = -3,
             nISeed = 1, abResample = 0)
setIn <- function(session, ...) do.call(session$setInputs, modifyList(base, list(...)))

testServer(APP_DIR, {
  cat("\n### A. Same seed + same params => identical plate\n")
  setIn(session, nISeed = 7); a <- copy(dataMain())
  setIn(session, nISeed = 3); invisible(dataMain())      # perturb
  setIn(session, nISeed = 7); b <- copy(dataMain())
  cat("identical:", identical(a, b), "\n"); stopifnot(identical(a, b))

  cat("\n### B. Different seed => different plate\n")
  setIn(session, nISeed = 7); a <- copy(dataMain())
  setIn(session, nISeed = 8); b <- copy(dataMain())
  cat("identical:", identical(a, b), "\n"); stopifnot(!identical(a, b))

  cat("\n### C. Resample click => new plate, same params\n")
  setIn(session, nISeed = 7, abResample = 0); a <- copy(dataMain())
  setIn(session, nISeed = 7, abResample = 1); b <- copy(dataMain())
  setIn(session, nISeed = 7, abResample = 2); c3 <- copy(dataMain())
  cat("click0 vs click1 differ:", !identical(a,b), "| click1 vs click2 differ:", !identical(b,c3), "\n")
  stopifnot(!identical(a,b), !identical(b,c3))
  cat("returning to click0 reproduces it:",
      { setIn(session, nISeed = 7, abResample = 0); identical(a, copy(dataMain())) }, "\n")

  cat("\n### D. Stream collision check: clicking Resample must not\n")
  cat("   reuse a neighbouring draw's noise\n")
  z <- function() { d <- copy(dataMain())
                    list(neg = d[type=="Ctrl Neg", meas], pos = d[type=="Ctrl Pos", meas]) }
  setIn(session, nISeed = 7, abResample = 0); s0 <- z()
  setIn(session, nISeed = 7, abResample = 1); s1 <- z()
  # standardised noise, to catch a shared underlying stream despite different mean/sd
  std <- function(v, m, s) (v - m)/s
  cat("neg(click1) vs pos(click0) standardised overlap:",
      identical(std(s1$neg,1.5,0.1), std(s0$pos,0.2,0.1)), "(want FALSE)\n")
  stopifnot(!identical(std(s1$neg,1.5,0.1), std(s0$pos,0.2,0.1)))

  cat("\n### E. Changing a parameter keeps the SAME noise realisation\n")
  setIn(session, nISeed = 7, slCtrlNegMn = 1.5); a <- copy(dataMain())
  setIn(session, nISeed = 7, slCtrlNegMn = 2.5); b <- copy(dataMain())
  an <- a[type=="Ctrl Neg", meas]; bn <- b[type=="Ctrl Neg", meas]
  cat("neg ctrl shifted by exactly +1.0:", isTRUE(all.equal(bn - an, rep(1.0, length(an)), tolerance=1e-3)), "\n")
  stopifnot(isTRUE(all.equal(bn - an, rep(1.0, length(an)), tolerance = 1e-3)))
  cat("=> the plate pattern is preserved; only the parameter's effect shows\n")

  cat("\n### F. Empty seed box (NA while typing) must not break\n")
  r <- tryCatch({ setIn(session, nISeed = NA); nrow(dataMain()) }, error=function(e) paste("ERROR:", conditionMessage(e)))
  cat("rows with NA seed:", r, "\n"); stopifnot(is.numeric(r), r == 384)

  cat("\n### G. Outputs still render with the new inputs\n")
  setIn(session, nISeed = 7, slHitsN = 12)
  for (o in c("vBzprime","vBssmd","plotDist","plotBox",
              "plotlyHMraw","plotlyHMzscore","plotlyHMnpi","plotlyHMhits")) {
    r <- tryCatch({ nchar(paste(unlist(output[[o]]), collapse="")) > 50 },
                  error=function(e) paste("ERROR:", conditionMessage(e)))
    cat(sprintf("  %-16s -> %s\n", o, r)); stopifnot(isTRUE(r))
  }
})
cat("\nALL SEED CHECKS PASSED\n")
