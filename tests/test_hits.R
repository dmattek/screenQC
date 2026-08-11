source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")

base <- appInputs(selCtrlLayout="first", slGradRow=0, slGradCol=0, slEdge=0,
             selPlateFormat="384", selSSMDctrl="strong", rBscreenType="inhibition",
             chBrobust=FALSE, slCtrlNegMn=1.5, slCtrlNegSd=0.1, slCtrlNegN=8,
             slCtrlPosMn=0.2, slCtrlPosSd=0.1, slCtrlPosN=8,
             slHitsMn=0.5, slHitsSd=0.1, slHitsN=0, slHitsThr=-3,
             nISeed=1, abResample=0)
setIn <- function(session, ...) do.call(session$setInputs, modifyList(base, list(...)))

testServer(APP_DIR, {
  cat("\n### A. ground truth is recorded and matches the #hits slider\n")
  for (n in c(0, 1, 12, 36)) {
    setIn(session, slHitsN = n)
    got <- sum(dataMain()$spiked)
    cat(sprintf("  slHitsN=%-3d spiked wells=%-3d %s\n", n, got, if (got==n) "OK" else "MISMATCH"))
    stopifnot(got == n)
  }
  cat("  controls never spiked:", all(!dataMain()[type != "Sample", spiked]), "\n")
  stopifnot(all(!dataMain()[type != "Sample", spiked]))

  cat("\n### B. clamp: 96-well, both controls at max(32), 36 hits requested\n")
  r <- tryCatch({
    setIn(session, selPlateFormat="96", slCtrlNegN=32, slCtrlPosN=32, slHitsN=36)
    c(rows=nrow(dataMain()), spiked=sum(dataMain()$spiked))
  }, error=function(e) paste("ERROR:", conditionMessage(e)))
  cat("  ->", if (is.character(r)) r else sprintf("rows=%d spiked=%d (clamped to the 32 sample wells)", r[1], r[2]), "\n")
  stopifnot(!is.character(r), r[["spiked"]] == 32)
  cat("  PASS: no crash\n")

  cat("\n### C. confusion counts cover exactly the sample wells\n")
  setIn(session, slHitsN = 24)
  p <- calcHitPerf(dataHits())
  nSample <- nrow(dataMain()[type == "Sample"])
  cat(sprintf("  TP+FP+FN+TN=%d  sample wells=%d\n", p$TP+p$FP+p$FN+p$TN, nSample))
  stopifnot(p$TP+p$FP+p$FN+p$TN == nSample)
  cat("  TP+FN =", p$TP+p$FN, "== spiked =", sum(dataMain()[type=="Sample", spiked]), "\n")
  stopifnot(p$TP+p$FN == sum(dataMain()[type=="Sample", spiked]))

  cat("\n### D. threshold sweep (inhibition: raising thr calls more wells)\n")
  cat("   thr     TP   FP   FN   TN    TPR     FPR     PPV\n")
  tprs <- c()
  for (thr in c(-6,-5,-4,-3,-2,-1,0)) {
    setIn(session, slHitsN=24, slHitsThr=thr)
    p <- calcHitPerf(dataHits()); tprs <- c(tprs, ifelse(is.na(p$TPR), 0, p$TPR))
    cat(sprintf("  %4d  %4d %4d %4d %4d  %6s  %6s  %6s\n", thr, p$TP,p$FP,p$FN,p$TN,
        ifelse(is.na(p$TPR),"n/a",sprintf("%.3f",p$TPR)),
        ifelse(is.na(p$FPR),"n/a",sprintf("%.3f",p$FPR)),
        ifelse(is.na(p$PPV),"n/a",sprintf("%.3f",p$PPV))))
  }
  cat("  TPR monotonically non-decreasing:", all(diff(tprs) >= 0), "\n")
  stopifnot(all(diff(tprs) >= 0))
  cat("  PASS: sensitivity/false-positive trade-off behaves correctly\n")

  cat("\n### E. no spiked wells => TPR shown as n/a, not a crash\n")
  setIn(session, slHitsN = 0, slHitsThr = -3)
  h <- paste(unlist(output$tabHitsPerf), collapse=" ")
  cat("  perf table contains 'n/a':", grepl("n/a", h), "\n"); stopifnot(grepl("n/a", h))

  cat("\n### F. both tables render across formats / screen types\n")
  for (fmt in c("96","384","1536")) for (st in c("inhibition","activation")) {
    setIn(session, selPlateFormat=fmt, rBscreenType=st, slHitsN=12,
          slHitsThr=if (st=="inhibition") -3 else 3)
    ok <- tryCatch(nchar(paste(unlist(output$tabHitsConf),collapse=""))>50 &&
                   nchar(paste(unlist(output$tabHitsPerf),collapse=""))>50,
                   error=function(e) paste("ERROR:", conditionMessage(e)))
    cat(sprintf("  %-5s %-11s -> %s\n", fmt, st, ok)); stopifnot(isTRUE(ok))
  }
})
cat("\nALL HIT-SCORING CHECKS PASSED\n")
