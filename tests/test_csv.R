source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")

base <- appInputs(selPlateFormat="384", selSSMDctrl="strong", rBscreenType="inhibition",
             chBrobust=FALSE, slCtrlNegMn=1.5, slCtrlNegSd=0.1, slCtrlNegN=8,
             slCtrlPosMn=0.2, slCtrlPosSd=0.1, slCtrlPosN=8,
             slHitsMn=0.5, slHitsSd=0.1, slHitsN=24, slHitsThr=-3,
             nISeed=1, abResample=0, selCtrlLayout="first",
             slGradRow=0, slGradCol=0, slEdge=0, selHitMetric="zscore")
setIn <- function(session,...) do.call(session$setInputs, modifyList(base, list(...)))

testServer(APP_DIR, {
  cat("### A. shape and columns\n")
  setIn(session)
  d <- dataExport()
  cat("  columns:", paste(names(d), collapse=", "), "\n")
  cat("  rows:", nrow(d), "(want 384, one per well)\n")
  stopifnot(nrow(d)==384,
            identical(names(d), c("well","row","col","type","spiked","meas",
                                  "zScore","NPI","hits","hitMetric","hitThr")))
  cat("  unique wells:", length(unique(d$well)), "-> long format, no duplicates\n")
  stopifnot(length(unique(d$well))==384)

  cat("\n### B. row-major ordering and well ids\n")
  cat("  first 6 wells:", paste(head(d$well,6), collapse=" "), "\n")
  cat("  last  3 wells:", paste(tail(d$well,3), collapse=" "), "\n")
  stopifnot(identical(head(d$well,3), c("A1","A2","A3")))

  cat("\n### C. every well type and the ground truth are present\n")
  print(d[, .N, by=type])
  cat("  spiked wells:", sum(d$spiked), "(want 24)\n")
  stopifnot(sum(d$spiked)==24, all(!d[type!="Sample", spiked]))

  cat("\n### D. both normalisations present regardless of selected metric\n")
  for (m in c("zscore","raw","npi")) {
    setIn(session, selHitMetric=m, slHitsThr=switch(m, zscore=-3, raw=1, npi=50))
    d <- dataExport()
    ok <- !any(is.na(d$zScore)) && !any(is.na(d$NPI)) && is.logical(d$hits)
    cat(sprintf("  metric=%-7s zScore ok=%s NPI ok=%s  hitMetric col='%s' thr=%s\n",
                m, !any(is.na(d$zScore)), !any(is.na(d$NPI)), d$hitMetric[1], d$hitThr[1]))
    stopifnot(ok, d$hitMetric[1]==hitMetrics[[m]][["name"]])
  }

  cat("\n### E. exported hits match what the app called\n")
  setIn(session, selHitMetric="zscore", slHitsThr=-2)
  d <- dataExport(); h <- dataHits()
  setkey(h, NULL)
  merged <- merge(d[, .(well, hits)],
                  h[, .(well=paste0(row, col), hits2=hits)], by="well")
  cat("  wells agreeing:", sum(merged$hits==merged$hits2), "/", nrow(merged), "\n")
  stopifnot(all(merged$hits==merged$hits2))

  cat("\n### F. artifacts are reflected in the exported measurements\n")
  setIn(session, slEdge=0, selHitMetric="zscore", slHitsThr=-3); a <- dataExport()
  setIn(session, slEdge=0.5, selHitMetric="zscore", slHitsThr=-3); b <- dataExport()
  cat(sprintf("  mean meas edge=0.0: %.4f   edge=0.5: %.4f\n", mean(a$meas), mean(b$meas)))
  stopifnot(mean(b$meas) > mean(a$meas))

  cat("\n### G. the real download handler: file name records seed and resample\n")
  for (cfg in list(list(1,0,"384"), list(7,3,"96"), list(NA,0,"1536"))) {
    setIn(session, nISeed=cfg[[1]], abResample=cfg[[2]], selPlateFormat=cfg[[3]])
    got <- basename(output$dbData)
    want <- sprintf("screenQC_%swell_seed%d_resample%d.csv", cfg[[3]],
                    if (is.na(cfg[[1]])) 1L else as.integer(cfg[[1]]), as.integer(cfg[[2]]))
    cat(sprintf("  seed=%-4s resample=%s -> %-42s %s\n", cfg[[1]], cfg[[2]], got,
                if (identical(got, want)) "OK" else paste("WANT", want)))
    stopifnot(identical(got, want))
  }
  cat("  (NA seed falls back to 1, same guard as seedBase)\n")

  cat("\n### H. the handler writes a CSV that round-trips\n")
  setIn(session, nISeed=1, abResample=0, selPlateFormat="384",
        selHitMetric="npi", slHitsThr=50, slHitsN=12)
  f <- output$dbData
  back <- fread(f)
  cat("  file size:", file.size(f), "bytes\n")
  cat("  read back:", nrow(back), "rows x", ncol(back), "cols\n")
  cat("  header:", paste(names(back), collapse=","), "\n")
  print(head(back, 3))
  stopifnot(nrow(back)==384, ncol(back)==11,
            is.logical(back$hits), is.logical(back$spiked),
            is.numeric(back$meas), is.numeric(back$zScore), is.numeric(back$NPI))
  cat("  types intact after fread; spiked/hits logical, measures numeric\n")

  cat("\n### I. 96 and 1536 plates export the right number of wells\n")
  for (fmt in c("96","384","1536")) {
    setIn(session, selPlateFormat=fmt, slHitsN=12)
    n <- nrow(fread(output$dbData))
    cat(sprintf("  %-5s -> %4d rows %s\n", fmt, n, if (n==as.integer(fmt)) "OK" else "MISMATCH"))
    stopifnot(n == as.integer(fmt))
  }
})
