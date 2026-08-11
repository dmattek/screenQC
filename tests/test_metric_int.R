source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")

base <- appInputs(selPlateFormat="384", selSSMDctrl="strong", rBscreenType="inhibition",
             chBrobust=FALSE, slCtrlNegMn=1.5, slCtrlNegSd=0.1, slCtrlNegN=8,
             slCtrlPosMn=0.2, slCtrlPosSd=0.1, slCtrlPosN=8,
             slHitsMn=0.5, slHitsSd=0.1, slHitsN=24, slHitsThr=-3,
             nISeed=1, abResample=0, selCtrlLayout="first",
             slGradRow=0, slGradCol=0, slEdge=0, selHitMetric="zscore")
setIn <- function(session,...) do.call(session$setInputs, modifyList(base, list(...)))

CAP <- new.env(); CAP$msgs <- list()
testServer(APP_DIR, {
  orig <- session$sendInputMessage
  session$sendInputMessage <- function(inputId, message) {
    CAP$msgs[[length(CAP$msgs)+1]] <- list(id=inputId, msg=message); orig(inputId, message)
  }
  last <- function(id) { k <- Filter(function(m) m$id==id, CAP$msgs); if (length(k)) tail(k,1)[[1]]$msg else NULL }

  cat("### A. changing the metric re-ranges the threshold slider\n")
  for (m in c("raw","npi","zscore")) {
    setIn(session, selHitMetric=m)
    msg <- last("slHitsThr"); cfg <- hitMetrics[[m]]
    # shiny sends the numbers as strings and wraps the label in an html shim
    lab <- as.character(msg$label$html)
    num <- function(x) as.numeric(x)
    ok <- !is.null(msg) && num(msg$min) == cfg$min && num(msg$max) == cfg$max &&
          num(msg$value) == cfg$value && lab == cfg$label && num(msg$step) == cfg$step
    cat(sprintf("  %-7s -> '%s' [%s, %s] value %s step %s  ok=%s\n", m,
                lab, msg$min, msg$max, msg$value, msg$step, ok))
    stopifnot(ok)
  }

  cat("\n### B. every metric renders the tab end to end\n")
  for (st in c("inhibition","activation")) for (m in c("zscore","raw","npi")) {
    thr <- switch(m, zscore = if (st=="inhibition") -2 else 2,
                     raw = if (st=="inhibition") 1.0 else 0.7, npi = 50)
    negMn <- if (st=="inhibition") 1.5 else 0.2; posMn <- if (st=="inhibition") 0.2 else 1.5
    hitMn <- if (st=="inhibition") 0.5 else 1.2
    setIn(session, rBscreenType=st, selHitMetric=m, slHitsThr=thr,
          slCtrlNegMn=negMn, slCtrlPosMn=posMn, slHitsMn=hitMn)
    p <- calcHitPerf(dataHits())
    ok <- TRUE
    for (o in c("plotlyHMhits","tabHitsConf","tabHitsPerf")) {
      r <- tryCatch(nchar(paste(unlist(output[[o]]),collapse=""))>50, error=function(e) FALSE)
      if (!isTRUE(r)) ok <- FALSE
    }
    cat(sprintf("  %-11s %-7s thr=%-5s TP=%-3d FP=%-4d TPR=%5.1f%%  renders=%s\n",
                st, m, thr, p$TP, p$FP, 100*ifelse(is.na(p$TPR),0,p$TPR), ok))
    stopifnot(ok, p$TPR > 0.5)
  }

  cat("\n### C. spiked markers still align under every metric\n")
  for (m in c("zscore","raw","npi")) {
    thr <- switch(m, zscore=-2, raw=1.0, npi=50)
    setIn(session, rBscreenType="inhibition", selHitMetric=m, slHitsThr=thr,
          slCtrlNegMn=1.5, slCtrlPosMn=0.2, slHitsMn=0.5)
    p <- calcHitPerf(dataHits())
    w <- fromJSON(as.character(output$plotlyHMhits), simplifyVector=FALSE)
    hm <- w$x$data[[1]]; mk <- w$x$data[[2]]
    xs <- unlist(hm$x); ys <- unlist(hm$y)
    z <- do.call(rbind, lapply(hm$z, function(r) vapply(r, function(e) if (is.null(e)) NA_real_ else as.numeric(e), numeric(1))))
    onT <- sum(vapply(seq_along(mk$x), function(k)
      isTRUE(z[match(as.character(mk$y[[k]]), ys), match(as.character(mk$x[[k]]), xs)]==1), logical(1)))
    # axis order must still be pinned
    xa <- unlist(w$x$layout$xaxis$categoryarray); ya <- unlist(w$x$layout$yaxis$categoryarray)
    cat(sprintf("  %-7s markers-on-T=%-3d TP=%-3d  cols 1..n=%s  A on top=%s\n", m, onT, p$TP,
        identical(xa, as.character(seq_len(length(xs)))), tail(ya,1)=="A"))
    stopifnot(onT == p$TP, identical(xa, as.character(seq_len(length(xs)))), tail(ya,1)=="A")
  }
  cat("\n### D. NPI direction end to end: inhibition hits are ABOVE the cut\n")
  setIn(session, rBscreenType="inhibition", selHitMetric="npi", slHitsThr=50,
        slCtrlNegMn=1.5, slCtrlPosMn=0.2, slHitsMn=0.5)
  d <- dataHits()
  cat(sprintf("  called wells NPI range: [%.1f, %.1f]; uncalled: [%.1f, %.1f]\n",
      min(d[hits %in% TRUE, NPI]), max(d[hits %in% TRUE, NPI]),
      min(d[hits %in% FALSE, NPI]), max(d[hits %in% FALSE, NPI])))
  stopifnot(min(d[hits %in% TRUE, NPI]) > 50, max(d[hits %in% FALSE, NPI]) <= 50)
  cat("  PASS\n")
})
cat("\nMETRIC INTEGRATION PASSED\n")
