source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")

mk <- function(negMn,posMn,hitMn,nHit=24,seed=1){
  set.seed(seed); nS <- 368
  s <- rnorm(nS,negMn,0.1); sp <- rep(FALSE,nS)
  i <- sample(nS,nHit); s[i] <- rnorm(nHit,hitMn,0.1); sp[i] <- TRUE
  data.table(type=c(rep("Ctrl Neg",8),rep("Ctrl Pos",8),rep("Sample",nS)),
             meas=c(rnorm(8,negMn,0.1),rnorm(8,posMn,0.1),s),
             spiked=c(rep(FALSE,16),sp))
}

cat("### 1. default metric is unchanged behaviour (zscore)\n")
dt <- mk(1.5,0.2,0.5)
a <- calcHits(dt, inScreenType="inhibition", inThr=-3)
b <- calcHits(dt, inScreenType="inhibition", inThr=-3, inMetric="zscore")
stopifnot(identical(a$hits, b$hits)); cat("  PASS\n")

cat("\n### 2. each metric picks the SAME wells at equivalent thresholds\n")
cat("   (inhibition: raw 1.0 <-> NPI 38.5 <-> the matching z-score)\n")
for (st in c("inhibition","activation")) {
  d <- if (st=="inhibition") mk(1.5,0.2,0.5) else mk(0.2,1.5,1.2)
  raw <- calcHits(d, inScreenType=st, inMetric="raw",
                  inThr=if (st=="inhibition") 1.0 else 0.7)
  # NPI equivalent of that raw cut
  n <- calcNPI(d, inScreenType=st)
  cut <- if (st=="inhibition") 1.0 else 0.7
  npiCut <- n[which.min(abs(meas-cut)), NPI]
  npi <- calcHits(d, inScreenType=st, inMetric="npi", inThr=npiCut)
  # z-score equivalent
  z <- calcZscore(d); zCut <- z[which.min(abs(meas-cut)), zScore]
  zs <- calcHits(d, inScreenType=st, inMetric="zscore", inThr=zCut)
  agree_n <- sum(raw$hits == npi$hits); agree_z <- sum(raw$hits == zs$hits)
  cat(sprintf("  %-11s raw calls=%-4d | NPI agrees on %d/%d | zscore agrees on %d/%d\n",
      st, sum(raw$hits), agree_n, nrow(d), agree_z, nrow(d)))
  stopifnot(agree_n >= nrow(d)-1, agree_z >= nrow(d)-1)
}
cat("  PASS: the three metrics are monotone re-parameterisations of each other\n")

cat("\n### 3. NPI comparison is flipped (the trap)\n")
d <- mk(1.5,0.2,0.5)
n <- calcNPI(d, inScreenType="inhibition")
h <- calcHits(d, inScreenType="inhibition", inMetric="npi", inThr=50)
cat(sprintf("  inhibition, NPI>50: called=%d, and every called well has NPI>50: %s\n",
    sum(h$hits), all(n$NPI[h$hits] > 50)))
stopifnot(all(n$NPI[h$hits] > 50), all(n$NPI[!h$hits] <= 50))
d2 <- mk(0.2,1.5,1.2)
n2 <- calcNPI(d2, inScreenType="activation")
h2 <- calcHits(d2, inScreenType="activation", inMetric="npi", inThr=50)
cat(sprintf("  activation, NPI<50: called=%d, and every called well has NPI<50: %s\n",
    sum(h2$hits), all(n2$NPI[h2$hits] < 50)))
stopifnot(all(n2$NPI[h2$hits] < 50))
cat("  PASS: NPI selects ABOVE for inhibition and BELOW for activation\n")

cat("\n### 4. each metric actually recovers the spiked wells\n")
for (st in c("inhibition","activation")) {
  d <- if (st=="inhibition") mk(1.5,0.2,0.5) else mk(0.2,1.5,1.2)
  for (m in c("raw","zscore","npi")) {
    thr <- switch(m, raw = if (st=="inhibition") 1.0 else 0.7,
                     zscore = if (st=="inhibition") -2 else 2,
                     npi = 50)
    p <- calcHitPerf(calcHits(d, inScreenType=st, inMetric=m, inThr=thr))
    cat(sprintf("  %-11s %-7s TPR=%5.1f%% FPR=%5.2f%% PPV=%6s\n", st, m,
        100*p$TPR, 100*p$FPR, ifelse(is.na(p$PPV),"n/a",sprintf("%.1f%%",100*p$PPV))))
    stopifnot(p$TPR > 0.5)
  }
}
cat("  PASS: every metric/screen-type combination recovers the majority of spikes\n")

cat("\n### 5. caller table still not mutated\n")
before <- names(dt); invisible(calcHits(dt, inMetric="npi", inThr=50)); invisible(calcHits(dt, inMetric="raw", inThr=1))
stopifnot(identical(before, names(dt))); cat("  PASS\n")

cat("\n### 6. hitMetrics config is complete\n")
for (m in names(hitMetrics)) {
  cfg <- hitMetrics[[m]]
  stopifnot(all(c("name","col","label","min","max","value","step") %in% names(cfg)),
            cfg$value >= cfg$min, cfg$value <= cfg$max)
  cat(sprintf("  %-7s %-26s [%g, %g] default %g step %g\n", m, cfg$label, cfg$min, cfg$max, cfg$value, cfg$step))
}
cat("  PASS\n")
