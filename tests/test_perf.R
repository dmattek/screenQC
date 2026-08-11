source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")

")), envir=globalenv())

cat("### calcHitPerf on a hand-built table with known counts\n")
# 10 sample wells: 4 spiked (3 called => TP=3, FN=1), 6 clean (2 called => FP=2, TN=4)
# plus controls that must be ignored entirely
dt <- data.table(
  type   = c(rep("Sample",10), "Ctrl Neg","Ctrl Pos"),
  spiked = c(rep(TRUE,4), rep(FALSE,6), FALSE, FALSE),
  hits   = c(TRUE,TRUE,TRUE,FALSE, TRUE,TRUE,FALSE,FALSE,FALSE,FALSE, TRUE,TRUE))
p <- calcHitPerf(dt)
cat(sprintf("  TP=%d (want 3)  FP=%d (want 2)  FN=%d (want 1)  TN=%d (want 4)\n", p$TP,p$FP,p$FN,p$TN))
stopifnot(p$TP==3, p$FP==2, p$FN==1, p$TN==4)
cat(sprintf("  TPR=%.3f (want %.3f)  FPR=%.3f (want %.3f)  PPV=%.3f (want %.3f)\n",
            p$TPR, 3/4, p$FPR, 2/6, p$PPV, 3/5))
stopifnot(all.equal(p$TPR,3/4), all.equal(p$FPR,2/6), all.equal(p$PPV,3/5))
cat("  controls excluded: TP+FP+FN+TN =", p$TP+p$FP+p$FN+p$TN, "(want 10, not 12)\n")
stopifnot(p$TP+p$FP+p$FN+p$TN == 10)
cat("  PASS\n")

cat("\n### no spiked wells => TPR/PPV undefined, FPR still defined\n")
dt2 <- data.table(type=rep("Sample",6), spiked=rep(FALSE,6),
                  hits=c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE))
p2 <- calcHitPerf(dt2)
cat(sprintf("  TPR=%s PPV=%s FPR=%.3f\n", p2$TPR, p2$PPV, p2$FPR))
stopifnot(is.na(p2$TPR), !is.na(p2$FPR), all.equal(p2$FPR, 1/6))
cat("  PASS (TPR n/a, PPV defined only if something was called)\n")

cat("\n### nothing called => PPV undefined\n")
dt3 <- data.table(type=rep("Sample",6), spiked=c(TRUE,rep(FALSE,5)), hits=rep(FALSE,6))
p3 <- calcHitPerf(dt3)
cat(sprintf("  PPV=%s TPR=%.3f\n", p3$PPV, p3$TPR))
stopifnot(is.na(p3$PPV), all.equal(p3$TPR, 0))
cat("  PASS\n")
