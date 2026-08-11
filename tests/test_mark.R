source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")


mkPlate <- function(nRow, nCol, nSpike, seed=1) {
  set.seed(seed); n <- nRow*nCol
  dt <- data.table(type=c(rep("Ctrl Neg",8), rep("Ctrl Pos",8), rep("Sample", n-16)),
                   meas=rnorm(n), spiked=FALSE)
  dt[sample(17:n, nSpike), spiked := TRUE]
  # same row labelling as dataPlotHM() in server.R
  rowLab <- if (nRow <= 26) LETTERS[1:nRow] else t(outer(LETTERS, LETTERS, FUN=paste0))[1:nRow]
  g <- expand.grid(rowLab, seq_len(nCol))
  dt <- cbind(dt, g); setnames(dt, c("Var1","Var2"), c("row","col"))
  dt[, col := as.factor(col)]
  dt[, hits := meas < -0.5]
  dt[]
}

cat("### 1. marker trace appears only when inColMark is given\n")
dt <- mkPlate(16, 24, 20)
p0 <- plotly_build(plotPlateHM(dt, "hits", "Hit"))
p1 <- plotly_build(plotPlateHM(dt, "hits", "Hit", inColMark="spiked", inMarkLabel="Spiked"))
cat("  without inColMark: traces =", length(p0$x$data), "(want 1)\n")
cat("  with    inColMark: traces =", length(p1$x$data), "(want 2)\n")
stopifnot(length(p0$x$data)==1, length(p1$x$data)==2)
cat("  2nd trace type/mode:", p1$x$data[[2]]$type, "/", p1$x$data[[2]]$mode, "\n")
stopifnot(p1$x$data[[2]]$type=="scatter", p1$x$data[[2]]$mode=="markers")

cat("\n### 2. one marker per spiked well, no more no less\n")
cat("  spiked in data:", sum(dt$spiked), " markers:", length(p1$x$data[[2]]$x), "\n")
stopifnot(length(p1$x$data[[2]]$x) == sum(dt$spiked))

cat("\n### 3. ALIGNMENT: each marker sits on the cell holding that well\n")
checkAlign <- function(nRow, nCol, nSpike, seed) {
  dt <- mkPlate(nRow, nCol, nSpike, seed)
  b  <- plotly_build(plotPlateHM(dt, "hits", "Hit", inColMark="spiked"))
  hm <- b$x$data[[1]]; mk <- b$x$data[[2]]
  z  <- if (is.matrix(hm$z)) hm$z else do.call(rbind, hm$z)
  xs <- as.character(hm$x); ys <- as.character(hm$y)
  bad <- 0
  for (k in seq_along(mk$x)) {
    i <- match(as.character(mk$y[k]), ys)      # row index on the plotted axis
    j <- match(as.character(mk$x[k]), xs)      # col index on the plotted axis
    zc <- z[i, j]                              # what the heatmap draws there
    tv <- dt[row==as.character(mk$y[k]) & col==as.character(mk$x[k]), hits]  # truth
    if (is.na(i) || is.na(j) || length(tv)!=1 || zc != as.numeric(tv)) bad <- bad + 1
  }
  bad
}
for (cfg in list(c(8,12,10,1), c(16,24,20,2), c(16,24,36,3), c(32,48,36,4))) {
  bad <- checkAlign(cfg[1], cfg[2], cfg[3], cfg[4])
  cat(sprintf("  %2dx%-2d plate, %2d spiked -> misaligned markers: %d\n", cfg[1],cfg[2],cfg[3],bad))
  stopifnot(bad == 0)
}
cat("  PASS: every marker overlays the cell whose value it belongs to\n")

cat("\n### 4. zero spiked wells => no marker trace, no error\n")
dt0 <- mkPlate(16,24,0); dt0[, spiked := FALSE]
b0 <- plotly_build(plotPlateHM(dt0, "hits", "Hit", inColMark="spiked"))
cat("  traces =", length(b0$x$data), "(want 1)\n"); stopifnot(length(b0$x$data)==1)

cat("\n### 5. NA in the mark column never selects a well\n")
dtNA <- mkPlate(16,24,5); dtNA[1:20, spiked := NA]
bNA <- plotly_build(plotPlateHM(dtNA, "hits", "Hit", inColMark="spiked"))
nMark <- if (length(bNA$x$data)>1) length(bNA$x$data[[2]]$x) else 0
cat("  spiked==TRUE:", sum(dtNA$spiked %in% TRUE), " markers:", nMark, "\n")
stopifnot(nMark == sum(dtNA$spiked %in% TRUE))

cat("\n### 6. heatmap layer unchanged by the overlay\n")
a <- plotly_build(plotPlateHM(dt, "hits", "Hit"))$x$data[[1]]
b <- plotly_build(plotPlateHM(dt, "hits", "Hit", inColMark="spiked"))$x$data[[1]]
cat("  x/y/z/text identical:", identical(a$x,b$x), identical(a$y,b$y),
    identical(a$z,b$z), identical(a$text,b$text), "\n")
stopifnot(identical(a$x,b$x), identical(a$y,b$y), identical(a$z,b$z), identical(a$text,b$text))
cat("\nALL MARKER CHECKS PASSED\n")
