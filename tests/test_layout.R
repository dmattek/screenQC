source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")

cat("### 1. row labels\n")
for (n in c(8,16,32)) cat(sprintf("  %2d rows: %s ... %s (unique=%d)\n", n,
  paste(head(plateRowLabels(n),3),collapse=","), paste(tail(plateRowLabels(n),3),collapse=","),
  length(unique(plateRowLabels(n)))))
stopifnot(identical(tail(plateRowLabels(32),3), c("AD","AE","AF")),
          length(unique(plateRowLabels(32)))==32)

mk <- function(nRow,nCol,nNeg,nPos) {
  n <- nRow*nCol
  data.table(type=c(rep("Ctrl Neg",nNeg), rep("Ctrl Pos",nPos), rep("Sample", n-nNeg-nPos)),
             meas=0, spiked=FALSE)
}

cat("\n### 2. every layout uses each position exactly once\n")
for (lay in c("first","edge","spread")) for (d in list(c(8,12),c(16,24),c(32,48))) {
  p <- assignPlatePositions(mk(d[1],d[2],8,8), d[1], d[2], lay)
  pos <- (p$colIdx-1L)*d[1] + p$rowIdx
  ok <- length(unique(pos))==d[1]*d[2] && setequal(pos, seq_len(d[1]*d[2]))
  cat(sprintf("  %-6s %2dx%-2d -> %s\n", lay, d[1], d[2], ok)); stopifnot(ok)
}

cat("\n### 3. layouts actually put controls where they claim\n")
for (lay in c("first","edge","spread")) {
  p <- assignPlatePositions(mk(16,24,8,8), 16, 24, lay)
  cc <- sort(unique(p[type!="Sample", colIdx]))
  cat(sprintf("  %-6s control columns: %s\n", lay, paste(cc, collapse=",")))
}
p <- assignPlatePositions(mk(16,24,8,8), 16, 24, "first")
stopifnot(all(p[type!="Sample", colIdx] == 1))
p <- assignPlatePositions(mk(16,24,16,16), 16, 24, "edge")
stopifnot(setequal(p[type!="Sample", colIdx], c(1,24)))
p <- assignPlatePositions(mk(16,24,8,8), 16, 24, "spread")
stopifnot(length(unique(p[type!="Sample", colIdx])) > 4)
cat("  PASS\n")

cat("\n### 4. edge layout falls back inwards when controls exceed the border\n")
# 96-well, both controls maxed: 64 controls but the 2 edge columns hold only 16
p <- assignPlatePositions(mk(8,12,32,32), 8, 12, "edge")
cc <- sort(unique(p[type!="Sample", colIdx]))
cat("  96-well, 64 controls -> columns used:", paste(cc, collapse=","), "\n")
stopifnot(nrow(p[type!="Sample"])==64, all(c(1,12) %in% cc))
cat("  PASS: no crash, spills into the next columns inwards\n")

cat("\n### 5. column-major ordering (what plotPlateHM reshapes on)\n")
p <- assignPlatePositions(mk(8,12,8,8), 8, 12, "first")
stopifnot(identical(p$colIdx, sort(p$colIdx)))
stopifnot(identical(as.character(p$row[1:8]), plateRowLabels(8)))
cat("  first 8 wells are column 1, rows", paste(p$row[1:8], collapse=""), "-> OK\n")

cat("\n### 6. artifacts: gradient spans exactly the slider value\n")
p <- assignPlatePositions(mk(16,24,8,8), 16, 24, "first")
a <- addPlateArtifacts(p, 16, 24, inGradCol=0.5)
d <- a[colIdx==24, mean(meas)] - a[colIdx==1, mean(meas)]
cat(sprintf("  col 1 -> col 24 change: %.4f (want 0.5)\n", d)); stopifnot(abs(d-0.5)<1e-9)
a <- addPlateArtifacts(p, 16, 24, inGradRow=-0.3)
d <- a[rowIdx==16, mean(meas)] - a[rowIdx==1, mean(meas)]
cat(sprintf("  row 1 -> row 16 change: %.4f (want -0.3)\n", d)); stopifnot(abs(d+0.3)<1e-9)

cat("\n### 7. edge effect is strongest on the frame and decays inwards\n")
a <- addPlateArtifacts(p, 16, 24, inEdge=1)
rings <- a[, .(m=mean(meas)), by=.(ring=pmin(rowIdx,16-rowIdx+1,colIdx,24-colIdx+1))][order(ring)]
cat("  mean by ring:", paste(sprintf("%d:%.3f", rings$ring, rings$m), collapse="  "), "\n")
stopifnot(all(diff(rings$m) < 0), abs(rings$m[1]-1) < 1e-9)
cat("  PASS: ring 1 == full magnitude, strictly decaying\n")

cat("\n### 8. zero artifacts leave the data untouched\n")
a <- addPlateArtifacts(p, 16, 24)
stopifnot(identical(a$meas, p$meas)); cat("  PASS\n")

cat("\n### 9. helpers do not mutate the caller\n")
before <- names(p); invisible(addPlateArtifacts(p, 16, 24, inEdge=1))
stopifnot(identical(before, names(p)), all(p$meas==0)); cat("  PASS\n")
