source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")

# ORIGINAL inline implementation, verbatim from git HEAD
orig <- function(locDT, valcol, label) {
  locRow = rev(unique(locDT[["row"]])); locCol = unique(locDT[["col"]])
  locM = matrix(as.numeric(locDT[[valcol]]), nrow=length(locRow), ncol=length(locCol))
  locM = locM[seq(from=length(locRow), to=1, by=-1),]
  locText = matrix(locDT[["type"]], nrow=length(locRow), ncol=length(locCol))
  locText = locText[seq(from=length(locRow), to=1, by=-1),]
  plot_ly(x=locCol, y=locRow, z=locM, text=locText, type="heatmap",
          hovertemplate=paste(paste0(label,": %{z}<br>"), "Col: %{x}<br>", "Row: %{y}<br>",
                              "Type: %{text}", "<extra></extra>"),
          colors=rev(RColorBrewer::brewer.pal(n=11, name="RdYlBu")))
}

set.seed(42)
dt <- rbind(data.table(type="Ctrl Neg", meas=rnorm(8,1.5,0.1)),
            data.table(type="Ctrl Pos", meas=rnorm(8,0.2,0.1)),
            data.table(type="Sample",   meas=rnorm(368,1.5,0.1)))
dt[, meas := signif(meas,4)]
g <- expand.grid(LETTERS[1:16], seq_len(24))
dt <- cbind(dt, g); setnames(dt, c("Var1","Var2"), c("row","col")); dt[, col := as.factor(col)]
dt <- calcZscore(dt); dt <- calcNPI(dt, inScreenType="inhibition")

d <- function(p) { b <- plotly_build(p)$x$data[[1]]; b[c("x","y","z","text")] }

for (v in list(c("meas","Meas"), c("zScore","z-score"), c("NPI","NPI"))) {
  a <- d(orig(dt, v[1], v[2]))
  b <- d(plotPlateHM(dt, inColVal=v[1], inHoverLabel=v[2]))
  cat(sprintf("%-8s x:%-5s y:%-5s z:%-5s text:%-5s\n", v[1],
              identical(a$x,b$x), identical(a$y,b$y), identical(a$z,b$z), identical(a$text,b$text)))
  stopifnot(identical(a$x,b$x), identical(a$y,b$y), identical(a$z,b$z), identical(a$text,b$text))
}
cat("\nhovertemplate old:", sQuote(plotly_build(orig(dt,"meas","Meas"))$x$data[[1]]$hovertemplate), "\n")
cat("hovertemplate new:", sQuote(plotly_build(plotPlateHM(dt,inColVal="meas",inHoverLabel="Meas"))$x$data[[1]]$hovertemplate), "\n")
cat("\nEQUIVALENT (x/y/z/text identical)\n")
