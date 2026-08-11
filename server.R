#
# screenQC: a Shiny app for exploring screen quality in HCS
# Author: Maciej Dobrzynski
#
# This is the server logic for a Shiny web application.
#

server <- function(input, output, session) {

  # Base of the random seed used by the three draws below.
  #
  # Clicking Resample advances input$abResample by one, which yields a fresh
  # plate from unchanged parameters. Holding it fixed makes the app
  # deterministic: dragging a slider then re-uses the same underlying noise,
  # so the change in the quality indices is the effect of that slider alone.
  #
  # Scaled by 10 so that the per-draw offsets added below never collide with
  # the base of a neighbouring click.
  seedBase = reactive({
    cat("server:seedBase\n")

    # The numeric input is empty (NA) while being edited
    locSeed = suppressWarnings(as.integer(input$nISeed))
    if (length(locSeed) == 0L || is.na(locSeed))
      locSeed = 1L

    return(10L * (locSeed + as.integer(input$abResample)))
  })

  dataCtrlNeg = reactive({
    cat("server:dataCtrlNeg\n")

    set.seed(seedBase() + 1L)

    return(
      data.table(type = rep("Ctrl Neg", input$slCtrlNegN),
                 meas = rnorm(n = input$slCtrlNegN,
                              mean = input$slCtrlNegMn,
                              sd = input$slCtrlNegSd),
                 spiked = FALSE)
    )
  })

  dataCtrlPos = reactive({
    cat("server:dataCtrlPos\n")

    set.seed(seedBase() + 2L)

    return(
      data.table(type = rep("Ctrl Pos", input$slCtrlPosN),
                 meas = rnorm(n = input$slCtrlPosN,
                              mean = input$slCtrlPosMn,
                              sd = input$slCtrlPosSd),
                 spiked = FALSE)
    )
  })

  dataSamples = reactive({
    cat("server:dataSamples\n")

    set.seed(seedBase() + 3L)

    locNwells = as.integer(input$selPlateFormat) -
      as.integer(input$slCtrlPosN) -
      as.integer(input$slCtrlNegN)

    # Random dt with samples
    # The same mean & sd as negative control
    # spiked records the ground truth used to score the called hits
    locDT = data.table(type = rep("Sample", locNwells),
                       meas = rnorm(n = locNwells,
                                    mean = input$slCtrlNegMn,
                                    sd = input$slCtrlNegSd),
                       spiked = FALSE)

    # Add "hits"
    # A 96-well plate with both control counts at their maximum leaves fewer
    # sample wells than the #hits slider allows, hence the clamp
    locNhits = min(input$slHitsN, nrow(locDT))

    if (locNhits > 0 ) {
      locHits = sample(nrow(locDT), locNhits)
      locDT[locHits,
            `:=`(meas = rnorm(n = length(locHits),
                              mean = input$slHitsMn,
                              sd = input$slHitsSd),
                 spiked = TRUE)]
    }

    return(locDT)

  })
  
  # The three draws stacked, before the wells are placed on the plate
  dataMain = reactive({
    cat("server:dataMain\n")

    return(
      rbind(
        dataCtrlNeg(),
        dataCtrlPos(),
        dataSamples())
    )
  })

  # The plate every consumer works from: wells positioned according to the
  # chosen control layout, then the spatial artifacts laid on top.
  #
  # Positioning has to happen here rather than further downstream, because
  # the artifacts are position-dependent and have to reach the quality
  # indices and the distribution plots, not just the heatmaps.
  dataPlate = reactive({
    cat("server:dataPlate\n")

    locDims = plateDims(input$selPlateFormat)

    locDT = assignPlatePositions(dataMain(),
                                 inNrow = locDims[["nRow"]],
                                 inNcol = locDims[["nCol"]],
                                 inLayout = input$selCtrlLayout)

    locDT = addPlateArtifacts(locDT,
                              inNrow = locDims[["nRow"]],
                              inNcol = locDims[["nCol"]],
                              inGradRow = input$slGradRow,
                              inGradCol = input$slGradCol,
                              inEdge = input$slEdge)

    locDT[,
          meas := signif(meas, 4)]

    return(locDT)
  })
  
  # Colour limits for the raw heatmap, derived from the distribution
  # parameters rather than from the drawn values, so that the scale holds
  # still while resampling or dragging the hit threshold
  rawZlim = reactive({
    cat("server:rawZlim\n")

    locMn = c(input$slCtrlNegMn, input$slCtrlPosMn)
    locSd = c(input$slCtrlNegSd, input$slCtrlPosSd)

    if (input$slHitsN > 0) {
      locMn = c(locMn, input$slHitsMn)
      locSd = c(locSd, input$slHitsSd)
    }

    # Artifacts only ever shift wells in the direction of their sign
    locArt = c(min(0, input$slGradRow) + min(0, input$slGradCol) + min(0, input$slEdge),
               max(0, input$slGradRow) + max(0, input$slGradCol) + max(0, input$slEdge))

    return(list(zmin = min(locMn) - 3 * max(locSd) + locArt[1],
                zmax = max(locMn) + 3 * max(locSd) + locArt[2]))
  })

  output$vBzfactor = renderValueBox({
    cat("server:vBzfactor\n")
    
    valueBox(
      value = sprintf("%.2f", 
                      calcZfactor(dataPlate(),
                                  inRobust = input$chBrobust,
                                  inScreenType = input$rBscreenType)), 
      subtitle = "Z-Factor", 
      color = "light-blue"
    )
  })
  
  output$vBzprime = renderValueBox({
    cat("server:vBzprime\n")
    
    locZp = calcZprime(dataPlate(),
                       inRobust = input$chBrobust)

    # NaN when both controls have zero spread and identical means;
    # the index is undefined rather than good or bad
    if(is.nan(locZp)) {
      locValue = "n/a"
      locColor = "black"
    } else {
      if(locZp > 0.5) {
        locColor = "olive"
        locText = "excellent"
      } else if (locZp > 0) {
        locColor = "orange"
        locText = "marginal"
      }
      else {
        locColor = "red"
        locText = "overlap"
      }

      locValue = sprintf("%.2f %s",
                         locZp,
                         locText)
    }

    return(clickableValueBox(locValue,
                             "Z-prime",
                             locColor,
                             "vBzprime"))
  })
  
  output$vBssmd = renderValueBox({
    cat("server:vBssmd\n")
    
    locSsmd = calcSSMD(dataPlate(),
                       inRobust = input$chBrobust,
                       inScreenType = input$rBscreenType)
    
    switch(input$selSSMDctrl,
           "moderate" = {locCtrl = c(-2, -1, -0.5)},
           "strong" = {locCtrl = c(-3, -2, -1)},
           "verystrong" = {locCtrl = c(-5, -3, -2)},
           "extremelystrong" = {locCtrl = c(-7, -5, -3)})

    # NaN when both controls have zero spread and identical means;
    # the index is undefined rather than good or bad
    if(is.nan(locSsmd)) {
      locValue = "n/a"
      locColor = "black"
    } else {
      if(locSsmd <= locCtrl[1]) {
        locColor = "olive"
        locText = "excellent"
      } else if (locSsmd <= locCtrl[2]) {
        locColor = "teal"
        locText = "good"
      }
      else if (locSsmd <= locCtrl[3]) {
        locColor = "orange"
        locText = "inferior"
      } else {
        locColor = "red"
        locText = "poor"
      }

      locValue = sprintf("%.2f %s",
                         locSsmd,
                         locText)
    }

    return(clickableValueBox(locValue,
                             "SSMD",
                             locColor,
                             "vBssmd"))
  })
  
  
  output$plotDist = renderPlot({
    cat("server:plotDist\n")
    
    ggplot(data = dataPlate(),
           aes(x = meas,
               y = after_stat(density))) +
      geom_density(aes(fill = type), 
                   alpha = 0.5) +
      scale_fill_tableau(palette = "Tableau 10", 
                         name = "") +
      xlab("Measurement") +
      ylab("Density") +
      theme_bw()
  })
  
  output$plotBox = renderPlot({
    cat("server:plotBox\n")
    
    ggplot(data = dataPlate(),
           aes(x = type,
               y = meas)) +
      geom_boxplot(aes(fill = type), 
                   alpha = 0.5) +
      scale_fill_tableau(palette = "Tableau 10", 
                         name = "") +
      xlab("Well Type") +
      ylab("Measurement") +
      theme_bw()
  })
  
  output$plotlyHMraw = renderPlotly({
    cat("server:plotlyHMraw\n")

    plotPlateHM(dataPlate(),
                inColVal = "meas",
                inHoverLabel = "Meas",
                zmin = rawZlim()[["zmin"]],
                zmax = rawZlim()[["zmax"]])
  })

  output$plotlyHMzscore = renderPlotly({
    cat("server:plotlyHMzscore\n")

    locDT = calcZscore(dataPlate(),
                       inRobust = input$chBrobust)

    # Symmetric about 0 and matching the range of the threshold slider
    plotPlateHM(locDT,
                inColVal = "zScore",
                inHoverLabel = "z-score",
                zmin = -6,
                zmax = 6)
  })

  output$plotlyHMnpi = renderPlotly({
    cat("server:plotlyHMnpi\n")

    locDT = calcNPI(dataPlate(),
                    inRobust = input$chBrobust,
                    inScreenType = input$rBscreenType)

    # 0 is the negative control and 100 the positive control by construction,
    # with margin for wells falling outside that span
    plotPlateHM(locDT,
                inColVal = "NPI",
                inHoverLabel = "NPI",
                zmin = -25,
                zmax = 125)
  })

  # Called hits, shared by the heatmap and the scoring tables below
  dataHits = reactive({
    cat("server:dataHits\n")

    return(calcHits(dataPlate(),
                    inRobust = input$chBrobust,
                    inScreenType = input$rBscreenType,
                    inMetric = input$selHitMetric,
                    inThr = input$slHitsThr))
  })

  # The plate in long format, one row per well ----
  dataExport = reactive({
    cat("server:dataExport\n")

    # dataHits() only carries the column its own metric thresholded on, so
    # add both normalisations to give the same export whatever is selected
    locDT = calcZscore(dataHits(),
                       inRobust = input$chBrobust)

    locDT = calcNPI(locDT,
                    inRobust = input$chBrobust,
                    inScreenType = input$rBscreenType)

    # hits alone cannot be read without knowing what produced it
    locDT[, `:=`(well = paste0(row, col),
                 zScore = signif(zScore, 4),
                 NPI = signif(NPI, 4),
                 hitMetric = hitMetrics[[input$selHitMetric]][["name"]],
                 hitThr = input$slHitsThr)]

    # Row-major, the order a plate is usually read in
    setorder(locDT, rowIdx, colIdx)

    return(locDT[,
                 .(well, row, col, type, spiked, meas,
                   zScore, NPI, hits, hitMetric, hitThr)])
  })

  output$dbData = downloadHandler(
    filename = function() {
      # Same guard as seedBase(): the numeric input is NA while being edited
      locSeed = suppressWarnings(as.integer(input$nISeed))
      if (length(locSeed) == 0L || is.na(locSeed))
        locSeed = 1L

      # Seed and resample count are what make the plate reproducible
      return(
        sprintf("screenQC_%swell_seed%d_resample%d.csv",
                input$selPlateFormat,
                locSeed,
                as.integer(input$abResample))
      )
    },

    content = function(file) {
      fwrite(dataExport(), file)
    }
  )

  output$plotlyHMhits = renderPlotly({
    cat("server:plotlyHMhits\n")

    locDT = dataHits()

    # Two-colour discrete scale for the logical hit column
    # From: https://stackoverflow.com/a/49944091/1898713
    locColorScale <- data.table(z=c(0, 0.5, 0.5, 1),
                                col=rep(ggthemes::tableau_color_pal(palette = "Color Blind")(2),
                                        each = 2))
    locColorScale[,
                  col := as.character(col)]

    # Cells shrink as the plate grows, so the marker has to follow
    locMarkSize = switch(input$selPlateFormat,
                         "96" = 11,
                         "384" = 6,
                         "1536" = 3)

    plotPlateHM(locDT,
                inColVal = "hits",
                inHoverLabel = "Hit",
                inColors = NULL,
                inColMark = "spiked",
                inMarkLabel = "Spiked (true hit)",
                inMarkSize = locMarkSize,
                colorscale = locColorScale,
                # Pinned, so that a plate with nothing called still renders
                # as F rather than filling with one flat extreme
                zmin = 0,
                zmax = 1,
                colorbar = list(tickvals = c(0.25, 0.75),
                                ticktext = c("F", "T")))
  })

  # Called hits scored against the wells that were actually spiked ----

  output$tabHitsConf = renderTable({
    cat("server:tabHitsConf\n")

    locPerf = calcHitPerf(dataHits())

    return(
      data.frame(" " = c("Called hit", "Not called"),
                 "Spiked" = c(locPerf[["TP"]], locPerf[["FN"]]),
                 "Not spiked" = c(locPerf[["FP"]], locPerf[["TN"]]),
                 check.names = FALSE)
    )
  },
  digits = 0,
  align = "lrr",
  width = "100%")

  output$tabHitsPerf = renderTable({
    cat("server:tabHitsPerf\n")

    locPerf = calcHitPerf(dataHits())

    # NA whenever the rate has an empty denominator, e.g. no spiked wells
    locPct = function(inVal) {
      if (is.na(inVal))
        return("n/a")
      else
        return(sprintf("%.1f%%", 100 * inVal))
    }

    return(
      data.frame("Metric" = c("Sensitivity (TPR)",
                              "False positive rate (FPR)",
                              "Precision (PPV)"),
                 "Value" = c(locPct(locPerf[["TPR"]]),
                             locPct(locPerf[["FPR"]]),
                             locPct(locPerf[["PPV"]])),
                 check.names = FALSE)
    )
  },
  align = "lr",
  width = "100%")
  
  
  # Re-scale the hit threshold when the metric changes ----
  # The three quantities live on entirely different scales, so the slider is
  # rebuilt from the hitMetrics entry rather than keeping a stale range.
  observeEvent(input$selHitMetric, {
    locCfg = hitMetrics[[input$selHitMetric]]

    updateSliderInput(session,
                      "slHitsThr",
                      label = locCfg[["label"]],
                      min = locCfg[["min"]],
                      max = locCfg[["max"]],
                      value = locCfg[["value"]],
                      step = locCfg[["step"]])
  })

  # Keep the #hits slider within the wells actually available ----
  # Control wells eat into the sample wells, so on a 96-well plate with both
  # control counts at their maximum only 32 sample wells remain. dataSamples()
  # clamps regardless; this stops the slider offering the impossible values.
  observeEvent(
    {
      input$selPlateFormat
      input$slCtrlNegN
      input$slCtrlPosN
    },
    {
      locMax = as.integer(input$selPlateFormat) -
        as.integer(input$slCtrlNegN) -
        as.integer(input$slCtrlPosN)

      updateSliderInput(session,
                        "slHitsN",
                        max = locMax,
                        value = min(isolate(input$slHitsN), locMax))
    })

  # Pop-overs ----
  # The help pop-overs are carried by the helpLink() labels in ui.R and bound
  # client-side, so that clicking a label opens its help and clicking the
  # input next to it does not. See the note on helpLink() in global.R.

  observeEvent(input$button_vBzprime, {
    toggleModal(session, "modal_vBzprime", "open")
  })
  
  observeEvent(input$button_vBssmd, {
    toggleModal(session, "modal_vBssmd", "open")
  })
}