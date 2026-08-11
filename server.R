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
  
  dataMain = reactive({
    cat("server:dataMain\n")
    
    locDT = rbind(
      dataCtrlNeg(),
      dataCtrlPos(),
      dataSamples())
    # 
    # locDT[,
    #       type := factor(type, 
    #                      levels = c("Ctrl Neg",
    #                                 "Ctrl Pos",
    #                                 "Sample"))]
    locDT[,
          meas := signif(meas, 4)]    
    
    return(locDT)
  })
  
  # Prepare data in long format for plotting with ggplot;
  # add row and column numbers depending on plate format.
  dataPlotHM = reactive({
    cat("server:dataPlotHM\n")
    
    locDT = dataMain()
    
    switch(input$selPlateFormat,
           "96" = {
             locX = 12
             locY = 8
             locGrid = expand.grid(LETTERS[1:locY],
                                   seq_len(locX))},
           "384" = {
             locX = 24
             locY = 16
             locGrid = expand.grid(LETTERS[1:locY],
                                   seq_len(locX))
           },
           "1536" = {
             locX = 48
             locY = 32
             locGrid = expand.grid(t(outer(LETTERS, LETTERS, FUN = paste0))[1:locY],
                                   seq_len(locX))
           })
    
    
    
    locDT = cbind(locDT,
                  locGrid)
    
    setnames(locDT, 
             c("Var1", "Var2"),
             c("row", "col"))
    
    locDT[,
          col := as.factor(col)]
  })
  
  output$vBzfactor = renderValueBox({
    cat("server:vBzfactor\n")
    
    valueBox(
      value = sprintf("%.2f", 
                      calcZfactor(dataMain(),
                                  inRobust = input$chBrobust,
                                  inScreenType = input$rBscreenType)), 
      subtitle = "Z-Factor", 
      color = "light-blue"
    )
  })
  
  output$vBzprime = renderValueBox({
    cat("server:vBzprime\n")
    
    locZp = calcZprime(dataMain(),
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
    
    locSsmd = calcSSMD(dataMain(),
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
    
    ggplot(data = dataMain(),
           aes(x = meas,
               y = ..density..)) +
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
    
    ggplot(data = dataMain(),
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

    plotPlateHM(dataPlotHM(),
                inColVal = "meas",
                inHoverLabel = "Meas")
  })

  output$plotlyHMzscore = renderPlotly({
    cat("server:plotlyHMzscore\n")

    locDT = calcZscore(dataPlotHM(),
                       inRobust = input$chBrobust)

    plotPlateHM(locDT,
                inColVal = "zScore",
                inHoverLabel = "z-score")
  })

  output$plotlyHMnpi = renderPlotly({
    cat("server:plotlyHMnpi\n")

    locDT = calcNPI(dataPlotHM(),
                    inRobust = input$chBrobust,
                    inScreenType = input$rBscreenType)

    plotPlateHM(locDT,
                inColVal = "NPI",
                inHoverLabel = "NPI")
  })

  # Called hits, shared by the heatmap and the scoring tables below
  dataHits = reactive({
    cat("server:dataHits\n")

    return(calcHits(dataPlotHM(),
                    inRobust = input$chBrobust,
                    inScreenType = input$rBscreenType,
                    inThr = input$slHitsThr))
  })

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
  
  
  # Pop-overs ----
  addPopover(session,
             "alPlateFormat",
             title = "Plate format",
             content = helpTextServer[["alPlateFormat"]],
             trigger = "click")
  
  addPopover(session,
             "alSSMDcrit",
             title = "SSMD criteria",
             content = helpTextServer[["alSSMDcrit"]],
             trigger = "click")

  addPopover(session,
             "alSeed",
             title = "Random seed",
             content = helpTextServer[["nISeed"]],
             trigger = "click")

  addPopover(session,
             "alHitPerf",
             title = "Scoring called hits",
             content = helpTextServer[["alHitPerf"]],
             trigger = "click")
  
  observeEvent(input$button_vBzprime, {
    toggleModal(session, "modal_vBzprime", "open")
  })
  
  observeEvent(input$button_vBssmd, {
    toggleModal(session, "modal_vBssmd", "open")
  })
}