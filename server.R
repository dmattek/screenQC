#
# screenQC: a Shiny app for exploring screen quality in HCS
# Author: Maciej Dobrzynski
#
# This is the server logic for a Shiny web application.
#

server <- function(input, output) {
  
  dataCtrlNeg = reactive({
    cat("server:dataCtrlNeg\n")
    
    return(
      data.table(type = rep("Ctrl Neg", input$slCtrlNegN),
                 meas = rnorm(n = input$slCtrlNegN,
                              mean = input$slCtrlNegMn,
                              sd = input$slCtrlNegSd))
    )
  })
  
  dataCtrlPos = reactive({
    cat("server:dataCtrlPos\n")
    
    return(
      data.table(type = rep("Ctrl Pos", input$slCtrlPosN),
                 meas = rnorm(n = input$slCtrlPosN,
                              mean = input$slCtrlPosMn,
                              sd = input$slCtrlPosSd))
    )
  })
  
  dataSamples = reactive({
    cat("server:dataSamples\n")
    
    locNwells = as.integer(input$selPlateFormat) - 
      as.integer(input$slCtrlPosN) - 
      as.integer(input$slCtrlNegN)
    
    # Random dt with samples
    locDT = data.table(type = rep("Sample", locNwells),
                       meas = rnorm(n = locNwells,
                                    mean = input$slSamplesMn,
                                    sd = input$slSamplesSd))
    
    # Add "hits"
    if (input$slHitsN > 0 ) {
      locHits = sample(nrow(locDT), input$slHitsN)
      locDT[locHits,
            `:=`(meas = rnorm(n = length(locHits),
                              mean = input$slCtrlPosMn,
                              sd = input$slCtrlPosSd))]
    }
    
    return(locDT)
    
  })
  
  dataMain = reactive({
    cat("server:dataMain\n")
    
    locDT = rbind(
      dataCtrlNeg(),
      dataCtrlPos(),
      dataSamples())
    
    locDT[,
          type := factor(type, 
                         levels = c("Ctrl Neg",
                                    "Ctrl Pos",
                                    "Sample"))]
    locDT[,
          meas := signif(meas, 4)]    
    
    return(locDT)
  })
  
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
      value = sprintf("%.2f", calcZfactor(dataMain(),
                                          inRobust = input$chBrobust)), 
      subtitle = "Z-Factor", 
      color = "light-blue"
    )
  })
  
  output$vBzprime = renderValueBox({
    cat("server:vBzprime\n")
    
    locZp = calcZprime(dataMain(),
                       inRobust = input$chBrobust)
    
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
    
    
    valueBox(
      value = sprintf("%.2f %s", 
                      locZp,
                      locText), 
      subtitle = "Z-prime", 
      color = locColor
    )
  })
  
  output$vBssmd = renderValueBox({
    cat("server:vBssmd\n")
    
    locSsmd = calcSSMD(dataMain(),
                       inRobust = input$chBrobust)
    
    switch(input$selSSMDctrl,
           "moderate" = {locCtrl = c(-2, -1, -0.5)},
           "strong" = {locCtrl = c(-3, -2, -1)},
           "verystrong" = {locCtrl = c(-5, -3, -2)},
           "extremelystrong" = {locCtrl = c(-7, -5, -3)})
    
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
    
    
    valueBox(
      value = sprintf("%.2f %s", 
                      locSsmd,
                      locText), 
      subtitle = "SSMD", 
      color = locColor
    )
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
    
    locDT = dataPlotHM()
    
    locP = ggplot(data = locDT,
                  aes(x = col,
                      y = row,
                      label = type)) +
      geom_tile(aes(fill = meas)) +
      scale_fill_distiller(palette = "RdYlBu") +
      scale_y_discrete(limits = rev(unique(locDT[["row"]]))) +
      xlab("") +
      ylab("") +
      theme_bw()
    
    ggplotly(locP)
  })
  
  output$plotlyHMz = renderPlotly({
    cat("server:plotlyHMz\n")
    
    locDT = dataPlotHM()
    locDT = calcZscore(locDT,
                       inRobust = input$chBrobust)

    locP = ggplot(data = locDT,
                  aes(x = col,
                      y = row,
                      label = type)) +
      geom_tile(aes(fill = zScore)) +
      scale_fill_distiller(palette = "RdYlBu") +
      scale_y_discrete(limits = rev(unique(locDT[["row"]]))) +
      xlab("") +
      ylab("") +
      theme_bw()
    
    ggplotly(locP)
  })
  
  output$plotlyHMn = renderPlotly({
    cat("server:plotlyHMn\n")
    
    locDT = dataPlotHM()
    locDT = calcNPI(locDT,
                    inRobust = input$chBrobust)
    
    locP = ggplot(data = locDT,
                  aes(x = col,
                      y = row,
                      label = type)) +
      geom_tile(aes(fill = NPI)) +
      scale_fill_distiller(palette = "RdYlBu", direction = 1) +
      scale_y_discrete(limits = rev(unique(locDT[["row"]]))) +
      xlab("") +
      ylab("") +
      theme_bw()
    
    ggplotly(locP)
  })
  
  
  
  
}