#
# screenQC: a Shiny app for exploring screen quality in HCS
# Author: Maciej Dobrzynski
#
# This is the auxilary logic for a Shiny web application.
#

# Check if all required packages are installed, if not, attempt to install the missing ones
required_packages = c(
  "shiny",
  "shinydashboard",
  "shinyBS",
  "data.table",
  "ggplot2",
  "plotly",
  "ggthemes",
  "RColorBrewer"
)
missing_packages =
  required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(missing_packages)) {
  cat(paste(
    "Missing packages:",
    paste(missing_packages, collapse = ";"),
    "\nAttempting to install them."
  ))
  install.packages(missing_packages)
}

library(shinydashboard)
library(shinyBS)
library(data.table)
library(ggplot2)
library(plotly)
library(ggthemes)
library(RColorBrewer)

## Help text ----
helpTextServer = list(
  chBrobust ="Calculate robust statistics: median instead of the mean, MAD instead of SD.",
  nISeed = "<p>Seed of the random number generator.</p>
  <p>The same seed and the same parameters always produce the same plate.
  Changing a parameter then re-uses the same random noise, so that any change
  in the quality indices comes from that parameter alone.</p>",
  abResample = "Draw a new random plate from the same parameters.
  Use it to see how much the quality indices vary between runs.",
  alHitPerf = "<p>The wells spiked with the &quot;hit&quot; distribution are
  known, so the wells called by the z-score threshold can be scored against
  them. Only sample wells are counted; the controls are excluded.</p>
  <p>On the plate above, the colour is the well <i>called</i> by the
  threshold, while the circles mark the wells that were <i>really</i> spiked.
  A circle on a &quot;T&quot; well is a true positive, a circle on an
  &quot;F&quot; well a miss, and a &quot;T&quot; well without a circle a
  false positive.</p>
  <p>Lowering the threshold recovers more of the spiked wells (higher
  sensitivity) at the cost of calling more inactive wells (higher false
  positive rate). Set <i>#hits</i> above zero to populate the table.</p>",
  rBscreenType = "Choose antagonist when testing inhibitors, agonist for activators.",
  alPlateFormat = "Choose a plate format for synthetic data, e.g. 96-, 384-, 1536-well.",
  alSSMDcrit = "<p>Choose quality control level for the strictly 
  standardized mean difference (SSMD).</p>
  <p>Learn more <a href=\"https://en.wikipedia.org/wiki/Strictly_standardized_mean_difference#Quality_control\" target=\"_blank\">here</a>.</p>",
  alSSMDinfo = "<p>Strictly standardized mean difference (SSMD)
  is a measure of effect size. It is the mean divided by the 
  standard deviation of a difference of two random values respectively 
  from two groups.</p>
  <p>Learn more <a href=\"https://en.wikipedia.org/wiki/Strictly_standardized_mean_difference#Statistical_parameter\" target=\"_blank\">here</a>.</p>",
  alZprimeInfo = "<p>The Z'-factor is the characteristic parameter for 
  the quality of the assay itself, without intervention of test compounds.</p>
  <p>
  <table style=\"width:100%\">
  <tr>
    <th>Z'-factor</th>
    <th>Interpretation</th> 
  </tr>
  <tr>
    <td>1.0</td>
    <td>Theoretical maximum.</td> 
  </tr>
  <tr>
    <td>0.5 - 1.0</td>
    <td>An excellent assay.</td> 
  </tr>
  <tr>
    <td>0 - 0.5</td>
    <td>A marginal assay.</td> 
  </tr>
  <tr>
    <td>Less than 0</td>
    <td>Too much overlap between the positive and negative controls for the assay to be useful.</td> 
  </tr>
  </table>
  </p>
  <p>Learn more <a href=\"https://en.wikipedia.org/wiki/Z-factor#Definition\" target=\"_blank\">here</a>.</p>"
)

## Help pop-overs ----

# A label that carries its own Bootstrap popover, opened by clicking the
# label text and nothing else.
#
# shinyBS::addPopover cannot express that. Its JS resolves the target with
#   $("#id").closest(".shiny-input-container, .shiny-bound-output, .btn, ...")
# so a link used as an input label binds the popover to the whole control --
# clicking the dropdown to change a value pops the help open -- while a link
# outside any input container matches nothing and the popover silently never
# attaches at all. Binding straight to the element avoids both.
helpLink = function(inId,
                    inLabel,
                    inTitle,
                    inContent) {

  return(
    actionLink(inId,
               inLabel,
               # data-original-title rather than title, so the browser never
               # shows a native tooltip on hover before Bootstrap initialises
               `data-original-title` = inTitle,
               `data-toggle` = "popover",
               `data-trigger` = "click",
               `data-placement` = "bottom",
               `data-container` = "body",
               `data-html` = "true",
               `data-content` = inContent)
  )
}

# Initialises every helpLink() on the page, and closes an open popover when
# the next click lands outside it. Include once in the body.
helpLinkScript = tags$script(HTML("
$(function() {
  $('[data-toggle=\"popover\"]').popover();

  $(document).on('click', function(e) {
    $('[data-toggle=\"popover\"]').each(function() {
      var $t = $(this);
      if (!$t.is(e.target) &&
          $t.has(e.target).length === 0 &&
          $('.popover').has(e.target).length === 0) {
        $t.popover('hide');
      }
    });
  });
});
"))


## Plate quality ----

calcZfactor = function(inDT, 
                       inColMeas = "meas",
                       inColType = "type",
                       inWellType = list(ctrlNeg = "Ctrl Neg",
                                         ctrlPos = "Ctrl Pos",
                                         sample = "Sample"),
                       inScreenType = c("inhibition",
                                        "activation"),
                       inRobust = F) {
  
  inScreenType = match.arg(inScreenType)
  
  # Aggregate input data
  if(inRobust) {
    locDTaggr = inDT[,
                     .(wellMn = median(get(inColMeas)),
                       wellSd = mad(get(inColMeas))),
                     by = c(inColType)]
  } else {
    locDTaggr = inDT[,
                     .(wellMn = mean(get(inColMeas)),
                       wellSd = sd(get(inColMeas))),
                     by = c(inColType)]
  }
  
  # Calculate stats for samples
  locSampleMn = locDTaggr[get(inColType) == inWellType[["sample"]]][["wellMn"]]
  locSampleSd = locDTaggr[get(inColType) == inWellType[["sample"]]][["wellSd"]]
  
  # Calculate stats for the control
  if (inScreenType == "inhibition") {
    locCtrlMn = locDTaggr[get(inColType) == inWellType[["ctrlNeg"]]][["wellMn"]]
    locCtrlSd = locDTaggr[get(inColType) == inWellType[["ctrlNeg"]]][["wellSd"]]
    
  } else {
    locCtrlMn = locDTaggr[get(inColType) == inWellType[["ctrlPos"]]][["wellMn"]]
    locCtrlSd = locDTaggr[get(inColType) == inWellType[["ctrlPos"]]][["wellSd"]]
    
  }
  
  return(1 - (3*locSampleSd + 3*locCtrlSd)/abs(locSampleMn - locCtrlMn))
}


calcZprime = function(inDT, 
                      inColMeas = "meas",
                      inColType = "type",
                      inWellType = list(ctrlNeg = "Ctrl Neg",
                                        ctrlPos = "Ctrl Pos",
                                        sample = "Sample"),
                      inRobust = F) {
  
  # Aggregate input data
  if(inRobust) {
    locDTaggr = inDT[get(inColType) %in% c(inWellType[["ctrlNeg"]],
                                           inWellType[["ctrlPos"]]),
                     .(wellMn = median(get(inColMeas)),
                       wellSd = mad(get(inColMeas))),
                     by = c(inColType)]
  } else {
    locDTaggr = inDT[get(inColType) %in% c(inWellType[["ctrlNeg"]],
                                           inWellType[["ctrlPos"]]),
                     .(wellMn = mean(get(inColMeas)),
                       wellSd = sd(get(inColMeas))),
                     by = c(inColType)]
  }
  
  # Calculate stats for controls
  locCtrlNegMn = locDTaggr[get(inColType) == inWellType[["ctrlNeg"]]][["wellMn"]]
  locCtrlNegSd = locDTaggr[get(inColType) == inWellType[["ctrlNeg"]]][["wellSd"]]
  
  locCtrlPosMn = locDTaggr[get(inColType) == inWellType[["ctrlPos"]]][["wellMn"]]
  locCtrlPosSd = locDTaggr[get(inColType) == inWellType[["ctrlPos"]]][["wellSd"]]
  
  return(1 - (3*locCtrlPosSd + 3*locCtrlNegSd)/abs(locCtrlPosMn - locCtrlNegMn))
}

calcSSMD = function(inDT, 
                    inColMeas = "meas",
                    inColType = "type",
                    inWellType = list(ctrlNeg = "Ctrl Neg",
                                      ctrlPos = "Ctrl Pos",
                                      sample = "Sample"),
                    inScreenType = c("inhibition",
                                     "activation"),
                    inRobust = F) {
  
  inScreenType = match.arg(inScreenType)
  
  # Aggregate input data
  if(inRobust) {
    locDTaggr = inDT[get(inColType) %in% c(inWellType[["ctrlNeg"]],
                                           inWellType[["ctrlPos"]]),
                     .(wellMn = median(get(inColMeas)),
                       wellSd = mad(get(inColMeas))),
                     by = c(inColType)]
  } else {
    locDTaggr = inDT[get(inColType) %in% c(inWellType[["ctrlNeg"]],
                                           inWellType[["ctrlPos"]]),
                     .(wellMn = mean(get(inColMeas)),
                       wellSd = sd(get(inColMeas))),
                     by = c(inColType)]
  }
  
  # Calculate stats for controls
  locCtrlNegMn = locDTaggr[get(inColType) == inWellType[["ctrlNeg"]]][["wellMn"]]
  locCtrlNegSd = locDTaggr[get(inColType) == inWellType[["ctrlNeg"]]][["wellSd"]]
  
  locCtrlPosMn = locDTaggr[get(inColType) == inWellType[["ctrlPos"]]][["wellMn"]]
  locCtrlPosSd = locDTaggr[get(inColType) == inWellType[["ctrlPos"]]][["wellSd"]]
  
  if (inScreenType == "inhibition") {
    locRes = (locCtrlPosMn - locCtrlNegMn) / sqrt(locCtrlPosSd^2 + locCtrlNegSd^2)
  } else {
    locRes = (locCtrlNegMn - locCtrlPosMn) / sqrt(locCtrlPosSd^2 + locCtrlNegSd^2)
  }
  
  return(locRes)
}

## Plate normalisations ----

calcZscore = function(inDT, 
                      inColMeas = "meas",
                      inColType = "type",
                      inWellType = list(ctrlNeg = "Ctrl Neg",
                                        ctrlPos = "Ctrl Pos",
                                        sample = "Sample"),
                      inRobust = F) {

  # Work on a copy; the caller's data.table must not gain columns
  inDT = copy(inDT)

  if(inRobust) {
    locDTaggr = inDT[get(inColType) == inWellType[["sample"]],
                     .(wellMn = median(get(inColMeas)),
                       wellSd = mad(get(inColMeas)))]
  } else {
    locDTaggr = inDT[get(inColType) == inWellType[["sample"]],
                     .(wellMn = mean(get(inColMeas)),
                       wellSd = sd(get(inColMeas)))]
  }
  
  inDT[,
       zScore := (get(inColMeas) - locDTaggr[["wellMn"]]) / locDTaggr[["wellSd"]]]
  
  return(inDT)
}

calcNPI = function(inDT, 
                   inColMeas = "meas",
                   inColType = "type",
                   inWellType = list(ctrlNeg = "Ctrl Neg",
                                     ctrlPos = "Ctrl Pos",
                                     sample = "Sample"),
                   inScreenType = c("inhibition",
                                    "activation"),
                   inRobust = F) {

  inScreenType = match.arg(inScreenType)

  # Work on a copy; the caller's data.table must not gain columns
  inDT = copy(inDT)

  # Aggregate input data
  if(inRobust) {
    locDTaggr = inDT[get(inColType) %in% c(inWellType[["ctrlNeg"]],
                                           inWellType[["ctrlPos"]]),
                     .(wellMn = median(get(inColMeas))),
                     by = c(inColType)]
  } else {
    locDTaggr = inDT[get(inColType) %in% c(inWellType[["ctrlNeg"]],
                                           inWellType[["ctrlPos"]]),
                     .(wellMn = mean(get(inColMeas))),
                     by = c(inColType)]
  }
  
  # Calculate stats for controls
  locCtrlNegMn = locDTaggr[get(inColType) == inWellType[["ctrlNeg"]]][["wellMn"]]
  locCtrlPosMn = locDTaggr[get(inColType) == inWellType[["ctrlPos"]]][["wellMn"]]
  
  if (inScreenType == "activation") {
    inDT[,
         NPI := 100 *(locCtrlPosMn - get(inColMeas)) / (locCtrlPosMn - locCtrlNegMn)]
  } else {
    inDT[,
         NPI := 100 *(locCtrlNegMn - get(inColMeas)) / (locCtrlNegMn - locCtrlPosMn)]
  }
  
  return(inDT)
  
}


## Hit selection ----

calcHits = function(inDT, 
                    inColMeas = "meas",
                    inColType = "type",
                    inWellType = list(ctrlNeg = "Ctrl Neg",
                                      ctrlPos = "Ctrl Pos",
                                      sample = "Sample"),
                    inScreenType = c("inhibition",
                                     "activation"),
                    inThr = -3,
                    inRobust = F) {

  inScreenType = match.arg(inScreenType)

  # Work on a copy; the caller's data.table must not gain columns
  inDT = copy(inDT)

  if(inRobust) {
    locDTaggr = inDT[get(inColType) == inWellType[["sample"]],
                     .(wellMn = median(get(inColMeas)),
                       wellSd = mad(get(inColMeas)))]
  } else {
    locDTaggr = inDT[get(inColType) == inWellType[["sample"]],
                     .(wellMn = mean(get(inColMeas)),
                       wellSd = sd(get(inColMeas)))]
  }
  
  inDT[,
       zScore := (get(inColMeas) - locDTaggr[["wellMn"]]) / locDTaggr[["wellSd"]]]
  
  if (inScreenType == "inhibition") {
    inDT[,
         hits := zScore < inThr]
  } else {
    inDT[,
         hits := zScore > inThr]
  }

  return(inDT)
}

# Compare called hits against the wells that were actually spiked, and
# return the confusion matrix together with the rates derived from it.
#
# Only sample wells are counted: hit calling asks which test compounds are
# active, so scoring the controls would credit the assay for recovering
# positive controls it was calibrated on.
calcHitPerf = function(inDT,
                       inColTruth = "spiked",
                       inColPred = "hits",
                       inColType = "type",
                       inWellType = list(ctrlNeg = "Ctrl Neg",
                                         ctrlPos = "Ctrl Pos",
                                         sample = "Sample")) {

  locDT = inDT[get(inColType) == inWellType[["sample"]]]

  locTruth = locDT[[inColTruth]]
  locPred = locDT[[inColPred]]

  locTP = sum(locTruth & locPred)
  locFP = sum(!locTruth & locPred)
  locFN = sum(locTruth & !locPred)
  locTN = sum(!locTruth & !locPred)

  # NA rather than NaN on an empty denominator, e.g. the true positive rate
  # with no spiked wells, so that the caller can print it as "n/a"
  locRate = function(inNum, inDen) {
    if (inDen > 0)
      return(inNum / inDen)
    else
      return(NA_real_)
  }

  return(list(TP = locTP,
              FP = locFP,
              FN = locFN,
              TN = locTN,
              TPR = locRate(locTP, locTP + locFN),
              FPR = locRate(locFP, locFP + locTN),
              PPV = locRate(locTP, locTP + locFP)))
}


## Plotting ----

# Default diverging palette for plate heatmaps
hmColors = rev(RColorBrewer::brewer.pal(n = 11,
                                        name = "RdYlBu"))

# Plot a single column of a plate data.table as a plotly heatmap.
# Expects the row/col grid added by dataPlotHM() in server.R.
# Pass inColors = NULL to supply a custom colorscale via ...
#
# inColMark names a logical column; the wells it flags are overlaid with a
# marker, which lets a second, independent property be read off the same
# plate, e.g. which of the called hits were really spiked. inMarkSize needs
# to shrink as the plate format grows, since the cells do.
plotPlateHM = function(inDT,
                       inColVal,
                       inHoverLabel,
                       inColType = "type",
                       inColors = hmColors,
                       inColMark = NULL,
                       inMarkLabel = "Marked",
                       inMarkSize = 6,
                       ...) {

  # Rows are plotted bottom-up, hence the reversal of both the
  # axis labels and the matrix rows
  locRow = rev(unique(inDT[["row"]]))
  locCol = unique(inDT[["col"]])
  locRev = seq(from = length(locRow),
               to = 1,
               by = -1)

  locM = matrix(as.numeric(inDT[[inColVal]]),
                nrow = length(locRow),
                ncol = length(locCol))[locRev, ]

  # Well types shown in the hover tooltip
  locText = matrix(inDT[[inColType]],
                   nrow = length(locRow),
                   ncol = length(locCol))[locRev, ]

  locArgs = list(
    x = locCol,
    y = locRow,
    z = locM,
    text = locText,
    type = "heatmap",
    hovertemplate = paste0(inHoverLabel, ": %{z}<br>",
                           "Col: %{x}<br>",
                           "Row: %{y}<br>",
                           "Type: %{text}",
                           "<extra></extra>"),
    ...
  )

  if (!is.null(inColors))
    locArgs[["colors"]] = inColors

  locP = do.call(plot_ly, locArgs)

  if (is.null(inColMark))
    return(locP)

  # %in% TRUE rather than == TRUE, so that NA never selects a well
  locMarkDT = inDT[get(inColMark) %in% TRUE]

  if (nrow(locMarkDT) == 0L)
    return(locP)

  # A white dot with a dark rim, legible against either end of the
  # underlying colour scale. inherit = FALSE keeps the heatmap's z and
  # text attributes off this trace.
  locP = add_trace(locP,
                   x = as.character(locMarkDT[["col"]]),
                   y = as.character(locMarkDT[["row"]]),
                   type = "scatter",
                   mode = "markers",
                   name = inMarkLabel,
                   marker = list(symbol = "circle",
                                 size = inMarkSize,
                                 color = "rgba(255,255,255,0.9)",
                                 line = list(color = "rgb(40,40,40)",
                                             width = 1)),
                   hovertemplate = paste0(inMarkLabel, "<br>",
                                          "Col: %{x}<br>",
                                          "Row: %{y}",
                                          "<extra></extra>"),
                   showlegend = TRUE,
                   inherit = FALSE)

  # Horizontal legend below the plate, clear of the colourbar
  locP = layout(locP,
                legend = list(orientation = "h",
                              x = 0,
                              y = -0.15))

  return(locP)
}


## Value boxes ----

# valueBox that behaves as an action button, so that clicking it
# opens the bsModal named modal_<inId> declared in ui.R.
# The button itself is exposed to the server as input$button_<inId>.
clickableValueBox = function(inValue,
                             inSubtitle,
                             inColor,
                             inId) {

  locBox = valueBox(value = inValue,
                    href = "#",
                    subtitle = inSubtitle,
                    color = inColor)

  locBox$children[[1]]$attribs$class = "action-button"
  locBox$children[[1]]$attribs$id = paste0("button_", inId)

  return(locBox)
}


