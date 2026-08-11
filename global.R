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
  dbData = "Download the plate as a CSV file, one row per well, with the raw
  measurement, both normalisations, the ground truth and the current hit call.
  The file name records the seed and resample count that generated it.",
  alCtrlLayout = "<p>Where the control wells sit on the plate.</p>
  <p><i>First columns</i> fills the plate from column 1, <i>edge columns</i>
  puts the controls on the outer columns, and <i>interleaved</i> spreads them
  evenly.</p>
  <p>The layout only matters once a plate artifact is switched on, and what
  it decides is how <i>evenly</i> the artifact hits the controls. Z-prime is
  built from the difference between the two control means, so a bias shared
  equally by both cancels out of it: controls sitting at one uniform depth
  can be badly offset and Z-prime will not notice. Spread the controls over
  wells the artifact treats differently and Z-prime drops sharply.</p>
  <p>So a healthy Z-prime is not by itself evidence of a clean plate. Turn on
  an edge effect and compare the layouts against the hit scoring below the
  Hits heatmap.</p>",
  alArtifacts = "<p>Systematic spatial effects added on top of the random
  well-to-well noise, in measurement units.</p>
  <p>The two <i>gradients</i> ramp linearly across the rows and the columns,
  the slider being the total change from one side of the plate to the other.
  The <i>edge effect</i> shifts the outermost frame of wells and decays
  inwards.</p>
  <p>Artifacts apply to every well, controls included. They inflate the
  spread of the sample wells, which is the denominator of the z-score, so
  genuine hits stop clearing the threshold and sensitivity falls away. This
  can happen while Z-prime still reads as excellent, and it is why real
  screens normalise spatially instead of trusting a single quality index.</p>",
  alHitMetric = "<p>Which quantity the hit threshold is applied to.</p>
  <p><i>Raw measurement</i> uses the well value as it stands, <i>z-score</i>
  standardises it against the spread of the sample wells, and <i>NPI</i>
  rescales it between the two controls.</p>
  <p>Mind the direction. An active well has a <b>low</b> raw value and a
  <b>low</b> z-score in an antagonist screen, but a <b>high</b> NPI, because
  NPI is anchored at 0 for an inactive well and 100 for a fully affected one.
  The comparison flips accordingly, so an NPI threshold selects wells above
  it while the other two select wells below.</p>
  <p>The three sit on different scales, so the slider is re-ranged when the
  metric changes.</p>",
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


## Plate geometry and layout ----

plateDims = function(inFormat) {
  return(
    switch(as.character(inFormat),
           "96"   = list(nRow = 8,  nCol = 12),
           "384"  = list(nRow = 16, nCol = 24),
           "1536" = list(nRow = 32, nCol = 48))
  )
}

# Plate rows are lettered A..Z and then AA, AB, ... , which for the 32 rows
# of a 1536-well plate means A..Z followed by AA..AF
plateRowLabels = function(inNrow) {
  return(c(LETTERS,
           paste0("A", LETTERS))[seq_len(inNrow)])
}

# Grid positions handed to the control wells, in preference order.
# Positions are numbered column-major, i.e. (col - 1) * nRow + row.
#
#   first  - fill the plate from the first column on (the original layout)
#   edge   - the outermost columns, working inwards
#   spread - spaced evenly over the whole plate
#
# Every branch returns a full ordering rather than just the first inNctrl
# positions, so that a control count larger than the preferred region still
# has somewhere to go: with both control sliders at their maximum the edge
# columns of a 96-well plate cannot hold them all.
ctrlPositions = function(inNrow,
                         inNcol,
                         inNctrl,
                         inLayout = c("first", "edge", "spread")) {

  inLayout = match.arg(inLayout)

  locN = inNrow * inNcol
  locAll = seq_len(locN)

  if (inNctrl < 1L)
    return(integer(0))

  locPos = switch(
    inLayout,

    "first" = locAll,

    "edge" = {
      # columns ordered 1, nCol, 2, nCol-1, ...
      locHalf = inNcol %/% 2L
      locOrder = as.vector(rbind(seq_len(locHalf),
                                 rev(seq_len(inNcol))[seq_len(locHalf)]))
      if (inNcol %% 2L == 1L)
        locOrder = c(locOrder, locHalf + 1L)

      as.vector(vapply(locOrder,
                       function(inC) (inC - 1L) * inNrow + seq_len(inNrow),
                       numeric(inNrow)))
    },

    "spread" = {
      locSeq = unique(round(seq(from = 1,
                                to = locN,
                                length.out = inNctrl)))
      c(locSeq, setdiff(locAll, locSeq))
    })

  return(as.integer(locPos[seq_len(inNctrl)]))
}

# Place the wells of inDT on the plate grid and label them.
# Rows of inDT are expected in the order they are generated: controls first,
# then samples. The result is sorted column-major by position, which is the
# order plotPlateHM() reshapes into a matrix.
assignPlatePositions = function(inDT,
                                inNrow,
                                inNcol,
                                inLayout = "first",
                                inColType = "type",
                                inWellType = list(ctrlNeg = "Ctrl Neg",
                                                  ctrlPos = "Ctrl Pos",
                                                  sample = "Sample")) {

  inDT = copy(inDT)

  locN = inNrow * inNcol
  stopifnot(nrow(inDT) == locN)

  locIsCtrl = inDT[[inColType]] != inWellType[["sample"]]

  locCtrlPos = ctrlPositions(inNrow,
                             inNcol,
                             sum(locIsCtrl),
                             inLayout)
  locSamplePos = setdiff(seq_len(locN), locCtrlPos)

  locPos = integer(locN)
  locPos[locIsCtrl] = locCtrlPos
  locPos[!locIsCtrl] = locSamplePos

  inDT[, `:=`(rowIdx = ((locPos - 1L) %% inNrow) + 1L,
              colIdx = ((locPos - 1L) %/% inNrow) + 1L)]

  inDT[, `:=`(row = plateRowLabels(inNrow)[rowIdx],
              col = factor(colIdx, levels = seq_len(inNcol)))]

  setorder(inDT, colIdx, rowIdx)

  return(inDT[])
}


## Plate artifacts ----

# Systematic spatial effects laid on top of the well measurements.
#
# Applied to every well, controls included: an edge effect that biases the
# border is precisely what wrecks Z-prime once the controls sit there, which
# is the point of being able to move them.
addPlateArtifacts = function(inDT,
                             inNrow,
                             inNcol,
                             inColMeas = "meas",
                             inGradRow = 0,
                             inGradCol = 0,
                             inEdge = 0,
                             inEdgeDecay = 1.5) {

  inDT = copy(inDT)

  # Gradients span the plate, so a slider value is the total change from
  # one side to the other rather than a per-well step
  locGrad = numeric(nrow(inDT))

  if (inNrow > 1L)
    locGrad = locGrad + inGradRow * (inDT[["rowIdx"]] - 1L) / (inNrow - 1L)

  if (inNcol > 1L)
    locGrad = locGrad + inGradCol * (inDT[["colIdx"]] - 1L) / (inNcol - 1L)

  # Ring 1 is the outermost frame of wells; the effect decays inwards
  locRing = pmin(inDT[["rowIdx"]],
                 inNrow - inDT[["rowIdx"]] + 1L,
                 inDT[["colIdx"]],
                 inNcol - inDT[["colIdx"]] + 1L)

  locEdge = inEdge * exp(-(locRing - 1L) / inEdgeDecay)

  inDT[, (inColMeas) := get(inColMeas) + locGrad + locEdge]

  return(inDT[])
}


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

# The quantities a hit threshold can be applied to, with the slider setup
# each one needs. Single source of truth for ui.R and server.R.
hitMetrics = list(
  zscore = list(name = "z-score",
                col = "zScore",
                label = "z-score threshold",
                min = -6, max = 6, value = -3, step = 0.1),

  raw = list(name = "Raw measurement",
             col = "meas",
             label = "Raw measurement threshold",
             min = -6, max = 10, value = 1, step = 0.1),

  npi = list(name = "NPI",
             col = "NPI",
             label = "NPI threshold",
             min = -50, max = 150, value = 50, step = 1)
)

# Flag wells whose inMetric value falls beyond inThr.
#
# Direction: an active well has a low measurement in an inhibition screen and
# a high one in an activation screen, so raw and z-score are compared in that
# direction. NPI is scaled against the controls and runs the opposite way in
# BOTH screen types -- an inhibition hit sits near 100 and an activation hit
# near 0 -- so its comparison is flipped.
calcHits = function(inDT,
                    inColMeas = "meas",
                    inColType = "type",
                    inWellType = list(ctrlNeg = "Ctrl Neg",
                                      ctrlPos = "Ctrl Pos",
                                      sample = "Sample"),
                    inScreenType = c("inhibition",
                                     "activation"),
                    inMetric = c("zscore",
                                 "raw",
                                 "npi"),
                    inThr = -3,
                    inRobust = F) {

  inScreenType = match.arg(inScreenType)
  inMetric = match.arg(inMetric)

  # Work on a copy; the caller's data.table must not gain columns
  inDT = copy(inDT)

  # Add whichever column the threshold is applied to
  if (inMetric == "zscore") {
    inDT = calcZscore(inDT,
                      inColMeas = inColMeas,
                      inColType = inColType,
                      inWellType = inWellType,
                      inRobust = inRobust)
    locCol = "zScore"

  } else if (inMetric == "npi") {
    inDT = calcNPI(inDT,
                   inColMeas = inColMeas,
                   inColType = inColType,
                   inWellType = inWellType,
                   inScreenType = inScreenType,
                   inRobust = inRobust)
    locCol = "NPI"

  } else {
    locCol = inColMeas
  }

  locBelow = (inScreenType == "inhibition")

  if (inMetric == "npi")
    locBelow = !locBelow

  if (locBelow) {
    inDT[,
         hits := get(locCol) < inThr]
  } else {
    inDT[,
         hits := get(locCol) > inThr]
  }

  return(inDT[])
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

  # Adding the scatter trace makes plotly derive the category order of both
  # axes from that trace alone and sort it as strings, which scrambles the
  # columns (10, 11, 12, 5, 6, ...) and undoes the row reversal that puts
  # row A at the top. Pin both axes back to the heatmap's own ordering.
  locP = layout(locP,
                legend = list(orientation = "h",
                              x = 0,
                              y = -0.15),
                xaxis = list(type = "category",
                             categoryorder = "array",
                             categoryarray = as.character(locCol)),
                yaxis = list(type = "category",
                             categoryorder = "array",
                             categoryarray = as.character(locRow)))

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


