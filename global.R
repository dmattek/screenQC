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


