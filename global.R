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
  "data.table",
  "ggplot2",
  "plotly",
  "ggthemes"
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
library(data.table)
library(ggplot2)
library(ggthemes)
library(plotly)

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
  
  return((locCtrlPosMn - locCtrlNegMn) / sqrt(locCtrlPosSd^2 + locCtrlNegSd^2))
}

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




