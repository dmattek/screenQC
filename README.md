# screenQC

Interactively explore quantitative concepts of high-content screening.

Access the running instance of the web-app [here](https://macdobry.shinyapps.io/screenQC/ "External link to a running app").

## What does the app do?

The app illustrates quantitative concepts of [high-content screening](https://en.wikipedia.org/wiki/High-content_screening "External link to Wikipedia"), such as a z-score, z-factor, z-prime, normalised percentage inhibition, standardized mean difference.

The app generates synthetic data for a desired plate format (e.g. 96, 384, 1536-well). The data includes the measurement samples, and the negative and positive controls. The user can change the number of control wells, their sample mean and standard deviation. Additionally, "hits" with the mean and standard deviation corresponding to the positive control can be added to the dataset.

Synthetic data can be plotted as distributions, box-plots or heatmaps with raw or normalized data. Normalisations include the [z-score](https://en.wikipedia.org/wiki/Standard_score) and Normalised Percentage Inhibition.

The app calculates the follwoing plate quality indices:

- [SSMD](https://en.wikipedia.org/wiki/Strictly_standardized_mean_difference), strictly standardized mean difference,
- [Z-factor](https://en.wikipedia.org/wiki/Z-factor) and Z-prime.

## Further reading

1. *Increasing the Content of High-Content Screening: An Overview* (2014); [DOI: 10.1177/1087057114528537](https://dx.doi.org/10.1177/1087057114528537 "External link").
2. *Data Analysis Approaches in High Throughput Screening* (2013); [DOI: 10.5772/52508](https://dx.doi.org/10.5772/52508 "External link").
3. *Statistical practice in high-throughput screening data analysis* (2006); [DOI: 10.1038/nbt1186](https://dx.doi.org/10.1038/nbt1186 "External link").
4. *A Simple Statistical Parameter for Use in Evaluation and Validation of High Throughput Screening Assays* (1999); [DOI: 10.1177/108705719900400206](https://doi.org/10.1177/108705719900400206 "External link").

## Getting started

The app can be run either locally or on a server. The installation process is detailed below.

### Run locally

#### Prerequisites
The app works on all major OS (Windows, macOS, standard Linux distributions) and requires a recent R installation. The R installation guide can be found on the official [website](https://www.r-project.org/).

The app depends on several packages which should be automatically installed when the app is launched for the first time. Alternatively, the complete list of dependencies can be manually installed from the R console by typing:

```r
install.packages(
  c(
    "shiny", "shinydashboard",
    "data.table", 
    "ggplot2", "ggthemes","plotly")) 
```

For new R users, once you have installed R, we recommend to use R from Rstudio. It provides an excellent programming interface and makes it slightly easier to run shiny apps with a single click! Instructions for installation can be found on the Rstudio [website](https://rstudio.com/ "External link").

#### Install and run
First, download the latest version of the app directly from the [GitHub repository](https://github.com/dmattek/screenQC)(green button *Clone or download*, download as zip). Unzip the folder and place it in your favorite location. 

If you have installed RStudio, launch it and go to *File -> Open Project*. Navigate to the location where you placed the app and open the file `screenQC.Rproj`. This will load the app in the current Rstudio session. To start the app, open the `server.R` or the `ui.R` file in the Rstudio session, then click the *Run App* button with a green triangle in the upper right corner of the window with code open.

If you have R but did not (or do not want to) install Rstudio, you can also start the app directly from your OS's command line with:

```
R -e "shiny::runApp('path-to-application-folder')"
```

Then, open your web browser and point to the address given by the output of that command.

### Run from GitHub
If you already have an R installation with all the packages you can also run the following lines in your R console to start immediatly with a temporary copy of the app:

```
library(shiny)
runGitHub("dmattek/screenQC")
```

However, for performance reasons you might prefer to switch to an offline installation as outlined above!

### Run on a server
To deploy the app on RStudio/Shiny server, follow the instructions [here](https://shiny.rstudio.com/deploy/ "External link: shiny hosting").

