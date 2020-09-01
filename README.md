# screenQC

## What does the app do?

It is an interactive web-app to explore parameters related to quality control in high-content screening.

The app generates synthetic data for a desired plate format (96, 384, 1536-well). The data includes samples, negative and positive controls. The user can change the number of control wells, their sample mean and standard deviation. Additionally, "hits" with the mean and standard deviation corresponding to the positive control can be added to the dataset.

Synthetic data can be plotted as distributions, box-plots or heatmaps with raw or normalized data. Normalisations include the [z-score](https://en.wikipedia.org/wiki/Standard_score) and Normalised Percentage Inhibition..

The app calculates the follwoing plate quality indices:

- [SSMD](https://en.wikipedia.org/wiki/Strictly_standardized_mean_difference), strictly standardized mean difference,
- [Z-factor](https://en.wikipedia.org/wiki/Z-factor) and Z-prime.

## Getting started

The app can be run either locally or on a server. The installation process is detailed below.

### Running the app locally

#### Prerequisites
The app works on all major OS (Windows, macOS, standard Linux distributions) and only requires a recent R installation on which the standard package `shiny` is installed. Instructions to install R can be found on the official [R-project website](https://www.r-project.org/). Once R is installed, one can install the `shiny` package by typing: `install.packages("shiny")` in the R console.

The app depends on several other packages which should be automatically installed when the user launches the app for the first time. The complete list of dependencies can also be manually installed from the R console by typing:

```
install.packages(c("shiny", "shinydashboard",
			"data.table", 
			"ggplot2", "ggthemes","plotly")) 
```

For new R users, once you have installed R, we recommend to use R from Rstudio. It provides an excellent programming interface and makes it slightly easier to run ishiny apps with a single click! Instructions for installation can be found on the [Rstudio website](https://rstudio.com/).

#### Install and Start the App
First, download the latest version of the app directly from the [GitHub repository](https://github.com/dmattek/screenQC)(green button *Clone or download*, download as zip). Unzip the folder and place it in your favorite location. 

If you have installed RStudio, launch it and go to *File -> Open Project*. In the contextual menu navigate to the location where you placed the app and open the file `screenQC.Rproj`. This will load the app in the current Rstudio session. To start the app, open the `server.R` or the `ui.R` file in the Rstudio session, then click the *Run App* button with a green triangle in the upper right corner of the window with code open.

If you did not install Rstudio, or do not wish to use it, you can also start the app directly from your OS's command line with:
```
R -e "shiny::runApp('path-to-application-folder')"
```
In this case, open your web browser and point to the address given by the output of that command.

### Running the app directly from GitHub
If you have already an R installation with `shiny` installed you can also run the two following lines in your R console to get immediatly started with a temporary copy of the app:
```
library(shiny)
runGitHub("dmattek/screenQC")
```
If you like it, we strongly recommend that you switch to a regular installation! This will make it work offline without the need to download the source code at every start.

### Running instance
Access the running instance of the app at [shinyapps.io](https://macdobry.shinyapps.io/screenQC/ "Link to a running app").

### Running the app on a server
The app can be deployed on RStudio/Shiny server. Please follow the instructions [here](https://shiny.rstudio.com/deploy/ "Shiny - Hosting").

