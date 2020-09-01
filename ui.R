#
# screenQC: a Shiny app for exploring screen quality in HCS
# Author: Maciej Dobrzynski
#
# This is the UI logic for a Shiny web application.
#

ui <- dashboardPage(
  dashboardHeader(title = "screenQC"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", 
               tabName = "dashboard", 
               icon = icon("dashboard"))),
    disable = T
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(width = 8,
                    title = "Configuration",
                    column(
                      width = 4,
                      selectInput("selPlateFormat",
                                  "Plate format",
                                  choices = list("96" = 96,
                                                 "384" = 384,
                                                 "1536" = 1536),
                                  selected = "384")
                    ),
                    
                    column(
                      width = 4,
                      selectInput("selSSMDctrl",
                                  "SSMD control",
                                  choices = list("Moderate" = "moderate",
                                                 "Strong" = "strong",
                                                 "Very strong" = "verystrong",
                                                 "Extremely strong" = "extremelystrong"))),
                    
                    column(
                      width = 2,
                      checkboxInput("chBrobust",
                                    "Robust",
                                    value = F)
                    )
                ),
              ),
              
              fluidRow(
                box(
                  width = 4,
                  title = "Samples",
                  sliderInput("slSamplesMn", "Mean:", min = 0, max = 4, value = 1.2, step = 0.1),
                  sliderInput("slSamplesSd", "SD:", min = 0, max = 2, value = 0.1, step = 0.1),
                  sliderInput("slHitsN", "#hits:", min = 0, max = 24, value = 0, step = 1)
                ),
                
                box(
                  width = 4,
                  title = "Negative Control",
                  sliderInput("slCtrlNegMn", "Mean:", min = 0, max = 4, value = 1.5, step = 0.1),
                  sliderInput("slCtrlNegSd", "SD:", min = 0, max = 2, value = 0.1, step = 0.1),
                  sliderInput("slCtrlNegN", "#wells:", min = 1, max = 32, value = 8, step = 1)
                ),
                
                box(
                  width = 4,
                  title = "Positive Control",
                  sliderInput("slCtrlPosMn", "Mean:", min = 0, max = 4, value = 0.2, step = 0.1),
                  sliderInput("slCtrlPosSd", "SD:", min = 0, max = 2, value = 0.1, step = 0.1),
                  sliderInput("slCtrlPosN", "#wells:", min = 1, max = 32, value = 8, step = 1)
                )
                
              ),
              
              fluidRow(
                tabBox(
                  width = 8,
                  tabPanel("Dist",
                           plotOutput("plotDist", height = 400)),
                  tabPanel("Box-plot",
                           plotOutput("plotBox", height = 400)),
                  tabPanel("Raw",
                           plotlyOutput("plotlyHMraw", height = 400)),
                  tabPanel("zScore",
                           plotlyOutput("plotlyHMz", height = 400)),
                  tabPanel("NPI",
                           plotlyOutput("plotlyHMn", height = 400))
                  
                ),
                valueBoxOutput("vBzfactor"),
                valueBoxOutput("vBzprime"),
                valueBoxOutput("vBssmd")
              )
      )
    )
  )
)