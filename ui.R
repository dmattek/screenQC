#
# screenQC: a Shiny app for exploring screen quality in HCS
# Author: Maciej Dobrzynski
#
# This is the UI logic for a Shiny web application.
#

myHeader = dashboardHeader(title = "screenQC")

mySidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", 
             tabName = "dashboard", 
             icon = icon("dashboard"))),
  disable = T)

myBody = dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              box(width = 12,
                  title = "Configuration",
                  column(
                    width = 3,
                    selectInput("selPlateFormat",
                                actionLink("alPlateFormat", "Plate format"),
                                choices = list("96" = 96,
                                               "384" = 384,
                                               "1536" = 1536),
                                selected = "384")
                  ),
                  
                  column(
                    width = 3,
                    selectInput("selSSMDctrl",
                                actionLink("alSSMDcrit", "SSMD control"),
                                choices = list("Moderate" = "moderate",
                                               "Strong" = "strong",
                                               "Very strong" = "verystrong",
                                               "Extremely strong" = "extremelystrong"))),
                  
                  column(
                    width = 3,
                    
                    radioButtons("rBscreenType",
                                 "Scren type",
                                 choices = list("Agonist" = "activation",
                                                "Antagonist" = "inhibition"),
                                 selected = "inhibition"),
                    bsTooltip('rBscreenType', 
                              helpTextServer[["rBscreenType"]], 
                              placement = "top", 
                              trigger = "hover", 
                              options = NULL)
                  ),
                  
                  column(
                    width = 3,
                    checkboxInput("chBrobust",
                                  "Robust stats",
                                  value = F),
                    bsTooltip('chBrobust', 
                              helpTextServer[["chBrobust"]], 
                              placement = "top", 
                              trigger = "hover", 
                              options = NULL)
                  )
              ),
            ),
            
            fluidRow(
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
              ),
              
              box(
                width = 4,
                title = "Hits",
                sliderInput("slHitsMn", "Mean:", min = 0, max = 4, value = 0.5, step = 0.1),
                sliderInput("slHitsSd", "SD:", min = 0, max = 2, value = 0.1, step = 0.1),
                sliderInput("slHitsN", "#hits:", min = 0, max = 36, value = 0, step = 1)
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
                         plotlyOutput("plotlyHMzscore", height = 400)),
                tabPanel("NPI",
                         plotlyOutput("plotlyHMnpi", height = 400)),
                tabPanel("Hits",
                         plotlyOutput("plotlyHMhits", height = 400),
                         sliderInput("slHitsThr", 
                                     "z-score threshold", 
                                     min = -6, max = 6, 
                                     value = -3))
              ),
              #valueBoxOutput("vBzfactor"),
              valueBoxOutput("vBzprime"),
              bsModal("modal_vBzprime", 
                      "Z'-factor", 
                      HTML(helpTextServer[["alZprimeInfo"]]),
                      trigger = "click"),
              
              valueBoxOutput("vBssmd"),
              bsModal("modal_vBssmd", 
                      "SSMD", 
                      HTML(helpTextServer[["alSSMDinfo"]]),
                      trigger = "click")
            )
    )
  )
)


ui <- dashboardPage(
  myHeader,
  
  ## Sidebar content
  mySidebar,
  
  ## Body content
  myBody
)