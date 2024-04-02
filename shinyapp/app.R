#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(tmap)
library(spdep)
library(sf)
library(sp)

library(GWmodel)
library(plotly)
library(ClustGeo)
library(dendextend)
library(GGally)
library(ggdendro)
library(corrplot)
library(DT)
library(shinyWidgets)
library(RColorBrewer)


# GWR Level of Detail
varGwrLod <- c(
  "LAD"="LAD",
  "Ward"="Ward",
  "MSOA"="MSOA"
)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    # -----Navigation Bar
    navbarPage("SGSAS", fluid=TRUE, windowTitle="Simple Geo-Spatial Analysis using R and Shiny ", selected="eda",
               tabPanel("GWR", value="gwr", fluid=TRUE, icon=icon("laptop-code"),
                        sidebarLayout(position="left", fluid=TRUE,
                                      sidebarPanel(width=3, fluid=TRUE,
                                                   fluidRow(
                                                     column(5,
                                                            fluidRow(       
                                                              selectInput(inputId="GwrLod",
                                                                          label="GWR Level",
                                                                          choices=varGwrLod,
                                                                          selected="LAD",
                                                                          multiple=FALSE,
                                                                          width="97%"
                                                              ))))),
                                      mainPanel(width=9, fluid=TRUE,
                                                fluidRow(
                                                  column(6,
                                                         leafletOutput("gwr1"),
                                                         column(6,
                                                                selectInput(inputId="Gwr1Reference",
                                                                            label="Reference Value",
                                                                            choices=c("Local R2"="Local_R2"
                                                                            ),
                                                                            selected=NULL,
                                                                            multiple=FALSE,
                                                                            width="100%"
                                                                )),
                                                         column(6,
                                                                selectInput(inputId="Gwr1Binning",
                                                                            label="Binning Method",
                                                                            choices=c("Std Deviation"="sd",
                                                                                      "Equal"="equal",
                                                                                      "Pretty"="pretty",
                                                                                      "Quantile"="quantile",
                                                                                      "K-means Cluster"="kmeans",
                                                                                      "Hierarchical Cluster"="hclust",
                                                                                      "Bagged Cluster"="bclust",
                                                                                      "Fisher"="fisher",
                                                                                      "Jenks"="jenks",
                                                                                      "Log10 Pretty"="log10_pretty"
                                                                            ),
                                                                            selected="quantile",
                                                                            multiple=FALSE,
                                                                            width="100%"
                                                                )),
                                                         sliderInput(inputId="Gwr1N",
                                                                     label="Select number of classes",
                                                                     min=2,
                                                                     max=30,
                                                                     value=5,
                                                                     width="100%"
                                                         )
                                                  ),
                                                  column(6,
                                                         leafletOutput("gwr2"),
                                                         column(4,
                                                                HTML(
                                                                  '<div class="info-box-content" style="font-weight: lighter; font-size: smaller">
                                                  <span class="info-box-text">GWR Adj R2: </span>
                                                  <span class="info-box-number">
                                                    <div id="showGwrR2" class="shiny-text-output" style="font-weight: bolder; font-size: larger"></div>
                                                  </span>
                                                  </div>'
                                                                ),
                                                                HTML(
                                                                  '<div class="info-box-content" style="font-weight: lighter; font-size: smaller">
                                                  <span class="info-box-text">LM Adj R2: </span>
                                                  <span class="info-box-number">
                                                    <div id="showLmR2" class="shiny-text-output" style="font-weight: bolder; font-size: larger"></div>
                                                  </span>
                                                  </div>'
                                                                )),
                                                         column(4,
                                                                HTML(
                                                                  '<div class="info-box-content" style="font-weight: lighter; font-size: smaller">
                                                  <span class="info-box-text">GWR AICc: </span>
                                                  <span class="info-box-number">
                                                    <div id="showGwrAic" class="shiny-text-output" style="font-weight: bolder; font-size: larger"></div>
                                                  </span>
                                                  </div>'
                                                                ),
                                                                HTML(
                                                                  '<div class="info-box-content" style="font-weight: lighter; font-size: smaller">
                                                  <span class="info-box-text">LM AICc: </span>
                                                  <span class="info-box-number">
                                                    <div id="showLmAic" class="shiny-text-output" style="font-weight: bolder; font-size: larger"></div>
                                                  </span>
                                                  </div>'
                                                                )),
                                                         column(4,
                                                                HTML(
                                                                  '<div class="info-box-content" style="font-weight: lighter; font-size: smaller">
                                                  <span class="info-box-text">Bandwidth: </span>
                                                  <span class="info-box-number">
                                                    <div id="showGwrBw" class="shiny-text-output" style="font-weight: bolder; font-size: larger"></div>
                                                  </span>
                                                  </div>'
                                                                ),
                                                                HTML(
                                                                  '<div class="info-box-content" style="font-weight: lighter; font-size: smaller">
                                                  <span class="info-box-text">Data Points: </span>
                                                  <span class="info-box-number">
                                                    <div id="showGwrDp" class="shiny-text-output" style="font-weight: bolder; font-size: larger"></div>
                                                  </span>
                                                  </div>'
                                                                ))
                                                  )
                                                ),
                                                conditionalPanel(condition="input.GwrShowSummary==1",
                                                                 verbatimTextOutput(outputId="GwrSummary")
                                                )
                                                
                                      )
                                      ))),
               
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    ),

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
