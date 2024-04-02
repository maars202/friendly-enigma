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


# Define UI for application that draws a histogram
# Define varGwrLod
varGwrLod <- c(
  "LAD"="LAD",
  "Ward"="Ward",
  "MSOA"="MSOA"
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Simple Geo-Spatial Analysis using R and Shiny"),
  
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
                      )),
             
             tabPanel("Second order analysis: K-Function test by sub regions", value="k_function", fluid=TRUE,
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("Region", "Region", choices = c("East Region", "West Region", "North Region", "NorthEast Region", "Central Region"))
                        ),
                        mainPanel(
                          textOutput("k_function_result"),
                          uiOutput("k_function_image")
                        )
                      )
             ),
             
             tabPanel("Second order analysis: K-Test using Fast Fourier transform", value="k_test_fft", fluid=TRUE,
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("RoomType", "Room Type", choices = c("Hotel Rooms", "Shared Rooms", "Private Rooms", "Entire Home Apartments"))
                        ),
                        mainPanel(
                          textOutput("k_test_fft_result"),
                          uiOutput("k_test_fft_image")
                        )
                      )
             ),
             
             tabPanel("Kernel Density Estimation", value="kde", fluid=TRUE,
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("RoomType_kde", "Room Type", choices = c("Private Rooms", "Entire Homes/Apartments", "Shared Rooms", "Hotel Rooms"))
                        ),
                        mainPanel(
                          textOutput("kde_result"),
                          uiOutput("kde_image")
                        )
                      )
             )
             
  )
)

server <- function(input, output) {
  
  # Define varGwrLod
  varGwrLod <- c(
    "LAD"="LAD",
    "Ward"="Ward",
    "MSOA"="MSOA"
  )
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
  
  output$k_function_result <- renderText({
    region <- input$Region
    
    if(region == "East Region") {
      return("East Region: Limited data for hotel and shared room listings. Observations suggest clustering for entire homes and private rooms.")
    } else if(region == "West Region") {
      return("West Region: Limited data for hotel rooms. Shared rooms exhibit spatial randomness, while private rooms and entire homes show clustering.")
    } else if(region == "North Region") {
      return("North Region: No hotel rooms, few entire homes and shared rooms. Clustering observed for entire homes and private rooms.")
    } else if(region == "NorthEast Region") {
      return("Northeast Region: Limited data for hotel and shared rooms. Clustering observed for entire homes and private rooms.")
    } else if(region == "Central Region") {
      return("Central Region: Highest concentration of all room types, consistent clustering observed.")
    }
  })
  
  output$k_test_fft_result <- renderText({
    room_type <- input$RoomType
    
    if(room_type == "Hotel Rooms") {
      return("Hotel Rooms: Clustering observed around 0.25km, consistent with close proximity in the Central region.")
    } else if(room_type == "Shared Rooms") {
      return("Shared Rooms: Clustering observed at a wider radius of 0.6km, mainly in the Central region.")
    } else if(room_type == "Private Rooms") {
      return("Private Rooms: Clustering observed at 0.4km and above.")
    } else if(room_type == "Entire Home Apartments") {
      return("Entire Home Apartments: Clustering observed from 0.3km and up, consistent with previous observations.")
    }
  })
  
  output$kde_result <- renderText({
    room_type <- input$RoomType_kde
    
    if(room_type == "Private Rooms") {
      return("Private rooms for rent are found in various clusters across Singapore, particularly in the Lavender area, which extends to Kallang Bahru, Balestier, and Little India. Additionally, there is another significant cluster in Chinatown and its surrounding areas, providing proximity to the Central Business District (CBD). A smaller cluster exists in the River Valley area, offering a more secluded atmosphere. Aljunied also hosts a cluster, albeit further from the city center, but still accessible via public transportation. These private room listings are often found in neighborhoods with a mix of rental and owner-occupied houses, contributing to the diverse options available.")
    } else if(room_type == "Entire Homes/Apartments") {
      return("Entire homes and apartments for rent are primarily concentrated in Balestier, with smaller densities observed in Lavender, Bendemeer, and Farrer Park. There is a high concentration of listings in Chinatown, with some spread to Anson and Raffles Place. The Orchard area boasts a higher density of entire homes and apartments compared to private rooms. Aljunied features two distinct clusters—one near the city center and another near the MRT station—providing options for renters. Orchard and Chinatown areas are particularly popular for investment or rental properties due to their attractiveness to tenants and investors alike.")
    } else if(room_type == "Shared Rooms") {
      return("Shared rooms for rent are mainly concentrated in the Lavender area, albeit with lower density compared to private rooms and entire homes. There's also a small concentration of shared rooms in the vicinity of Chinatown, offering alternative accommodation options.")
    } else if(room_type == "Hotel Rooms") {
      return("Hotel rooms, including boutique and mid-tier establishments, are predominantly concentrated in the Chinatown and Boat Quay/Clarke Quay area, catering to both the business district and tourists. The density of hotel room offerings is comparable to that of private rooms, providing a variety of options for travelers and visitors to Singapore.")
    }
  })
  
  output$k_function_image <- renderUI({
    # UI logic to render image based on selected region
    region <- input$Region
    if (region == "East Region") {
      img_path <- "second_east.png"
    } else if (region == "West Region") {
      img_path <- "second_west.png"
    } else if (region == "North Region") {
      img_path <- "second_north.png"
    } else if (region == "NorthEast Region") {
      img_path <- "second_ne.png"
    } else if (region == "Central Region") {
      img_path <- "second_central.png"
    }
    tags$img(src = img_path, height=500, width=500)
  })
  
  output$k_test_fft_image <- renderUI({
    # UI logic to render image based on selected room type
    room_type <- input$RoomType
    if (room_type == "Hotel Rooms") {
      img_path <- "hotel.png"
    } else if (room_type == "Shared Rooms") {
      img_path <- "shared_rooms.png"
    } else if (room_type == "Private Rooms") {
      img_path <- "private_rooms.png"
    } else if (room_type == "Entire Home Apartments") {
      img_path <- "shared_rooms.png"
    }
    tags$img(src = img_path, height=400, width=800)
  })
  
  output$kde_image <- renderUI({
    # UI logic to render image based on selected room type for KDE
    room_type <- input$RoomType_kde
    if (room_type == "Private Rooms") {
      img_path <- "kde_private.jpg"
    } else if (room_type == "Entire Homes/Apartments") {
      img_path <- "kde_entire.png"
    } else if (room_type == "Shared Rooms") {
      img_path <- "kde_shared.jpg"
    } else if (room_type == "Hotel Rooms") {
      img_path <- "kde_hotel.jpg"
    }
    tags$img(src = img_path, height=400, width=600)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


  