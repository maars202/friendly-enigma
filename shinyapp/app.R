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

library(shinycssloaders)

# -----Load data files
# airbnb <- st_read("data/geospatial", layer = "airbnb")
airbnb <- st_read("data", layer = "airbnb")

# Define UI for application that draws a histogram
# Define varGwrLod
varGwrLod <- c(
  "LAD"="LAD",
  "Ward"="Ward",
  "MSOA"="MSOA"
)

depVariables = c(
  "price"="price"
)
expVariables = c(
                 "minimum_nights"="minimum_nights",
                 "number_of_reviews" = "number_of_reviews",
                 "reviews_per_month" = "reviews_per_month", 
                 "calculated_host_listings_count" = "calculated_host_listings_count",
                 "availability_365" = "availability_365",
                 "number_of_reviews_ltm" = "number_of_reviews_ltm")


kernelMethods = c("Gaussian"="gaussian",
                  "Exponential"="exponential",
                  "Bisquare"="bisquare",
                  "Tricube"="tricube",
                  "Boxcar"="boxcar")


# GWR Approach
varGwrApproach <- c(
  "CV"="CV",
  "AIC"="AIC"
)

# GWR Bandwidth
varGwrBandwidth <- c(
  "Fixed"=FALSE,
  "Adaptive"=TRUE
)

# GWR AutoBandwidth
varGwrAutoBandwidth <- c(
  "Manual"=FALSE,
  "Auto"=TRUE
)

# GWR Distance
varGwrDistance <- c(
  "Euclidean"=2,
  "Manhattan"=1
)
airbnb <- airbnb |> rename(mean_price = men_prc, median_price = mdn_prc, max_price = max_prc, min_price = min_prc)
varprice <- c("mean_price", "median_price", "max_price", "min_price")


# Define UI for application that draws a histogram
ui <- fluidPage(
  # theme = shinytheme("united"),
  theme = shinytheme("superhero"),
  
  # Application title
  titlePanel("Simple Geo-Spatial Analysis using R and Shiny"),
  
  # -----Navigation Bar
  navbarPage("SGSAS", fluid=TRUE, windowTitle="Simple Geo-Spatial Analysis using R and Shiny ", selected="user_guide",
             tabPanel("User Guide", value="user_guide", fluid=TRUE,
                      
                      tags$h1("Happy Hideouts"),
                      tags$p("Welcome to our app, Happy Hideouts! We give the awesome opportunity to find out more about the Airbnb listings in the Singapore market which is much different from the rest of the world! Please use this user guide to guide your journey across our app."),
                      
                      tags$h2("Data Analytics"),
                      uiOutput("user_guide_data_analytics"),
                      tags$p("Consist of the exploratory data analysis of all the possible AirBnB datasets. What are the Datasets that we are using?\n"),
                      tags$ul(
                        tags$li("Listings - Summary information on listings"),
                        tags$li("Detailed Listings - Detailed listing information of airbnb for rent"),
                        tags$li("Calendar - Detailed calendar data for listings"),
                        tags$li("Reviews - Summary review data"),
                        tags$li("Detailed Reviews - Detailed review data for listings"),
                        tags$li("Neighbourhoods - list of neighbourhoods in the city and a neighbourhood GeoJSON file"),
                      ),
                      
                      tags$h4("Available Charts to explore on Shiny App:"),
                      tags$ul(
                        tags$li("Room types by region"),
                        tags$li("Pricing of Room Types"),
                        tags$li("Room Types by Price and Region"),
                        tags$li("Host Per year"),
                        tags$li("Listings with no review"),
                        tags$li("Review score by room type"),
                      ),
                      tags$h2("Second Order Analysis: K Function test by sub regions"),
                      uiOutput("user_guide_second_order_analysis"),
                      tags$ul(
                        tags$li("Consists of the K functions results according to the regions that are selected and compares it against the different room types available in the region."),
                        tags$li("Included insights and possible findings under the charts"),
                      ),
                      tags$h4("Regions available:"),
                      tags$ul(
                        tags$li("East Region"),
                        tags$li("West Region"),
                        tags$li("North Region"),
                        tags$li("North East Region"),
                        tags$li("Central Region"),
                      ),
                      tags$h5("Room types available for comparison of each region selected:"),
                      tags$ul(
                        tags$li("Hotel room"),
                        tags$li("Entire home/ apartment"),
                        tags$li("Private room"),
                        tags$li("Shared room"),
                      ),
                      tags$p("Overall this discusses a spatial analysis approach to studying the clustering and competition among different types of listings (hotel rooms, entire homes, private rooms, and shared rooms) within a geographic study area. The analysis is performed using Ripley's K-function, which measures the number of events found within a certain distance of any particular event in a point pattern. This method helps to assess whether the point processes exhibit Complete Spatial Randomness (CSR) or whether there are signs of clustering or competition. The analysis uses the Kest() function from the spatstat package in R to compute the K-function and perform simulations to test the null hypothesis of CSR. The results are compared to theoretical values, and if the observed values fall outside the bounds of the theoretical values, it implies either clustering (above the upper bound) or competition (below the lower bound). The study examines different regions of Singapore (East, West, North, Northeast, and Central) and the four types of listings. The findings indicate varying degrees of clustering across regions and room types:"),
                      tags$ul(
                        tags$li("East Region: Observed clustering for entire homes and private rooms, but not for hotel or shared rooms due to limited data."),
                        tags$li("West Region: Shared rooms exhibit spatial randomness, while private rooms and entire homes show clustering."),
                        tags$li("North Region: Entire homes cluster over 0.2km, and shared rooms cluster under 0.3km. Private rooms also show clustering."),
                        tags$li("Northeast Region: Clustering observed for entire homes and private rooms, but limited data for hotel and shared rooms."),
                        tags$li("Central Region: The largest concentration of all room types, with evidence of clustering across all categories."),
                      ),
                      tags$p("In summary, the study uses Ripley's K-function to assess spatial patterns of different types of listings in various regions of Singapore. Clustering is observed in most room types across different regions, with some exceptions due to data limitations."),
                      tags$h2("Second-Order Analysis: K-Test using Fast Fourier Transform"),
                      uiOutput("user_guide_second_order_fast_fourier"),
                      tags$p("This section describes the use of the Fast Fourier Transform (FFT) technique to perform a K-test analysis on large point patterns using the Kest.fft() function from the spatstat package in R. This method is an alternative to the traditional Kest() function and is more efficient for large datasets, as it discretizes the point pattern onto a rectangular raster and applies FFT to estimate the K-function."),
                      tags$p("Key findings from the analysis include:"),
                      tags$ul(
                        tags$li("Efficiency: The Kest.fft() function is computationally faster, allowing for more simulations (e.g., 300) in a shorter amount of time (e.g., 44 seconds) compared to the minutes or hours it would take using Kest()."),
                        tags$li("Setup: The data is set up as a list of ppp objects (point patterns) covering all of Singapore, split into different room types. The function uses a Gaussian smoothing kernel with a standard deviation (sigma) estimated using the bw.diggle() function."),
                        tags$li("Results/ Selection on Shiny app: The K-test using FFT reveals the following patterns for different types of rooms across Singapore:",
                                tags$ul(
                                  tags$li("Hotel Rooms: Show signs of clustering at around 0.25 km, consistent with the concentration of hotels in the Central region."),
                                  tags$li("Shared Rooms: Exhibit clustering across Singapore at a wider radius of 0.6 km, matching the distribution of shared rooms primarily in the Central region."),
                                  tags$li("Private Rooms: Display signs of clustering at distances of 0.4 km and above."),
                                  tags$li("Entire Homes/Apartments: Indicate clustering at distances of 0.3 km and up, consistent with previous observations."),
                                ),),
                      ),
                      tags$p("In summary, the Kest.fft() function provides an efficient and insightful way to analyze large point patterns, revealing clustering trends across different types of room listings in Singapore at varying distances."),
                        
                      
                      tags$h2("Kernel Density Estimation"),
                      uiOutput("user_guide_kde"),
                      tags$p("The section describes the application of Kernel Density Estimation (KDE) to different room types in Singapore, aiming to identify neighbourhoods with high densities of listings. The analysis uses the bandwidth from previous K-tests to plot the density estimates for each room type."),
                      tags$p("Key findings from the analysis include:"),
                      tags$ul(
                        tags$li("Private Rooms: Private rooms are primarily clustered in the Lavender area, with density spreading to nearby areas such as Kallang Bahru, Balestier, and Little India. Other clusters are found in Chinatown and surrounding areas, River Valley (including Leonie Hill and Oxley), and Aljunied. These areas are on the city fringe or close to the Central Business District and tend to have a good mix of rental and owner-occupied houses."),
                        tags$li("Entire Homes/Apartments: The main clusters of entire homes and apartments are found in Balestier, with smaller densities in Lavender, Bendemeer, and Farrer Park. There is a high concentration in Chinatown, spreading to nearby areas like Anson and Raffles Place. The area around Orchard Road also has a high density of entire home listings, reflecting the popularity of these areas for investment or rental properties. Aljunied also has two distinct clusters of entire homes: one close to the city center and another near the MRT station."),
                        tags$li("Shared Rooms: Shared rooms are concentrated in Lavender, spreading to nearby areas, although the overall density is lower than that of private rooms or entire homes/apartments. A small cluster of shared rooms is also present in the Chinatown area."),
                        tags$li("Hotel Rooms: Hotel rooms are concentrated in the Chinatown and Boat Quay/Clarke Quay areas, which house smaller boutique hotels and low- to mid-tier hotels catering to business districts and tourists. The density of hotel rooms is comparable to that of private rooms."),
                      ),
                      tags$p("In summary, the KDE analysis reveals distinct spatial patterns for different types of room listings across Singapore, with private rooms, entire homes/apartments, shared rooms, and hotel rooms clustering in specific areas based on various factors such as proximity to the city center, public transport networks, and popular investment or rental properties.
"),
                      
                      tags$h2("Geographically Weighted Regression (GWR)"),
                      tags$h4("GWR Webpage:"),
                      uiOutput("user_guide_gwr"),
                      
                      tags$h4("GWR Responsive Loading Page when recalculation is triggered:"),
                      uiOutput("user_guide_gwr2"),
                      
                      tags$p("The GWR is designed to provide you with accurate price estimates for various regions based on different independent variables such as minimum nights spent at listing. You can toggle between different independent variables such as minimum_nights to customize your price estimate. You can also choose other controls such as the type of distribution you would like, including Gaussian, Exponential, Tricube and more. You can also choose between adaptive bandwidth vs fixed bandwidth, although we have found that adaptive bandwidth provides better estimates based on geographical area.Alongside the price estimate, the app displays the Local R2 value. This value indicates how much of the variance in price is accounted for by the model for the specific estimate and region combination. A higher Local R2 value signifies a better fit of the model to the data. Good luck experimenting to get the best estimate for your needs!"),
                      tags$h4("Summary Chart:"),
                      tags$p("For users interested in a deeper understanding of how the model works, we have also provided a mini summary chart at the bottom of the app. This chart offers insights into how the model works beneath the surface, allowing for a more scientific understanding of the price estimation process."),
                      
             ),
             
             #START OF GWR PART1
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
                                                            )),
                                                          fluidRow(       
                                                            selectInput(inputId="GwrY",
                                                                        label="GwrY",
                                                                        choices=depVariables,
                                                                        selected="price",
                                                                        multiple=FALSE,
                                                                        width="97%"
                                                            )),
                                                          fluidRow(       
                                                            selectInput(inputId="GwrX",
                                                                        label="GwrX",
                                                                        choices=expVariables,
                                                                        selected=c("minimum_nights", 
                                                                                   "number_of_reviews", 
                                                                                   "reviews_per_month", 
                                                                                   "calculated_host_listings_count", 
                                                                                   "availability_365",
                                                                                   "number_of_reviews_ltm"),
                                                                        multiple=TRUE,
                                                                        width="97%"
                                                            )),
                                                          fluidRow(       
                                                            selectInput(inputId="GwrKernel",
                                                                        label="Kernel Method",
                                                                        choices=kernelMethods,
                                                                        selected="Gaussian",
                                                                        multiple=FALSE,
                                                                        width="97%"
                                                            )),
                                                          fluidRow(
                                                            column(5,
                                                                   radioButtons(inputId="GwrApproach",
                                                                                label="Approach",
                                                                                choices=varGwrApproach,
                                                                                selected="CV",
                                                                                inline=FALSE,
                                                                                width="100%"
                                                                   ),
                                                                   checkboxInput(inputId="GwrBandwidth",
                                                                                 label="Adaptive",
                                                                                 value=TRUE,
                                                                                 width="100%"
                                                                   )
                                                            ),
                                                            column(7,
                                                                   radioButtons(inputId="GwrDistance",
                                                                                label="Distance",
                                                                                choices=varGwrDistance,
                                                                                selected=2,
                                                                                inline=FALSE,
                                                                                width="100%"
                                                                   ),
                                                                   checkboxInput(inputId="GwrAutoBandwidth",
                                                                                 label="Auto Bandwidth",
                                                                                 value=TRUE,
                                                                                 width="100%"
                                                                   ))),
                                                          
                                                          fluidRow(
                                                            conditionalPanel(condition="input.GwrAutoBandwidth==0",
                                                                             numericInput(inputId="ManualBandwidth",
                                                                                          label="Specify Bandwidth",
                                                                                          min=5,
                                                                                          max=9999,
                                                                                          value=20,
                                                                                          width="75px"
                                                                             )
                                                            ),
                                                            checkboxInput(inputId="GwrShowSummary",
                                                                          label="Summary",
                                                                          value=TRUE,
                                                                          width="100%"
                                                            )
                                                          )
                                                          ))),
                                    mainPanel(width=9, fluid=TRUE,
                                              fluidRow(
                                              
                                                column(6,
                                                       leafletOutput("gwr9") %>% withSpinner(color="#0dc5c1")),
                                                
                                                column(6,
                                                       leafletOutput("gwr8") %>% withSpinner(color="#FF7F50")),
                                                
                                                
                                                
                                              ),
                                              conditionalPanel(condition="input.GwrShowSummary==1",
                                                               verbatimTextOutput(outputId="GwrSummary2") %>% withSpinner(color="#BF40BF")
                                              )
                                              
                                    )
                      
                      )),
             
             #END OF GWR PART1
             # 

             
             
             
             tabPanel("Data Analytics", value="data_analytics", fluid=TRUE,
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("analytics_option", "Choose Analytics Option",
                                      choices = c("Room types by region",
                                                  "Pricing of Room Types",
                                                  "Room Types by Price and Region",
                                                  "Host per year",
                                                  "Listing with no review",
                                                  "Review score by room type"))
                        ),
                        mainPanel(
                          uiOutput("analytics_visualization"),
                        )
                      )
             ),
             tabPanel("Second order analysis: K-Function test by sub regions", value="k_function", fluid=TRUE,
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("Region", "Region", choices = c("East Region", "West Region", "North Region", "NorthEast Region", "Central Region"))
                        ),
                        mainPanel(
                          uiOutput("k_function_image"),
                          textOutput("k_function_result")
                        )
                      )
             ),
             
             tabPanel("Second order analysis: K-Test using Fast Fourier transform", value="k_test_fft", fluid=TRUE,
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("RoomType", "Room Type", choices = c("Hotel Rooms", "Shared Rooms", "Private Rooms", "Entire Home Apartments"))
                        ),
                        mainPanel(
                          uiOutput("k_test_fft_image"),
                          textOutput("k_test_fft_result")
                        )
                      )
             ),
             
             tabPanel("Kernel Density Estimation", value="kde", fluid=TRUE,
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("RoomType_kde", "Room Type", choices = c("Private Rooms", "Entire Homes/Apartments", "Shared Rooms", "Hotel Rooms"))
                        ),
                        mainPanel(
                          uiOutput("kde_image"),
                          textOutput("kde_result")
                        )
                      )
             ),
             tabPanel("ESDA", value="esda", fluid=TRUE, icon=icon("globe-americas"),
                              sidebarLayout(position="left", fluid=TRUE,
                                sidebarPanel(width=3, fluid=TRUE,
                                  selectInput(inputId="inprice", label="Price Measure", choices=varprice, selected="mean_price", multiple=FALSE,width="100%")),
                                mainPanel(width = 9, 
                                  fluidRow(
                                    column(6, plotOutput("lisa"),
                                      column(6, selectInput(inputId="inLisaMethod", label="Analysis Method", choices=c("Contiguity Queen"="q", "Contiguity Rook"="r"), selected="q", multiple=FALSE, width="100%")),
                                    column(6, selectInput(inputId="inLisaSignificance", label="Confidence Level", choices=c("90%"=0.1, "95%"=0.05, "99%"=0.01, "99.9%"=0.001), selected=0.05, multiple=FALSE, width="100%"))),
                                    column(6, plotOutput("reference"),
                                      column(6, selectInput(inputId="inReference", label="Reference Value", choices=c("Price"="r", "Local Moran's I"="i", "P-Value"="p"), selected="p", multiple=FALSE, width="100%")), 
                                      column(6, conditionalPanel(condition="input.inReference!='p'", selectInput(inputId="inBinning", label="Binning Method", choices=c("Std Deviation"="sd", "Equal"="equal", "Pretty"="pretty", "Quantile"="quantile", "K-means Cluster"="kmeans", "Hierarchical Cluster"="hclust","Bagged Cluster"="bclust","Fisher"="fisher","Jenks"="jenks", "Log10 Pretty"="log10_pretty"), selected="quantile", multiple=FALSE, width="100%"))),
                                      conditionalPanel(condition="input.inReference!='p'", sliderInput(inputId="inN", label="Select number of classes", min=2, max=10, value=5, width="100%")))))))
             
  )
)

server <- function(input, output, session) {
  
  # -----All Global functions, variables here
  rv <- reactiveValues()
  
  # Define varGwrLod
  varGwrLod <- c(
    "LAD"="LAD",
    "Ward"="Ward",
    "MSOA"="MSOA"
  )
  
  output$user_guide_gwr <- renderUI({
    tags$img(src = "GWR_preview.png", height=400, width=700)
  })
  
  output$user_guide_gwr2 <- renderUI({
    tags$img(src = "GWR_loading_page.png", height=400, width=700)
  })
  
  output$user_guide_data_analytics <- renderUI({
    tags$img(src = "room_types_region.png", height=400, width=700)
  })
  
  output$user_guide_second_order_analysis <- renderUI({
    tags$img(src = "second_east.png", height=400, width=700)
  }) 
  
  output$user_guide_second_order_fast_fourier <- renderUI({
    tags$img(src = "hotel.png", height=400, width=700)
  })
  
  user_guide_kernel_density_estimation <- renderUI({
    tags$img(src = "kde_private_preview.png", height=400, width=700)
  })
  
  output$user_guide_kde <- renderUI({
    tags$img(src = "kde_user_guide2.png", height=400, width=700)
  })
  
  
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
  
  output$analytics_visualization <- renderUI({
    analytics_option <- input$analytics_option
    
    if (analytics_option == "Room types by region") {
      img_path <- "room_types_region.png"
    } else if (analytics_option == "Pricing of Room Types") {
      img_path <- "pricing_room_types.png"
    } else if (analytics_option == "Room Types by Price and Region") {
      img_path <- "room_types_price_region.png"
    } else if (analytics_option == "Host per year") {
      img_path <- "host_per_year.png"
    } else if (analytics_option == "Listing with no review") {
      img_path <- "listing_no_review.png"
    } else if (analytics_option == "Review score by room type") {
      img_path <- "review_score_room_type.png"
    }
    
    tags$img(src = img_path, height=400, width=600)
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
      img_path <- "entire_home.png"
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




#START OF GWR PART2


# -----GWR functions

observe({
  rv$variableSelect2 <- input$ExplanatoryVariable
  updateSelectInput(session, inputId="Gwr1Reference",
                    label="Reference Value",
                    choices=c("Local R2"="Local_R2",rv$variableSelect)
  )
})


output$gwr2 <- renderLeaflet({
  
  
  if (input$Gwr1Reference=="Local_R2") {
    gwr2Plot_old <- tm_shape(rv$GwrResult) +
      tm_fill("residual",
              title="Residual",
              style="sd",
              # n=6,
              # breaks=c(-1,-0.999,0,0.999,1),
              palette="RdBu",
              midpoint=0,
              id="area_nm",
              alpha=0.8,
              legend.format=list(digits=3)
      ) +
      tm_borders(alpha=0.8
      ) +
      tm_view(view.legend.position=c("right","top"),
              control.position=c("left","bottom"),
              colorNA="Black"
      ) +
      tmap_options(basemaps=c("Esri.WorldGrayCanvas","Stamen.TonerLite","OpenStreetMap"),
                   basemaps.alpha=c(0.8,0.5,0.7)
      ) +
      tm_shape(rv$subsetGwrView) +
      tm_borders(col="black",
                 lwd=3)
    
    airbnb_resale.sf.adaptive2 = read_rds("./data/airbnb_resale.sf.adaptive.rds")
    mpsz_svy21_3 = read_rds("./data/mpsz_svy21")
    gwr2Plot <- tm_shape(mpsz_svy21_3)+
      tm_polygons(alpha = 0.1) +
      tm_shape(airbnb_resale.sf.adaptive2) +  
      tm_dots(col = "Local_R2",
              border.col = "gray60",
              border.lwd = 1) +
      tm_view(set.zoom.limits = c(11,14))
  }
  else {
    GwrPV <- paste0(input$Gwr1Reference, "_PV")
    gwr2Plot_old <- tm_shape(rv$GwrResult) +
      tm_fill(GwrPV,
              title="P-value",
              style="fixed",
              n=5,
              breaks=c(0,0.001,0.01,0.05,0.1,1),
              palette=colorsNBu,
              midpoint=0,
              id="area_nm",
              alpha=0.8,
              legend.format=list(digits=3)
      ) +
      tm_borders(alpha=0.8
      ) +
      tm_view(view.legend.position=c("right","top"),
              control.position=c("left","bottom"),
              colorNA="Black"
      ) +
      tmap_options(basemaps=c("Esri.WorldGrayCanvas","Stamen.TonerLite","OpenStreetMap"),
                   basemaps.alpha=c(0.8,0.5,0.7)
      ) +
      tm_shape(rv$subsetGwrView) +
      tm_borders(col="black",
                 lwd=3)
    
    
    # insert selections here:
    # formula = price ~ input$ExplanatoryVariable[0]
    # for (dim in input$ExplanatoryVariable){
    #   formula = formula + dim
    # }
    GwrFormula <- as.formula(paste(input$DependentVariable,paste(input$ExplanatoryVariable, collapse="+"), sep="~"))
    
    airbnb_resale.sp = read_rds("data/airbnb_resale.sp")
    GwrBw <- bw.gwr(GwrFormula, data=airbnb_resale.sp, approach=input$GwrApproach, kernel=input$GwrKernel, adaptive=input$GwrBandwidth, p=input$GwrDistance, longlat=FALSE)

    rv$Gwr <- gwr.basic(GwrFormula,
                              data=airbnb_resale.sp, bw=GwrBw,
                              kernel = input$GwrKernel,
                              adaptive=input$GwrBandwidth,
                              longlat = FALSE)
    
    var.n<-length(rv$Gwr$lm$coefficients)
    dp.n<-length(rv$Gwr$lm$residuals)
    rv$GwrDiagnostic <- as.data.frame(rv$Gwr$GW.diagnostic) %>%
      mutate(lm_RSS=sum(rv$Gwr$lm$residuals^2)) %>%
      mutate(lm_AIC=dp.n*log(lm_RSS/dp.n)+dp.n*log(2*pi)+dp.n+2*(var.n + 1)) %>%
      mutate(lm_AICc=dp.n*log(lm_RSS/dp.n)+dp.n*log(2*pi)+dp.n+2*dp.n*(var.n+1)/(dp.n-var.n-2)) %>%
      mutate(lm_R2=summary(rv$Gwr$lm)$r.squared) %>%
      mutate(lm_R2.adj=summary(rv$Gwr$lm)$adj.r.squared) %>%
      mutate(bw=rv$Gwr$GW.arguments$bw) %>%
      mutate(dp.n=dp.n)
    GwrSDF <- as.data.frame(rv$Gwr$SDF)

    
    airbnb_resale.sf.adaptive <- st_as_sf(rv$Gwr$SDF) %>%
      st_transform(crs=3414)

    airbnb_resale.sf.adaptive.svy21 <- st_transform(airbnb_resale.sf.adaptive, 3414)

    GwrSDF <- as.data.frame(rv$Gwr$SDF)
    airbnb_resale.sf.adaptive2 <- cbind(airbnb_resale.res.sf, as.matrix(GwrSDF))

    for (dim_ in rv$variableSelect) {
      # GwrSDF[, paste0(dim_, "_PV")] <- pt(abs(GwrSDF[, paste0(dim_, "_TV")]),df=length(GwrSDF)-1,lower.tail=FALSE)*2
      GwrSDF[, paste0(dim_, "_PV")] <- pt(abs(GwrSDF[, paste0(dim_, "_TV")]),df=rv$GwrDiagnostic$enp,lower.tail=FALSE)*2
    }
    
    # airbnb_resale.sf.adaptive2 = read_rds("/Users/maarunipandithurai/Documents/Documents - Maaruni’s MacBook Pro/maars202/geospatial/gaproject/shinyapp/data/airbnb_resale.sf.adaptive.rds")
    mpsz_svy21_3 = read_rds("data/mpsz_svy21")
    gwr2Plot <- tm_shape(mpsz_svy21_3)+
      tm_polygons(alpha = 0.1) +
      tm_shape(GwrSDF) +  
      tm_dots(col = "y",
              border.col = "gray60",
              border.lwd = 1) +
      tm_view(set.zoom.limits = c(11,14))
    
  }
  
  tmap_leaflet(gwr2Plot, in.shiny=TRUE)
  
  
})

h3(textOutput("caption"))

output$showGwrR2 <- renderText ({
  as.character(round(rv$GwrDiagnostic$gwR2.adj,digits=7))
})

output$showGwrAic <- renderText ({
  as.character(round(rv$GwrDiagnostic$AICc,digits=4))
})

output$showLmR2 <- renderText ({
  as.character(round(rv$GwrDiagnostic$lm_R2.adj,digits=7))
})

output$showLmAic <- renderText ({
  as.character(round(rv$GwrDiagnostic$lm_AICc,digits=4))
})

output$showGwrBw <- renderText ({
  if (input$GwrBandwidth==1) {
    paste0(as.character(round(rv$GwrDiagnostic$bw,digits=0)), " neighbours")
  }
  else {
    paste0(as.character(round(rv$GwrDiagnostic$bw,digits=0)), " metres")
  }
})

output$showGwrDp <- renderText ({
  as.character(rv$GwrDiagnostic$dp.n)
})

output$showGwrDp <- renderText ({
  "hellooooo" + getwd(".")
})

output$GwrSummary <- renderPrint({
  rv$GwrAdapt
})




observe({
  input$GwrLod
  input$GwrY
  input$GwrX
  input$GwrModel
  input$GwrDistance
  input$GwrBandwidth
  input$GwrKernel
  input$GwrApproach
  input$GwrAutoBandwidth
  input$GwrManualBandwidth
  input$Gwr1Reference
  input$Gwr1Binning
  input$Gwr1N
  input$DependentVariable
  input$ExplanatoryVariable
  #coordsGwr <- ladbbox[ladbbox$area_nm==input$GwrLad,c("xmin","ymin","xmax","ymax")]
  #if (!is.null(coordsGwr)) {
    leafletProxy("gwr1") %>%
      #fitBounds(coordsGwr$xmin, coordsGwr$ymin, coordsGwr$xmax, coordsGwr$ymax)
      fitBounds(2667.538, 15748.721, 56396.440, 50256.334 )
    leafletProxy("gwr2") %>%
      #fitBounds(coordsGwr$xmin, coordsGwr$ymin, coordsGwr$xmax, coordsGwr$ymax)
      fitBounds(2667.538, 15748.721, 56396.440, 50256.334 )
  #}
}, priority=2)

observe({
  coordsGwr2 <- input$gwr1_bounds
  #if (!is.null(coordsGwr2)) {
    leafletProxy("gwr2") %>%
      #fitBounds(coordsGwr2$west, coordsGwr2$south, coordsGwr2$east, coordsGwr2$north)
      fitBounds(2667.538, 15748.721, 56396.440, 50256.334 )
  #}
}, priority=1)



#start of example 
points <- eventReactive(input$recalc, {
  cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
}, ignoreNULL = FALSE)

output$gwr7 <- renderLeaflet({
  leaflet() %>%
    addProviderTiles(providers$Stadia.StamenTonerLite,
                     options = providerTileOptions(noWrap = TRUE)
    ) %>%
    addMarkers(data = points())
})
#end of example 

observe({
  rv$variableSelect2 <- input$ExplanatoryVariable
  updateSelectInput(session, inputId="Gwr1Reference",
                    label="Reference Value",
                    choices=c("Local R2"="Local_R2",rv$variableSelect2)
  )
  
})



airbnb_resale.sf.adaptive2 = read_rds("data/airbnb_resale.sf.adaptive.rds")
mpsz_svy21_3 = read_rds("data/mpsz_svy21")
coefficientEstimatesPlot = TRUE

gwr1Plot = NULL

if (coefficientEstimatesPlot == FALSE){gwr2Plot <- tm_shape(mpsz_svy21_3)+
  tm_polygons(alpha = 0.1) +
  tm_shape(airbnb_resale.sf.adaptive2) +  
  tm_dots(col = "Local_R2",
          border.col = "gray60",
          border.lwd = 1) +
  tm_view(set.zoom.limits = c(11,14))
}else{
  # sf_use_s2(TRUE)
  tmap_mode("view")
  number_of_reviews_SE <- tm_shape(mpsz_svy21_3)+
    tm_polygons(alpha = 0.1) +
    tm_shape(airbnb_resale.sf.adaptive2) +  
    tm_dots(col = "number_of_reviews_SE",
            border.col = "gray60",
            border.lwd = 1) +
    tm_view(set.zoom.limits = c(11,14))
  
  number_of_reviews_TV <- tm_shape(mpsz_svy21_3)+
    tm_polygons(alpha = 0.1) +
    tm_shape(airbnb_resale.sf.adaptive2) +  
    tm_dots(col = "number_of_reviews_TV",
            size = 0.15,
            border.col = "gray60",
            border.lwd = 1, palette = "YlGn") +
    tm_view(set.zoom.limits = c(11,14)) 
  # gwr1Plot = number_of_reviews_SE
  gwr2Plot = number_of_reviews_TV
  # gwr2Plot = tmap_arrange(number_of_reviews_SE, number_of_reviews_TV,  asp=1, ncol=2, sync = TRUE)
}


output$gwr9 <- renderLeaflet({
  
  GwrFormula <- as.formula(paste(input$GwrY,paste(input$GwrX, collapse="+"), sep="~"))
  airbnb_resale.sp = read_rds("data/airbnb_resale.sp")
  mpsz_svy21_3 = read_rds("data/mpsz_svy21")
  airbnb_resale.res.sf = read_rds("data/airbnb_resale.res.sf")
  GwrBw <- bw.gwr(GwrFormula, data=airbnb_resale.sp, approach=input$GwrApproach, kernel=input$GwrKernel, adaptive=FALSE, longlat=FALSE)
  rv$Gwr <- gwr.basic(GwrFormula,
                      data=airbnb_resale.sp, bw=GwrBw,
                      kernel = input$GwrKernel,
                      adaptive=input$GwrBandwidth,
                      longlat = FALSE)
  # 
  var.n<-length(rv$Gwr$lm$coefficients)
  dp.n<-length(rv$Gwr$lm$residuals)
  rv$GwrDiagnostic <- as.data.frame(rv$Gwr$GW.diagnostic) %>%
    mutate(lm_RSS=sum(rv$Gwr$lm$residuals^2)) %>%
    mutate(lm_AIC=dp.n*log(lm_RSS/dp.n)+dp.n*log(2*pi)+dp.n+2*(var.n + 1)) %>%
    mutate(lm_AICc=dp.n*log(lm_RSS/dp.n)+dp.n*log(2*pi)+dp.n+2*dp.n*(var.n+1)/(dp.n-var.n-2)) %>%
    mutate(lm_R2=summary(rv$Gwr$lm)$r.squared) %>%
    mutate(lm_R2.adj=summary(rv$Gwr$lm)$adj.r.squared) %>%
    mutate(bw=rv$Gwr$GW.arguments$bw) %>%
    mutate(dp.n=dp.n)
  GwrSDF <- as.data.frame(rv$Gwr$SDF)
  # 
  
  airbnb_resale.sf.adaptive <- st_as_sf(rv$Gwr$SDF) %>%
    st_transform(crs=3414)
  
  airbnb_resale.sf.adaptive.svy21 <- st_transform(airbnb_resale.sf.adaptive, 3414)
  
  airbnb_resale.sf.adaptive2 <- cbind(airbnb_resale.res.sf, as.matrix(as.data.frame(rv$Gwr$SDF)))
  
  # write_rds(airbnb_resale.sf.adaptive2, file = "/Users/maarunipandithurai/Documents/Documents - Maaruni’s MacBook Pro/maars202/geospatial/gaproject/shinyapp/data/airbnb_resale.sf.adaptive2")
  # airbnb_resale.sf.adaptive2 = read_rds("/Users/maarunipandithurai/Documents/Documents - Maaruni’s MacBook Pro/maars202/geospatial/gaproject/shinyapp/data/airbnb_resale.sf.adaptive2")
  
  for (dim_ in rv$variableSelect2) {
    # GwrSDF[, paste0(dim_, "_PV")] <- pt(abs(GwrSDF[, paste0(dim_, "_TV")]),df=length(GwrSDF)-1,lower.tail=FALSE)*2
    GwrSDF[, paste0(dim_, "_PV")] <- pt(abs(GwrSDF[, paste0(dim_, "_TV")]),df=rv$GwrDiagnostic$enp,lower.tail=FALSE)*2
  }
  
  
  
  # airbnb_resale.sf.adaptive2 = read_rds("/Users/maarunipandithurai/Documents/Documents - Maaruni’s MacBook Pro/maars202/geospatial/gaproject/shinyapp/data/airbnb_resale.sf.adaptive.rds")
  tmap_mode("view")
  number_of_reviews_SE <- tm_shape(mpsz_svy21_3)+
    tm_polygons(alpha = 0.1) +
    tm_shape(airbnb_resale.sf.adaptive2) +  
    tm_dots(col = "y",
            border.col = "gray60",
            border.lwd = 1) +
    tm_view(set.zoom.limits = c(11,14))
  
  number_of_reviews_TV <- tm_shape(mpsz_svy21_3)+
    tm_polygons(alpha = 0.1) +
    tm_shape(airbnb_resale.sf.adaptive2) +  
    tm_dots(col = "Local_R2",
            size = 0.15,
            border.col = "gray60",
            border.lwd = 1, palette = "YlGn") +
    tm_view(set.zoom.limits = c(11,14)) 
  gwr1Plot = number_of_reviews_SE
  gwr2Plot = number_of_reviews_TV
  
  
  
  
  rv$plot2 = number_of_reviews_SE
  tmap_options(check.and.fix = TRUE)
  tmap_leaflet(gwr2Plot, in.shiny=TRUE)
  
  }
  )


output$gwr8 <- renderLeaflet({
  tmap_options(check.and.fix = TRUE)
  tmap_leaflet( rv$plot2, in.shiny=TRUE)})

output$GwrSummary2 <- renderPrint({
  rv$Gwr
})
# END OF GWR PART2



colors <- c("#ffffff", "#2c7bb6", "#abd9e9", "#fdae61", "#d7191c")
clusters <- c("insignificant", "low-low", "low-high", "high-low", "high-high")
#Lisa Plot
output$lisa <- renderPlot({
    if (input$inLisaMethod=="q") {
      wm <- poly2nb(airbnb, queen=TRUE)
      rswm <- nb2listw(wm, style = "W",  zero.policy=TRUE)
    }
    else {
      wm <- poly2nb(airbnb, queen=FALSE)
      rswm <- nb2listw(wm, style = "W", zero.policy=TRUE)
    }
indicator <- pull(airbnb, input$inprice)
lmoran <- localmoran(indicator, rswm)
airbnb_lm <- cbind(airbnb, lmoran) |>
  rename(Pr.Ii = Pr.z....E.Ii..)

quadrant <- vector(mode= "numeric" , length=nrow(lmoran))
airbnb$lag <- lag.listw(rswm,indicator)
DV <- airbnb$lag - mean(airbnb$lag)
lm_i <- lmoran[,1] - mean(lmoran[,1])
quadrant[DV <0 & lm_i>0] <- 1
quadrant[DV >0 & lm_i<0] <- 2
quadrant[DV <0 & lm_i<0] <- 3  
quadrant[DV >0 & lm_i>0] <- 4  
signif <- as.numeric(input$inLisaSignificance)
quadrant[airbnb_lm$Pr.Ii>signif] <- 0
airbnb_lm$quadrant <- quadrant
airbnb_lm <- st_make_valid(airbnb_lm)
rv$airbnb_lm = airbnb_lm
tm_shape(airbnb_lm) +
  tm_fill(col = "quadrant", 
          style = "cat", 
          palette = colors[c(sort(unique(quadrant)))+1], 
          labels = clusters[c(sort(unique(quadrant)))+1],
          popup.vars = c("")) +
  tm_view(set.zoom.limits = c(11,17)) +
  tm_borders(alpha=0.5) + 
  tmap_options(check.and.fix = TRUE)
}) 
#Price, p-value and moran I stat plot
output$reference <- renderPlot({
     if (input$inReference=="r"){
      tmFill <- input$inprice
      tmTitle <- "Prices"
      tmStyle <- input$inBinning
      tmpalette <- "YlOrRd"
    }
    else if (input$inReference=="i"){
      tmFill <- "Ii"
      tmTitle <- "I-Values"
      tmStyle <- input$inBinning
      tmpalette <-"RdBu"
    }
    else {
      tmFill <- "Pr.Ii"
      tmTitle <- "P-Values"
      tmStyle <- "fixed"
      tmpalette <- "-Blues"
    }
  tm_shape(rv$airbnb_lm) +
      tm_fill(col = tmFill,
              title= tmTitle,
              style=tmStyle,
              n=input$inN,
              breaks=c(0,0.001,0.01,0.05,0.1,1),
              palette= tmpalette,
              midpoint=0,
              popup.vars=c("Prices"=input$inprice, "P_Value:"="Pr.Ii","Local_Moran's_I:"="Ii"),
              alpha=0.8,
              legend.format=list(digits=3)) +
      tm_borders(alpha=0.8) +
      tm_view(view.legend.position=c("right","top"),
              control.position=c("left","bottom"),
              colorNA="Black") 
   
})



}


# Run the application 
shinyApp(ui = ui, server = server)


  
