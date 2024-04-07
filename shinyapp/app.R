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

# -----Load data files
#load("data/ladbbox.rda")

# Define UI for application that draws a histogram
# Define varGwrLod
varGwrLod <- c(
  "LAD"="LAD",
  "Ward"="Ward",
  "MSOA"="MSOA"
)

depVariables = c(
  "price"="price",
  "minimum_nights"="minimum_nights",
  "minimum_nights2"="minimum_nights2"
)
expVariables = c("price"="price",
                 "minimum_nights"="minimum_nights",
                 "minimum_nights2"="minimum_nights2")


kernelMethods = c("Gaussian"="Gaussian",
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


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Simple Geo-Spatial Analysis using R and Shiny"),
  
  # -----Navigation Bar
  navbarPage("SGSAS", fluid=TRUE, windowTitle="Simple Geo-Spatial Analysis using R and Shiny ", selected="eda",
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
                                                            selectInput(inputId="DependentVariable",
                                                                        label="Dependent Variable",
                                                                        choices=depVariables,
                                                                        selected="price",
                                                                        multiple=FALSE,
                                                                        width="97%"
                                                            )),
                                                          fluidRow(       
                                                            selectInput(inputId="ExplanatoryVariable",
                                                                        label="Explanatory Variable",
                                                                        choices=expVariables,
                                                                        selected="minimum_nights",
                                                                        multiple=TRUE,
                                                                        width="97%"
                                                            )),
                                                          fluidRow(       
                                                            selectInput(inputId="KernelMethod",
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
                                                                          value=FALSE,
                                                                          width="100%"
                                                            )
                                                          )
                                                          ))),
                                    mainPanel(width=9, fluid=TRUE,
                                              fluidRow(
                                                # column(6,
                                                #        leafletOutput("gwr1"),
                                                #        column(6,
                                                #               selectInput(inputId="Gwr1Reference",
                                                #                           label="Reference Value",
                                                #                           choices=c("Local R2"="Local_R2"
                                                #                           ),
                                                #                           selected=NULL,
                                                #                           multiple=FALSE,
                                                #                           width="100%"
                                                #               )),
                                                #        column(6,
                                                #               selectInput(inputId="Gwr1Binning",
                                                #                           label="Binning Method",
                                                #                           choices=c("Std Deviation"="sd",
                                                #                                     "Equal"="equal",
                                                #                                     "Pretty"="pretty",
                                                #                                     "Quantile"="quantile",
                                                #                                     "K-means Cluster"="kmeans",
                                                #                                     "Hierarchical Cluster"="hclust",
                                                #                                     "Bagged Cluster"="bclust",
                                                #                                     "Fisher"="fisher",
                                                #                                     "Jenks"="jenks",
                                                #                                     "Log10 Pretty"="log10_pretty"
                                                #                           ),
                                                #                           selected="quantile",
                                                #                           multiple=FALSE,
                                                #                           width="100%"
                                                #               )),
                                                #        sliderInput(inputId="Gwr1N",
                                                #                    label="Select number of classes",
                                                #                    min=2,
                                                #                    max=30,
                                                #                    value=5,
                                                #                    width="100%"
                                                #        )
                                                # ),
                                                
                                                # {if (TRUE) {column(6,
                                                #        leafletOutput("gwr9")),
                                                # 
                                                # column(6,
                                                #        leafletOutput("gwr8"))} else {column(12,
                                                #                                           leafletOutput("gwr9"))}},
                                                
                                                column(6,
                                                       leafletOutput("gwr9")),
                                                
                                                column(6,
                                                       leafletOutput("gwr8")),
                                                
                                                
                                                
                                              ),
                                              conditionalPanel(condition="input.GwrShowSummary==1",
                                                               verbatimTextOutput(outputId="GwrSummary")
                                              )
                                              
                                    )
                      
                      )),
             
             #END OF GWR PART1
             
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
             )
             
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
  rv$variableSelect <- input$GwrX
  updateSelectInput(session, inputId="Gwr1Reference",
                    label="Reference Value",
                    choices=c("Local R2"="Local_R2",rv$variableSelect)
  )
})


output$gwr1 <- renderLeaflet({
  
  
  if (input$GwrLod=="LAD") {
    GwrDataSp <- maplad_sp
    GwrDataSf <- maplad_sf
    if (input$Gwr1Reference=="Local_R2"){
      Gwr1Title <- "Local R2"
    }
    else {
      Gwr1Title <- "Coefficients"
    }
  }
  else if (input$GwrLod=="Ward") {
    if (input$GwrY=="estimated_diabetes_prevalence") {
      GwrDataSp <- mapward_sp[mapward_sp@data$estimated_diabetes_prevalence!=0,]
      GwrDataSf <- mapward_sf[mapward_sf$estimated_diabetes_prevalence!=0,]
    }
    else {
      GwrDataSp <- mapward_sp
      GwrDataSf <- mapward_sf
    }
    if (input$Gwr1Reference=="Local_R2"){
      Gwr1Title <- "Local R2"
    }
    else {
      Gwr1Title <- "Coefficients"
    }
  }
  else {
    GwrDataSp <- mapmsoa_sp
    GwrDataSf <- mapmsoa_sf
    if (input$Gwr1Reference=="Local_R2"){
      Gwr1Title <- "Local R2"
    }
    else {
      Gwr1Title <- "Coefficients"
    }
  }
  
  if (input$GwrLad=="All"){
    rv$subsetGwrView <- maprgn_sf[,"area_nm"]
  }
  else {
    rv$subsetGwrView <- maplad_sf[maplad_sf$area_nm==input$GwrLad,"area_nm"]
  }
  
  
  GwrFormula <- as.formula(paste(input$GwrY,paste(input$GwrX, collapse="+"), sep="~"))
  if (input$GwrAutoBandwidth==1) {
    GwrBw <- bw.gwr(GwrFormula, data=GwrDataSp, approach=input$GwrApproach, kernel=input$GwrKernel, adaptive=input$GwrBandwidth, p=input$GwrDistance, longlat=FALSE)
  }
  else {
    GwrBw <- input$ManualBandwidth
  }
  
  rv$Gwr <- gwr.basic(GwrFormula, data=GwrDataSp, bw=GwrBw, kernel=input$GwrKernel, adaptive=input$GwrBandwidth, p=input$GwrDistance, longlat=FALSE, cv=TRUE)
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
  for (dim_ in rv$variableSelect) {
    # GwrSDF[, paste0(dim_, "_PV")] <- pt(abs(GwrSDF[, paste0(dim_, "_TV")]),df=length(GwrSDF)-1,lower.tail=FALSE)*2
    GwrSDF[, paste0(dim_, "_PV")] <- pt(abs(GwrSDF[, paste0(dim_, "_TV")]),df=rv$GwrDiagnostic$enp,lower.tail=FALSE)*2
  }
  
  rv$GwrResult <- GwrDataSf %>%
    select(area_id,area_nm,lad_id,lad_nm,geometry) %>%
    cbind(., as.matrix(GwrSDF))
  
  gwr1Plot <- tm_shape(rv$GwrResult) +
    tm_fill(input$Gwr1Reference,
            title=Gwr1Title,
            style=input$Gwr1Binning,
            n=input$Gwr1N,
            breaks=c(0,0.001,0.01,0.05,0.1,1),
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
  tmap_leaflet(gwr1Plot, in.shiny=TRUE)
  
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
    
    airbnb_resale.sf.adaptive2 = read_rds("/Users/maarunipandithurai/Documents/Documents - Maaruni’s MacBook Pro/maars202/geospatial/gaproject/shinyapp/data/airbnb_resale.sf.adaptive.rds")
    mpsz_svy21_3 = read_rds("/Users/maarunipandithurai/Documents/Documents - Maaruni’s MacBook Pro/maars202/geospatial/gaproject/shinyapp/data/mpsz_svy21")
    gwr2Plot <- tm_shape(mpsz_svy21_3)+
      tm_polygons(alpha = 0.1) +
      tm_shape(airbnb_resale.sf.adaptive2) +  
      tm_dots(col = "Local_R2",
              border.col = "gray60",
              border.lwd = 1) +
      tm_view(set.zoom.limits = c(11,14))
    
  }
  
  tmap_leaflet(gwr2Plot, in.shiny=TRUE)
  
  
})

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
  rv$Gwr
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

airbnb_resale.sf.adaptive2 = read_rds("/Users/maarunipandithurai/Documents/Documents - Maaruni’s MacBook Pro/maars202/geospatial/gaproject/shinyapp/data/airbnb_resale.sf.adaptive.rds")
mpsz_svy21_3 = read_rds("/Users/maarunipandithurai/Documents/Documents - Maaruni’s MacBook Pro/maars202/geospatial/gaproject/shinyapp/data/mpsz_svy21")
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
  gwr1Plot = number_of_reviews_SE
  gwr2Plot = number_of_reviews_TV
  # gwr2Plot = tmap_arrange(number_of_reviews_SE, number_of_reviews_TV,  asp=1, ncol=2, sync = TRUE)
}

output$gwr9 <- renderLeaflet({
  tmap_options(check.and.fix = TRUE)
  tmap_leaflet(gwr2Plot, in.shiny=TRUE)})

if (!is.null(gwr1Plot)){
output$gwr8 <- renderLeaflet({
  tmap_options(check.and.fix = TRUE)
  tmap_leaflet(gwr1Plot, in.shiny=TRUE)})
}

# END OF GWR PART2



}


# Run the application 
shinyApp(ui = ui, server = server)


  