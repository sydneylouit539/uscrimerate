rm(list=ls())
library(leaflet)
library(leaflet.minicharts)
library(dplyr)
library(RCurl)
library(rgdal)
library(htmltools)
library(shiny)
library(shinydashboard)
setwd("G:/1-2021 spring/r and sas/group assignment")
#import data
crimerate = read.csv('crimerate.csv')
#import shapefile
states.shape = st_read("usa_shapes.shp")
statename=sort(c(state.name,'District of Columbia','Puerto Rico'))

#============R Shiny=========

crime_ui = fluidPage(
  titlePanel("CDC Crime Rate Analytics"),
  sidebarLayout(
    #add control widgets
    sidebarPanel(
      selectInput("selectedoffense","Offense",choices=list("VIOLENTCRIME",
      "MURDER","RAPE","ROBBERY","AGGRAVATEDASSAULT","PROPERTYCRIME",
      "BURGLARY","LARCENYTHEFT","MOTORVEHICLETHEFT"),selected="VIOLENTCRIME"),
      selectInput("selectedstate","state",
      choices = statename,selected = "Alabama"),
      sliderInput("selectedyear","years:",
                  min = 1999,max = 2019,value=2019)),
    mainPanel(
      tabsetPanel(
        tabPanel("map",leafletOutput(outputId = "Dmap")),
        tabPanel("timeseries plot", plotOutput(outputId = "timeseries"))))))

crime_server <- function(input, output) {
  #reactive expressions
  labels<-reactive({#generate in html foramt
    paste("<p>",crimerate[crimerate$YEAR==input$selectedyear,]$STATE,"</p>",
          "<p>","Rate per 100,000 inhabitants: ",crimerate[crimerate$YEAR==input$selectedyear,][,input$selectedoffense],"</p>",sep="")
  })
  #R code to build the output
  output$Dmap <- renderLeaflet({
    #color pallette
    col_year=subset(crimerate,crimerate$YEAR==input$selectedyear)
    col_year_crime_low=floor(min(col_year[[input$selectedoffense]]))
    col_year_crime_high=ceiling(max(col_year[[input$selectedoffense]]))
    bins=c(0,seq(col_year_crime_low,col_year_crime_high,by=round(((col_year_crime_high-col_year_crime_low)/5))),Inf)
    pal<-colorBin("RdYlBu",domain = c(0,1),bins = bins)
    Dmap<-leaflet() %>% 
      addProviderTiles(provider = providers$Stamen.Toner) %>%
      setView(lng = -96,lat = 37.8,zoom = 3.5) %>%
      addPolygons(data = states.shape,
        weight = 1, #boundary thickness
        color = "white", #boundary color
        fillOpacity = 0.5, #opacity of polygons
        fillColor = pal(crimerate[crimerate$YEAR==input$selectedyear,][,input$selectedoffense]),
        label = lapply(labels(),HTML),
        highlightOptions = highlightOptions(
          weight = 5,color = "#666666",
          fillOpacity = 0.7,bringToFront = TRUE)) %>%
      addLegend(pal=pal,
        values = crimerate[crimerate$YEAR==input$selectedyear,]$MURDER,
        opacity = 0.7,position = "topright")
  })
  output$timeseries <- renderPlot({
    crime_data=subset(crimerate,crimerate$STATE==input$selectedstate)
    plot(crime_data[['YEAR']],crime_data[[input$selectedoffense]],xlab="Year",ylab="Crime",type="b")
  })
}
# Run the application 
shinyApp(ui = crime_ui, server = crime_server)