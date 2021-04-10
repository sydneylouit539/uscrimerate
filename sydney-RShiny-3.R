rm(list=ls())
library(leaflet)
library(leaflet.minicharts)
library(dplyr)
library(RCurl)
library(rgdal)
library(htmltools)
library(shiny)
library(shinydashboard)
#setwd("G:/1-2021 spring/r and sas/group assignment")
#import dataset
crimerate = read.csv('crimerate.csv')
#import shapefile with WGS84 projection
states.shape = st_transform(st_read("usa_shapes.shp"),'+proj=longlat +datum=WGS84')
#States, with DC and PR
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
      selectInput("selectedstate","Jurisdiction",
      choices = statename,selected = "Alabama"),
      sliderInput("selectedyear","Year:",
                  min = 1999,max = 2019,value=2019),
      sliderInput("selectedrange","Range:",
                  min=1999,max=2019,value=c(1999,2019))),
    mainPanel(
      tabsetPanel(
        tabPanel("Rate map Plot",leafletOutput(outputId = "Dmap")),
        tabPanel("Timeseries Plot", plotOutput(outputId = "timeseries")),
        tabPanel("Change Plot",leafletOutput(outputId = "delta"))))))

crime_server <- function(input, output) {
  #reactive expressions
  labels<-reactive({#generate in html format
    paste("<p>",crimerate[crimerate$YEAR==input$selectedyear,]$STATE,"</p>",
          "<p>","Rate per 100,000 inhabitants: ",crimerate[crimerate$YEAR==input$selectedyear,][,input$selectedoffense],"</p>",sep="")
  })
  change_labels = reactive({
    paste("<p>",crimerate[crimerate$YEAR==input$selectedyear,]$STATE,"</p>",
          "<p>","Change: ",round(100*(crimerate[crimerate$YEAR==input$selectedrange[2],][,input$selectedoffense]/crimerate[crimerate$YEAR==input$selectedrange[1],][,input$selectedoffense]-1),1),"%</p>",sep="")
  })
  #R code to build the output
  output$Dmap <- renderLeaflet({
    #color pallette
    col_year=subset(crimerate,crimerate$YEAR==input$selectedyear)
    col_offense = col_year[[input$selectedoffense]]
    pal=colorRampPalette(c('white','red2'))(100)[cut((col_offense-min(col_offense))/(ceiling(max(col_offense))-floor(min(col_offense))),breaks=seq(0,1,len=100),include.lowest=TRUE)]
    Dmap=leaflet() %>% 
      addProviderTiles(provider = providers$Stamen.Toner) %>%
      setView(lng = -96,lat = 37.8,zoom = 3.5) %>%
      addPolygons(data = states.shape,
        weight = 1, #boundary thickness
        color = "white", #boundary color
        fillOpacity = 0.8, #opacity of polygons
        fillColor = pal,label = lapply(labels(),HTML),
        highlightOptions = highlightOptions(
          weight = 5,color = "#666666",
          fillOpacity = 0.7,bringToFront = TRUE)) %>%
      addLegend(colors=c('red2','white'),
        labels=c('Highest','Lowest'),
        #pal=pal,
        #values = crimerate[crimerate$YEAR==input$selectedyear,]$MURDER,
        opacity = 0.7,position = "topright")
  })
  output$timeseries <- renderPlot({
    crime_data=subset(crimerate,crimerate$STATE==input$selectedstate)
    plot(crime_data[['YEAR']],crime_data[[input$selectedoffense]],xlab="Year",ylab="Crime",type="b")
  })
  output$delta = renderLeaflet({
    col_start=subset(crimerate,crimerate$YEAR==input$selectedrange[1])
    col_end=subset(crimerate,crimerate$YEAR==input$selectedrange[2])
    col_ratechange = col_end[[input$selectedoffense]]/col_start[[input$selectedoffense]]
    #Make ratechange green when decreases, red when increases, on 0-1 scale with no change at 0.5
    rate_scale = log(col_ratechange) 
    rate_scale = rate_scale/(2.001*max(abs(rate_scale)))+0.5
    pal=colorRampPalette(c('green','white','red'))(100)[cut(rate_scale,breaks=seq(0,1,len=100),include.lowest=TRUE)]
    delta=leaflet() %>% 
      addProviderTiles(provider = providers$Stamen.Toner) %>%
      setView(lng = -96,lat = 37.8,zoom = 3.5) %>%
      addPolygons(data = states.shape,
                  weight = 1, #boundary thickness
                  color = "white", #boundary color
                  fillOpacity = 0.8, #opacity of polygons
                  fillColor = pal,label = lapply(change_labels(),HTML),
                  highlightOptions = highlightOptions(
                    weight = 5,color = "#666666",
                    fillOpacity = 0.7,bringToFront = TRUE)) %>%
      addLegend(colors=c('red','white','green'),
                labels=c('Increase','No change','Decrease'),
                opacity = 0.7,position = "topright")
  })
}
# Run the application 
shinyApp(ui = crime_ui, server = crime_server)