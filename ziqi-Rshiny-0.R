library(leaflet)
library(leaflet.minicharts)
library(dplyr)
library(RCurl)
library(rgdal)
library(htmltools)
library(shiny)
library(shinydashboard)

#import data
crimerate = read.csv('crimerate.csv')

#import shapefile
states.shape<-readOGR("../cb_2018_us_state_500k/cb_2018_us_state_500k.shp")


#color pallet
bins<-c(0,200,250,300,350,400,450,500,Inf)
pal<-colorBin("RdYlBu",domain = c(0,1),bins = bins)


#============R Shiny=========

ui <- fluidPage(
  titlePanel("CDC Crime Rate Analytics"),
  sidebarLayout(
    #add control widgets
    sidebarPanel(
      selectInput("selectedoffense",h4("Offense"),choices=list("VIOLENTCRIME",
                  "MURDER","RAPE","ROBBERY","AGGRAVATEDASSAULT","PROPERTYCRIME",
                  "BURGLARY","LARCENYTHEFT","MOTORVEHICLETHEFT"),selected="VIOLENTCRIME"),
      selectInput("selectedyear",h4("Year"),choices=list(1999,2000,2001,2002,
                  2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,
                  2015,2016,2017,2018,2019),selected=2019),
      width = 2),
      
                 
  mainPanel(
    
    #create a output object
    leafletOutput(outputId = "Dmap")
  )
 ),
)
server <- function(input, output) {
  
  #reactive expressions
  labels<-reactive({#generate in html foramt
    paste("<p>",crimerate[crimerate$YEAR==input$selectedyear,]$STATE,"</p>",
          "<p>","crime rate index: ",crimerate[crimerate$YEAR==input$selectedyear,][,input$selectedoffense],"</p>",sep="")
  })
  
  #R code to build the output
  output$Dmap <- renderLeaflet(
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
                    weight = 5,
                    color = "#666666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE)) %>%
      addLegend(pal=pal,
                values = crimerate[crimerate$YEAR==input$selectedyear,]$MURDER,
                opacity = 0.7,
                position = "topright")
     
  )
}
# Run the application 
shinyApp(ui = ui, server = server)


