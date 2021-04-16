rm(list=ls())
#Libraries
library(leaflet)
library(leaflet.minicharts)
library(dplyr)
library(sf)
library(htmltools)
library(shiny)
library(shinydashboard)
#import dataset
crimerate = read.csv('crimerate.csv')
#import shapefile with WGS84 projection
states.shape = st_transform(st_read("usa_shapes.shp"),'+proj=longlat +datum=WGS84')
#States, with DC and PR
statename=sort(c(state.name,'District of Columbia','Puerto Rico'))

#============R Shiny=========

crime_ui = pageWithSidebar(
  titlePanel("CDC Crime Rate Analytics"),
  sidebarPanel(
    #conditionalPanel() functions for selected tab
    conditionalPanel(condition="input.tabselected==1",
                     selectInput("selectedoffense1","Offense",choices=list("VIOLENTCRIME",
                                                                          "MURDER","RAPE","ROBBERY","AGGRAVATEDASSAULT","PROPERTYCRIME",
                                                                          "BURGLARY","LARCENYTHEFT","MOTORVEHICLETHEFT"),selected="VIOLENTCRIME"),
                     sliderInput("selectedyear","Year:",
                                 min=1999,max=2019,value=2019)),
    conditionalPanel(condition="input.tabselected==2",
                     selectInput("selectedoffense2","Offense",choices=list("VIOLENTCRIME",
                                                                          "MURDER","RAPE","ROBBERY","AGGRAVATEDASSAULT","PROPERTYCRIME",                                   
                                                                          "BURGLARY","LARCENYTHEFT","MOTORVEHICLETHEFT"),selected="VIOLENTCRIME"),
                     selectInput("selectedstate","Jurisdiction",
                                 choices = statename,selected = "Alabama")),
    conditionalPanel(condition="input.tabselected==3",
                     selectInput("selectedoffense3","Offense",choices=list("VIOLENTCRIME",
                                                                          "MURDER","RAPE","ROBBERY","AGGRAVATEDASSAULT","PROPERTYCRIME",
                                                                          "BURGLARY","LARCENYTHEFT","MOTORVEHICLETHEFT"),selected="VIOLENTCRIME"),
                     sliderInput("selectedrange","Range:",
                                 min=1999,max=2019,value=c(1999,2019)))
  ),
  mainPanel( #Set layout for each tab
    tabsetPanel(
      tabPanel("Rate map Plot",value=1,leafletOutput(outputId = "Dmap")),
      tabPanel("Timeseries Plot",value=2, plotOutput(outputId = "timeseries")),
      tabPanel("Change Plot",value=3,leafletOutput(outputId = "delta")),
      id = "tabselected")))

crime_server=function(input, output) {
  #Reactive expressions
  labels=reactive({#Crime rate map labels
    paste("<p>",crimerate[crimerate$YEAR==input$selectedyear,]$STATE,"</p>",
          "<p>","Rate per 100,000 inhabitants: ",crimerate[crimerate$YEAR==input$selectedyear,][,input$selectedoffense1],"</p>",sep="")
  })
  change_labels = reactive({#Change map labels
    paste("<p>",crimerate[crimerate$YEAR==input$selectedyear,]$STATE,"</p>",
          "<p>","Change: ",round(100*(crimerate[crimerate$YEAR==input$selectedrange[2],][,input$selectedoffense3]/crimerate[crimerate$YEAR==input$selectedrange[1],][,input$selectedoffense3]-1),1),"%</p>",sep="")
  })
  #Build crime rate map
  output$Dmap=renderLeaflet({
    #Subset dataset
    col_year=subset(crimerate,crimerate$YEAR==input$selectedyear)
    col_offense=col_year[[input$selectedoffense1]]
    #Set color pallette
    pal=colorRampPalette(c('white','red2'))(100)[cut((col_offense-min(col_offense,na.rm=TRUE))/(ceiling(max(col_offense,na.rm=TRUE))-floor(min(col_offense,na.rm=TRUE))),breaks=seq(0,1,len=100),include.lowest=TRUE)]
    Dmap=leaflet() %>% #Initialize map
      addProviderTiles(provider = providers$Stamen.Toner) %>%
      setView(lng = -96,lat = 37.8,zoom = 3.5) %>%
      addPolygons(data = states.shape, #Next few arguments are customizable options in leaflet
                  weight = 1,color = "white",fillOpacity = 0.8, 
                  fillColor = pal,label = lapply(labels(),HTML),
                  highlightOptions = highlightOptions(
                    weight = 5,color = "#666666",
                    fillOpacity = 0.7,bringToFront = TRUE)) %>%
      addLegend(colors=c('red','white'), #Map Legend
                labels=c('Highest','Lowest'),
                opacity = 0.7,position = "topright")
  })
  #Build Time Series plots
  output$timeseries <- renderPlot({
    crime_data=subset(crimerate,crimerate$STATE==input$selectedstate)
    plot(crime_data[['YEAR']],crime_data[[input$selectedoffense2]],xlab="Year",ylab="Crime",type="b")
  })
  output$delta = renderLeaflet({
    col_start=subset(crimerate,crimerate$YEAR==input$selectedrange[1])
    col_end=subset(crimerate,crimerate$YEAR==input$selectedrange[2])
    col_ratechange = col_end[[input$selectedoffense3]]/col_start[[input$selectedoffense3]]
    #Make ratechange green when decreases, red when increases, on 0-1 scale with no change at 0.5
    rate_scale = log(col_ratechange) 
    rate_scale = rate_scale/(2.001*max(abs(rate_scale),na.rm=TRUE))+0.5
    pal=colorRampPalette(c('green','white','red'))(100)[cut(rate_scale,breaks=seq(0,1,len=100),include.lowest=TRUE)]
    delta=leaflet() %>% #Map
      addProviderTiles(provider = providers$Stamen.Toner) %>%
      setView(lng = -96,lat = 37.8,zoom = 3.5) %>%
      addPolygons(data = states.shape, #Next few arguments are customizable options in leaflet
                  weight = 1,color = "white",fillOpacity = 0.8,
                  fillColor = pal,label = lapply(change_labels(),HTML),
                  highlightOptions = highlightOptions(
                    weight = 5,color = "#666666",
                    fillOpacity = 0.7,bringToFront = TRUE)) %>%
      addLegend(colors=c('red','white','green'), #Map Legend
                labels=c('Increase','No change','Decrease'),
                opacity = 0.7,position = "topright")
  })
}
# Run the application
shinyApp(ui = crime_ui, server = crime_server)