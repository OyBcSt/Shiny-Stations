library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

load("./stations.RData")

mapDF <- data.frame(
  location = c("Dauphin Island"),
  lat = c(30.25125),
  lng = c(-88.07783333),
  hrefValue = c("disl")
)

#me
stationdata <- stationsDF
nodata <- stationsDF[stationsDF$fileExists == FALSE,]
hasdata <- stationsDF[stationsDF$fileExists == TRUE,]


function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -87.98733, lat = 30.50355, zoom = 9)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
#  stationsInBounds <- reactive({
#    if (is.null(input$map_bounds))
#      return(stationdata[FALSE,])
#    bounds <- input$map_bounds
#    latRng <- range(bounds$north, bounds$south)
#    lngRng <- range(bounds$east, bounds$west)
#    
#    subset(stationdata,
#           latitude >= latRng[1] & latitude <= latRng[2] &
#             longitude >= lngRng[1] & longitude <= lngRng[2])
#    
#  }) 

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    
#    leafletProxy("map", data = stationdata) %>%
#      clearShapes() %>%
#      addCircles(~Lon, ~Lat, radius=700, layerId=~Station,
#                 stroke=FALSE, fillOpacity=.8, fillColor=~color)

        leafletProxy("map", data = hasdata) %>%
         clearShapes() %>%
          addCircles(~Lon, ~Lat, radius=700, layerId=~Station, group = "Has Model Output",
                    stroke=FALSE, fillOpacity=.8, fillColor=~color) 

        leafletProxy("map", data = nodata) %>%
         addCircles(~Lon, ~Lat, radius=700, layerId=~Station,group = "No Model Output",
                 stroke=FALSE, fillOpacity=.8, fillColor=~color) %>%
         addLayersControl(
            overlayGroups = c("Has Model Output", "No Model Output"),
            options = layersControlOptions(collapsed = TRUE)
        )
          
  })

  # Show a popup at the given location
  showStationPopup <- function(station, lat, lng) {
    selectedStation <- stationsDF[stationsDF$Station == station,]
    
    content <- as.character(tagList(
      tags$strong(HTML(sprintf("%s",selectedStation$Station
      ))), tags$br()
    ))

    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = station)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showStationPopup(event$id, event$lat, event$lng)
    })
  })

  
  ## Data Explorer ###########################################

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.2
      station <- input$goto$station
      lat <- input$goto$lat
      lng <- input$goto$lng
      showStationPopup(station, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })


  output$stationtable <- DT::renderDataTable({
    df <- stationsDF %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Lon, '" data-station="', Station, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df, outputId = "stationtable")
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
 
  # This observer is for when a new data file is to be loaded.
  observe({
    output$value <- renderPrint(input$stationFile)
    data <- read.csv(input$stationFile)
    #Get rid of zero rows
    data <- data[rowSums(is.na(data)) == 0,]
    
    Time <- as.Date(data$Date, format = "%m/%d/%Y")
    TSurf <- data$T.Surface
    Tbot <- data$T.Bottom
    
    output$timeplot <- renderPlot({
      if(input$timeplot=="surftemp" || input$timeplot=="both"){
        Y_var <- TSurf
        y_label <- "Surface Temperature (\u00B0C)"
        y_letter <- "Ts"
      }else{
        Y_var <- Tbot
        y_label <- "Bottom Temperature (\u00B0C)"
        y_letter <- "Tb"
      }
      if(input$timeplot=="both"){
        Y2_var <- Tbot
        y_label <- "Surface, Bottom Temperature"
        y2_letter <- "Tb"
      }
      

      i_min <- format(min(Y_var),digits=3)
      i_max <- format(max(Y_var),digits=3)
      i_mean <- format(mean(Y_var),digits=3)
      if(input$timeplot=="both"){
        #Y2_var <- y2_var[ind_1:ind_2]
        i2_min <- format(min(Y2_var),digits=3)
        i2_max <- format(max(Y2_var),digits=3)
        i2_mean <- format(mean(Y2_var),digits=3)
      }
      
      x_label <- paste0(y_letter,"min=",i_min,"    ",
                        y_letter,"mean=",i_mean,"    ",
                        y_letter,"max=",i_max)
      if(input$timeplot=="both"){
        surf_label <- paste("Surface: min=",i_min,
                          "mean=",i_mean,
                          "max=",i_max)
        bot_label <- paste(
                          "Bottom:  min=",i2_min,
                          "mean=",i2_mean,
                          "max=",i2_max)
      }

     
      if(input$timeplot!="both") {
        plot(Time,Y_var,xlab=x_label,ylab=y_label,xaxt="n",pch=20,cex=0.3,col="black",bg="black",ylim=c(10,35))
        axis.Date(1, Time,format="%b %d") 
        abline(h = c(i_min,i_mean,i_max), col = c("#D1D0DE","#636D97","#D1D0DE"),lwd=2)
      }
        
      if(input$timeplot=="both"){
        plot(Time,Y_var,xlab="",ylab="",xaxt="n",pch=20,cex=0.3,col="black",bg="black",ylim=c(10,35))
        axis.Date(1, Time,format="%b %d")
        points(Time,Y2_var,pch=20,cex=0.3,col="red",bg="red")
        mtext(surf_label, side=1, line=3, col="black", cex=1, adj=0)
        mtext(bot_label, side=1, line=4, col="red", cex=1, adj=0)
        
      }
    })
    
  }) 
    
   
  
  # When map is clicked, show a popup with city info
  observe({
    event <- input$map_shape_click
    content <- as.character(tagList(
      tags$strong(HTML(sprintf("%s",event$id
      ))), tags$br()
    ))

    content2 <- paste0(
      "<a onclick=","customHref('",mapDF$hrefValue,"')><b>Go to Station ",
      event$id,
      "</b></a>"
    )
    
 
    output$whichstation <- renderText("")
    if (is.null(event))
      return()
    
    isolate({
      this_station <- stationsDF[stationsDF$Station== event$id,]
      if(this_station$fileExists==FALSE){ 
        content <- paste(content,"(no data)")
      }
      output$whichstation <- renderUI({
        HTML(content)
      })
    })
  })
  
  # When map is clicked, show the station name in the viz tab
  observe({
    event <- input$map_shape_click
    content <- as.character(tagList(
      tags$strong(HTML(sprintf("%s",event$id
      ))), tags$br()
    ))
    output$vizstation <- renderText("none selected")
    if (is.null(event))
      return()
    
    isolate({
      #output$vizstation <- renderText(event$id)
      this_station <- stationsDF[stationsDF$Station== event$id,]
      if(this_station$fileExists==FALSE){ 
        content <- paste(content,"(no data)")
      }
      output$vizstation <- renderUI({
        HTML(content)
      })
      this_station <<- event$id
      output$nText <- renderText({event$id})
    })
  })

  #Button stuff
  # builds a reactive expression that only invalidates 
  # when the value of input$goButton becomes out of date 
  # (i.e., when the button is pressed)
  #ntext <- eventReactive(input$goButton, {
  #  input$n
  #})


    
 
}
