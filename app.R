# packages
library(shiny)
library(leaflet)
options( warn = -1 )

load("./disl_temp_salt.RData")

# make df for leaflet
mapDF <- data.frame(
  location = c("Dauphin Island"),
  lat = c(30.25125),
  lng = c(-88.07783333),
  hrefValue = c("disl")
)


# ui
ui <- fluidPage(

  tagList(
  # link js
  tags$head(tags$link(includeScript("func.js"))),
  tags$head(tags$style("a{cursor:pointer;}"))
  ),
  # UI
  navbarPage(
    title="",
    tabPanel("Home", 
             value ="home",
             titlePanel("Mobile Bay Stations"),
             h4("Data available for each station"),
             leafletOutput("map")
    ),
    tabPanel("Dauphin Island",
             value="disl",
             titlePanel("Dauphin Island"),
             #p("...some text here..."),
             #helpText("Would you like to go back? If so click ", 
             #         HTML("<a onclick=","customHref('home')" ,">",
             #              "here","</a>"),
    fluidRow(
      column(6,
        radioButtons("timeplot", "Values:",
                 c("Temperature"="temp", "Salinity"="salt", "Both"="both"))),
      column(6,
        sliderInput("timeRange", label = "Time range",
                min = as.POSIXct("2019-01-01 00:00:00",tz = 'CST6CDT'),
                max = as.POSIXct("2019-12-31 15:00:00",tz = 'CST6CDT'),
                value = c(as.POSIXct("2019-01-01 00:00:00",tz = 'CST6CDT'),
                          as.POSIXct("2019-12-31 15:00:00",tz = 'CST6CDT')))
        )
      ),

        plotOutput("timeplot")
  ),
  
  tabPanel("All Stations",value="flask",titlePanel("All Stations"),
           HTML('<iframe width="560" height="315" src="https://flask.oybcst.tk" frameborder="0" allowfullscreen
      style="position:absolute;top:90;left:0;width:100%;height:100%;"></iframe>'))
)
)

# server
server <- function(input, output){
  
  # make map
  output$map <- renderLeaflet({
   
    leaflet(data = mapDF) %>%
      setView(
        lat = 30.50355, 
        lng = -87.98733,
        zoom = 9) %>%
      addTiles() %>%
      addMarkers(lng = ~lng , lat = ~lat,
                 popup = paste0(
                   "<a onclick=","customHref('",mapDF$hrefValue,"')>",
                   mapDF$location,
                   "</a>"
                 ))
  })
  
  output$timeplot <- renderPlot({
     if(input$timeplot=="temp" || input$timeplot=="both"){
       y_var <- disl_temp_salt$Temperature
       y_label <- "Temperature (\u00B0C)"
       y_letter <- "T"
     }else{
      y_var <- disl_temp_salt$Salinity
      y_label <- "Salinity"
      y_letter <- "S"
     }
    if(input$timeplot=="both"){
      y2_var <- disl_temp_salt$Salinity
      y_label <- "Temperature (\u00B0C), Salinity"
      y2_letter <- "S"
    }
    
    ind_1 <- which(disl_temp_salt$Time == input$timeRange[1])
    ind_2 <- which(disl_temp_salt$Time == input$timeRange[2])
    Time <- disl_temp_salt$Time[ind_1:ind_2]
    Y_var <- y_var[ind_1:ind_2]
    i_min <- format(min(Y_var),digits=3)
    i_max <- format(max(Y_var),digits=3)
    i_mean <- format(mean(Y_var),digits=3)
    if(input$timeplot=="both"){
      Y2_var <- y2_var[ind_1:ind_2]
      i2_min <- format(min(Y2_var),digits=3)
      i2_max <- format(max(Y2_var),digits=3)
      i2_mean <- format(mean(Y2_var),digits=3)     
    }
    
    xtitle=paste("Time: ",as.Date(input$timeRange[1])," until ",as.Date(input$timeRange[2]))
    #xlabel=paste(y_label," - ")
    x_label <- paste0(y_letter,"min=",i_min,"    ",
                     y_letter,"mean=",i_mean,"    ",
                     y_letter,"max=",i_max)
    if(input$timeplot=="both"){
      x_label <- paste0(y_letter,"min=",i_min,"    ",
                        y_letter,"mean=",i_mean,"    ",
                        y_letter,"max=",i_max,"       ",
                        y2_letter,"min=",i2_min,"    ",
                        y2_letter,"mean=",i2_mean,"    ",
                        y2_letter,"max=",i2_max)
    }
    plot(Time,Y_var,main=xtitle,xlab=x_label,ylab=y_label,pch=20,xaxt="n",cex=0.3,ylim=c(0,35),xlim=c(input$timeRange[1],input$timeRange[2]))
    axis.POSIXct(1, Time,format="%b %d")
    if(input$timeplot!="both") abline(h = c(i_min,i_mean,i_max), col = c("#D1D0DE","#636D97","#D1D0DE"),lwd=2)
    if(input$timeplot=="both"){
      points(Time,Y2_var,pch=20,cex=0.3,col="red")
    }
  })
  
  
}


# launch
shinyApp(ui, server)


