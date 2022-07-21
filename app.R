# packages
library(shiny)
library(leaflet)

load("./disl_temp_salt.RData")

# make df for leaflet
mapDF <- data.frame(
  location = c("Dauphin Island", "I7-F", "A7-F"),
  lat = c(30.25125, 30.58258333, 30.17156667),
  lng = c(-88.07783333, -88.06091667, -88.05013333),
  hrefValue = c("disl", "two", "three")
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
             titlePanel("Dauphin Island")
             #p("...some text here..."),
             #helpText("Would you like to go back? If so click ", 
             #         HTML("<a onclick=","customHref('home')" ,">",
             #              "here","</a>"),
    ,
    fluidRow(
      column(6,
    radioButtons("timeplot", "Values:",
                 c("Temperature"="temp", "Salinity"="salt"))),
    column(6,
    sliderInput("timeRange", label = "Time range",
                min = as.POSIXct("2019-01-01 00:00:00"),
                max = as.POSIXct("2019-12-31 15:00:00"),
                value = c(as.POSIXct("2019-01-01 00:00:00"),
                          as.POSIXct("2019-12-31 15:00:00"))))),
    plotOutput("timeplot")),
    tabPanel("I7-F",
             value="two",
             titlePanel("I7-F")
             #p("...some text here..."),
             #helpText("Would you like to go back? If so click ", 
             #         HTML("<a onclick=","customHref('home')" ,">",
             #              "here","</a>"))
             ),
    tabPanel("A7-F",
             value="three",
             titlePanel("A7-F")
             #p("...some text here..."),
             #helpText("Would you like to go back? If so click ", 
             #         HTML("<a onclick=","customHref('home')" ,">",
             #              "here","</a>")))
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
     if(input$timeplot=="temp"){
       y_var <- disl_temp_salt$Temperature
       y_label <- "Temperature"
     }
    if(input$timeplot=="salt"){
      y_var <- disl_temp_salt$Salinity
      y_label <- "Salinity"
    }
    output$from <- renderText(input$timeRange[1]);
    output$to <- renderText(input$timeRange[2]);
    xlabel=paste("Time: ",as.Date(input$timeRange[1])," until ",as.Date(input$timeRange[2]))
    #xlabel="Time"
    plot(disl_temp_salt$Time,y_var,xlab=xlabel,ylab=y_label,pch=20,xlim=c(input$timeRange[1],input$timeRange[2]),xaxt="n")
    axis.POSIXct(1, disl_temp_salt$Time,format="%b %d")
  })
  
}


# launch
shinyApp(ui, server)


