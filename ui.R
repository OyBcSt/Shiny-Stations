library(leaflet)


navbarPage("Mobile Bay Stations", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js"),
        includeScript("goviz.js"),
        tags$head(tags$style("a{cursor:pointer;}"))
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 120, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Stations"),
        helpText("Click the map to choose a station.  Blue stations have data."),
        htmlOutput("whichstation", inline = FALSE)
        
      ),

      tags$div(id="cite",
        'Data compiled for ', tags$em('NOAA-RESTORE')
      )
    )
  ),

  tabPanel("Station Lookup",
    hr(),
    DT::dataTableOutput("stationtable"),
    conditionalPanel("false", icon("crosshair"))
  ),
  
  tabPanel("Model Output",
           value = "disl",
           fluidRow(
             column(2,
                    h3("Select Station"),
                    br()),
             column(3,
                    h5("Current map point is:"),
                    h3(htmlOutput("vizstation", inline = FALSE))
                    ),
             column(3,
                    selectInput("stationFile", "Select a Station - does not automatically choose map point", vars)
             ),
             column(4,
                    helpText("Data file is:"),
                    verbatimTextOutput("value")
                    )),
             #column(4,
             #       h3("Time Range"),
             #       sliderInput("timeRange", label = "Time range",
             #                   min = as.POSIXct("2019-01-01 00:00:00",tz = 'CST6CDT'),
             #                   max = as.POSIXct("2019-12-31 15:00:00",tz = 'CST6CDT'),
             #                   value = c(as.POSIXct("2019-01-01 00:00:00",tz = 'CST6CDT'),
             #                             as.POSIXct("2019-12-31 15:00:00",tz = 'CST6CDT')))
             #)
           
           fluidRow(
             column(1,
             ),
             column(2,
                    h3("Values"),
                    radioButtons("timeplot", "Values:",
                                 c("Surface Temperature"="surftemp", "Bottom Temperature"="bottemp", "Both"="both"))),
             column(8,
                    plotOutput("timeplot")    
                    ),
             column(1,
             )
             ))

  
)
