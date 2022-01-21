#' myApp
#'
#' @param ... No input needed, function runs the app
#' @return no object is returned, just an app
#' @import shiny
#' @export

# pkgload::load_all(".");
# pkgload::load_all(".");
# roxygen2::roxygenise()
# myApp()
# library(shiny)

myApp <- function(...) {
  brand = c("Actigraph", "ActivPAL", "GENEActiv", "Axivity", "MOX")
  experiment = c("hfcr", "lfcr", "mfcr")
  shakerfreq = c(40, 50, 62, 75, 87, 100, 112, 125, 137, 150, 162, 175, 187, 200, 212, 225, 237, 250)
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
             selectInput("brand", "What accelerometer brand(s) would you like to inspect?", brand),
             selectInput("experiment", "Which experiment would you like to look at?", experiment),
             selectInput("shakerfreq", "Mechanical shaking frequency (rpm)", shakerfreq)
      ),
      mainPanel(
        plotOutput("timeseries")
      )
    )
  )
  
  server <- function(input, output, session) {
    brand = reactive(input$brand)
    experiment = reactive(input$experiment)
    shakerfreq = reactive(input$shakerfreq)
    
    # If brand or experiment changes, load new data
    toListen <- reactive({
      list(brand(), experiment())
    })
    ts = eventReactive(toListen(), {
      filename = paste0("/home/vincent/data/VUMC/shaker_experiments/structured_raw_data/", 
                        brand(),"_ms_",experiment(),".RData")
      print("load1")
      if (file.exists(filename)) {
        print(paste0("Loading file:", filename))
        load(filename)
        if (exists("extracteddata")) {
          print("data loaded")
        }
        print("load3")
        ts = extracteddata$data[[1]]
        print("load4")
      } else {
        print(paste0("File does not exist:", filename))
        ts = NULL
      }
      return(ts)
    })
    toListen2 = reactive({
      list(shakerfreq(), ts())
      })
    # If frequency changes reselect subset
    ts_subset = eventReactive(toListen2(), {
      print("shake1")
      if (length(ts() > 0)) {
        print("shake2")
        print(head(ts()))
        colnames(ts()) = tolower(colnames(ts()))
        print(head(ts()))
        ts_subset = ts()[which(ts()$shaking_frequency == shakerfreq()),c("time", "x", "y", "z")]
        print(head(ts_subset)) 
      }
      print("shake3")
      return(ts_subset)
    })
    output$timeseries = renderPlot({
      plot(ts_subset()$x, type = "l", main = "time series", ylab="X acceleration")
    })
  }
  # Run the application 
  shinyApp(ui, server)
}
