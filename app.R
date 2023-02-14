library(shiny)
devtools::install_cran("plotly") # plotly not yet on UW dataviz
devtools::install_cran("markdown") # markdown not yet on UW dataviz
devtools::install_cran("patchwork") # patchwork not yet on UW dataviz
devtools::install_github("byandell/foundr")
library(foundr)

traitData <- readRDS("traitData.rds")
traitStats <- readRDS("traitStats.rds")
traitSignal <- readRDS("traitSignal.rds")
helppath <- "help.md"

################################################################

ui <- foundr::foundrUI("Founder Diet Study")

server <- function(input, output, session) {
    
  foundr::foundrServer(input, output, session,
                       traitData, traitStats, traitSignal, helppath)
  
  # Allow reconnect with Shiny Server.
  session$allowReconnect(TRUE)
}


shiny::shinyApp(ui = ui, server = server)
