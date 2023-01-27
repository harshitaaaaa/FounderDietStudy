library(shiny)
devtools::install_github("byandell/foundr")
library(foundr)

traits <- readRDS("traits.rds")
traitsum <- readRDS("traitsum.rds")
condition <- "sex_diet"

################################################################

source("foundrDietStudy.R")

ui <- foundr::foundrUI("Founder Diet Study")

server <- function(input, output, session) {
    
  foundr::foundrServer(input, output, session,
                       traits, traitsum, condition)
  
  # Allow reconnect with Shiny Server.
  session$allowReconnect(TRUE)
}


shiny::shinyApp(ui = ui, server = server)
