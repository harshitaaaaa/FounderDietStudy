library(shiny)

source("subset_trait_names.sql.R")

db <- RSQLite::dbConnect(RSQLite::SQLite(), "traitData.sqlite")
traitData <- dplyr::tbl(db, "traitData")

ui <- fluidPage(
  titlePanel('Demo for SQLite'),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = 'trait',
        label = 'Trait:',
        choices = paste0("BW_", 6:15, "wk"),
        selected = 'BW_6wk'
      ),
      br(),
    ),
    mainPanel(
      shiny::uiOutput("table")
    )
  )
)
  
server <- function(input, output, session) {
  
  shiny::onStop(function() {RSQLite::dbDisconnect(db)})
  
  output$table <- shiny::renderUI({
    shiny::req(input$trait)
    datatraits <- paste0("Physio: ", local(input$trait))
    
#    object <- dplyr::tbl(db, "traitData")
#    object <- dplyr::collect(
#      dplyr::filter(
#        object,
#        .data$dataset == "Physio" & .data$trait %in% datatrait))
    
    object <- subset_trait_names.sql(traitData, datatraits)
    
    DT::renderDataTable(object)
  })
}

shiny::shinyApp(ui = ui, server = server)
