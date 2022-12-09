library(shiny)
library(tidyverse)
library(readr)
library(readxl)

################################################################


ui <- fluidPage(
  
  titlePanel("Founder Diet Study"),
  sidebarLayout(
    sidebarPanel(
      tagList(
        selectInput("datatype", "Data:", c("physio","liver","plasma"), "physio"),
        selectInput("order", "Order traits by",
                    c("p_sex_diet", "p_diet", "p_sex", "variability", "alphabetical", "original"),
                    "p_sex_diet"),
        sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1),
        uiOutput("downloadPlotUI"),
        fluidRow(
          shiny::column(
            6,
            uiOutput("tablename")),
          shiny::column(
            3,
            shiny::downloadButton("downloadTable", "Summary")))),
      uiOutput("trait")),
    
    # Main panel for displaying outputs ----
    mainPanel(
      uiOutput("outs")
      
    )
  )
)

server <- function(session, input, output) {
  
  # Trait summaries (for ordering traits, and summary table)
  traitsum <- reactive({
    readRDS("traitsum.rds")
  })
  dataset <- reactive({
    readRDS(paste0(req(input$datatype), ".rds")) %>%
      select(strain, number, sex, diet, trait, value)
  })
  traitarrange <- reactive({
    req(traitsum(), input$order, input$datatype)
    out <- traitsum() %>%
      filter(datatype == input$datatype)
    switch(input$order,
           variability = 
             out %>%
             arrange(desc(rawSD)),
           alphabetical = 
             out %>%
             arrange(trait),
           original = 
             out,
           p_sex = 
             out %>% 
             arrange(strain.sex),
           p_diet = 
             out %>% 
             arrange(strain.diet),
           p_sex_diet =
             out %>% 
             arrange(strain.sex.diet))
  })
  traitorder <- reactive({
    traitarrange()$trait
  })
  
  # Select traits
  output$trait <- renderUI({
    req(traitorder(), input$order, dataset())
    selectizeInput("trait", "Traits:", choices = NULL, multiple = TRUE)
  })
  observeEvent({
    req(dataset(), input$order)
    },
    {
    updateSelectizeInput(session, "trait", choices = traitorder(),
                         server = TRUE)
  })
  
  # Data for selected traits
  datatraits <- reactive({
    req(dataset(), input$trait)
    ltrait <- length(input$trait)
    dataset() %>%
      filter(trait %in% input$trait) %>%
      mutate(trait = abbreviate(trait, ceiling(60 / ltrait))) %>%
      unite(sex_diet, sex, diet)
  })
  
  
  # Output: Plots or Data
  output$outs <- shiny::renderUI({
    shiny::tagList(
      shiny::radioButtons("button", "", c("Plots", "Data Means", "Data Summary"), "Plots", inline = TRUE),
      shiny::conditionalPanel(
        condition = "input.button == 'Plots'",
        shiny::uiOutput("plots")),
      shiny::conditionalPanel(
        condition = "input.button == 'Data Means'",
        DT::dataTableOutput("datatable")),
      shiny::conditionalPanel(
        condition = "input.button == 'Data Summary'",
        DT::dataTableOutput("tablesum")))
  })
  
  # Plots
  distplot <- reactive({
    if(!isTruthy(dataset()) | !isTruthy(input$trait)) {
      return(ggplot())
    }
    if(!all(input$trait %in% dataset()$trait)) {
      return(ggplot())
    }
    ltrait <- length(input$trait)
    
    ggplot(datatraits()) +
      aes(sex_diet, value, col = sex_diet) +
      geom_jitter() +
      facet_grid(trait ~ strain, scales = "free_y") +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      ylab(ifelse(ltrait == 1, input$trait, "Trait Value")) +
      ggtitle(paste0(input$datatype, " data for trait",
                     ifelse(ltrait > 1, "s ", " "),
                     paste(abbreviate(input$trait, ceiling(60 / ltrait)),
                           collapse = ", ")))
  })
  output$distPlot <- renderPlot({
    distplot()
  })
  output$plots <- renderUI({
    req(input$height)
    plotOutput("distPlot", height = paste0(input$height, "in"))
  })
  output$downloadPlotUI <- renderUI({
    ltrait <- length(req(input$trait))
    filename <- paste0(req(input$datatype), "_",
                       paste(abbreviate(input$trait, ceiling(60 / ltrait)),
                             collapse = "_"))
    fluidRow(
      shiny::column(
        6,
        shiny::textInput("plotname", "Plot File Prefix", filename)),
      shiny::column(
        3,
        downloadButton("downloadPlot", "Plot")))
  })
  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {
      paste0(shiny::req(input$plotname), ".pdf") },
    content = function(file) {
      req(input$height)
      grDevices::pdf(file, width = 9, height = input$height)
      print(distplot())
      grDevices::dev.off()
    })
  
  # Data Table
  output$datatable <- DT::renderDataTable({
    datatraits() %>%
      group_by(strain, sex_diet, trait) %>%
      summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      ungroup() %>%
      mutate(value = signif(value, 4)) %>%
      pivot_wider(names_from = "strain", values_from = "value") %>%
      arrange(trait, sex_diet)},
    escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 10))
  output$tablesum <- DT::renderDataTable(
    traitarrange(),
    escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 10))
  output$tablename <- renderUI({
    filename <- req(input$datatype)
    shiny::textInput("filename", "Summary File Prefix", filename)
  })
  output$downloadTable <- shiny::downloadHandler(
    filename = function() {
      paste0(shiny::req(input$tablename), ".csv") },
    content = function(file) {
      utils::write.csv(traitarrange(), file, row.names = FALSE)
    }
  )
  
}

shiny::shinyApp(ui = ui, server = server)
