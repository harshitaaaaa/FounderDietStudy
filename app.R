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
                    c("variability", "alphabetical", "original", "p_sex_diet", "p_diet", "p_sex"),
                    "variability"),
        uiOutput("trait"),
        sliderInput("height", "Plot height (in):", 3, 10, 6, step = 1),
        shiny::column(
          7,
          shiny::textInput("plotfile", "Plot File", "myplot")),
        shiny::column(
          5,
          downloadButton("downloadPlot", "Plots")))),
    
    # Main panel for displaying outputs ----
    mainPanel(
      uiOutput("distUI")
      
    )
  )
)

server <- function(session, input, output) {
  
  traitsum <- reactive({
    readRDS("traitsum.rds")
  })
  dataset <- reactive({
    readRDS(paste0(req(input$datatype), ".rds")) %>%
      select(strain, number, sex, diet, trait, value)
  })
  traitorder <- reactive({
    req(traitsum(), input$order, input$datatype)
    out <- traitsum() %>%
      filter(datatype == input$datatype)
    (switch(input$order,
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
               arrange(strain.sex.diet)))$trait
  })
  
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
  
  distplot <- reactive({
    if(!isTruthy(dataset()) | !isTruthy(input$trait)) {
      return(ggplot())
    }
    if(!all(input$trait %in% dataset()$trait)) {
      return(ggplot())
    }
    ltrait <- length(input$trait)
    
    ggplot(dataset() %>%
             filter(trait %in% req(input$trait)) %>%
             mutate(trait = abbreviate(trait, ceiling(60 / ltrait))) %>%
             unite(sex_diet, sex, diet)) +
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
  output$distUI <- renderUI({
    req(input$height)
    plotOutput("distPlot", height = paste0(input$height, "in"))
  })
  output$downloadPlot <- shiny::downloadHandler(
    filename = function() {
      paste0(shiny::req(input$plotfile), ".pdf") },
    content = function(file) {
      req(input$height)
      grDevices::pdf(file, width = 9, height = input$height)
      print(distplot())
      grDevices::dev.off()
    }
  )
}

shiny::shinyApp(ui = ui, server = server)
