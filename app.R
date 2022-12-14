library(shiny)
library(tidyverse)
library(readr)
library(readxl)
library(RColorBrewer)

sex_diet_colors <- brewer.pal(n=4, name = "Dark2")
names(sex_diet_colors) <- c("F_HC_LF", "F_HF_LC", "M_HC_LF", "M_HF_LC")
CCcolors <- c("#F0E442", "#555555", "#E69F00", "#0072B2",
              "#56B4E9", "#009E73", "#D55E00", "#CC79A7")
names(CCcolors) <- c("AJ", "B6", "129", "NOD", "NZO", "CAST", "PWK", "WSB")
################################################################


ui <- fluidPage(
  
  titlePanel("Founder Diet Study"),
  sidebarLayout(
    sidebarPanel(
      tagList(
        uiOutput("intro"),
        fluidRow(
          shiny::column(
            4,
            selectInput("datatype", "Measurement set",
                        c("physio","liver","plasma"), "physio")),
          shiny::column(
            4,
            selectInput("order", "Order traits by",
                        c("p_sex_diet", "p_diet", "p_sex", "variability", "alphabetical", "original"),
                        "p_sex_diet")),
          shiny::column(
            4,
            radioButtons("facet","Facet by", c("strain","sex_diet"), "strain", inline = TRUE))),
        uiOutput("strains"),
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
      uiOutput("outs"),
      uiOutput("scatPlot")
    )
  )
)

server <- function(session, input, output) {
  
  output$intro <- renderUI({
    tagList("This founder dataset consists of",
            shiny::a("8 CC mice strains,", href = "https://www.jax.org/news-and-insights/2009/april/the-collaborative-cross-a-powerful-systems-genetics-tool"),
            "two diets (HC_LF = high carb, low fat; HF_LC = high fat, low carb) and both sexes with three measurement sets collected on 192 mice:",
            tags$ul(
              tags$li("physio: physiological data"),
              tags$li("liver: RNA-seq on liver"),
              tags$li("plasma: concentrations of circulating metabolites")),
            "Select one or more traits after deciding measurement set and trait order. Traits window supports partial matching to find desired traits.",
            "Facet plots by strain or sex_diet and subset strains if desired.",
            "Plots and data means (for selected traits) and data summaries (for whole measurement set) can be downloaded.",
            "See",
            shiny::a("Attie Lab Diabetes Database", href = "http://diabetes.wisc.edu/"),
            "for earlier study.",
            "GigHub:", shiny::a("byandell/FounderDietStudy",
                                        href = "https://github.com/byandell/FounderDietStudy"))
  })
  
  output$strains <- renderUI({
    choices <- names(CCcolors)
    checkboxGroupInput("strains", "Strains",
                       choices = choices, selected = choices, inline = TRUE)
  })
  
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
  datatraitslong <- reactive({
    req(dataset(), input$trait, input$strains)
    dataset() %>%
      filter(trait %in% input$trait,
             strain %in% input$strains)
  })
  datatraits <- reactive({
    req(datatraitslong(), input$trait)
    ltrait <- length(input$trait)
    datatraitslong() %>%
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
    
    req(input$facet)
    p <- ggplot(datatraits())
    switch(input$facet,
           strain = {
             p <- p +
               aes(sex_diet, value, fill = sex_diet) +
               geom_jitter(size = 3, shape = 21, color = "black", alpha = 0.65) +
               facet_grid(trait ~ strain, scales = "free_y") +
               scale_fill_manual(values = sex_diet_colors)
           },
           sex_diet = {
             p <- p +
               aes(strain, value, fill = strain) +
               geom_jitter(size = 3, shape = 21, color = "black", alpha = 0.65) +
               facet_grid(trait ~ sex_diet, scales = "free_y") +
               scale_fill_manual(values = CCcolors)
           })
    p +
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
                             collapse = "."))
    fluidRow(
      shiny::column(
        6,
        shiny::textAreaInput("plotname", "File Prefix", filename)),
      shiny::column(
        3,
        downloadButton("downloadPlot", "Plots")),
      shiny::column(
        3,
        downloadButton("downloadMean", "Means")))
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
  datameans <- reactive({
    datatraits() %>%
      group_by(strain, sex_diet, trait) %>%
      summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      ungroup() %>%
      mutate(value = signif(value, 4)) %>%
      pivot_wider(names_from = "strain", values_from = "value") %>%
      arrange(trait, sex_diet)
  })
  output$datatable <- DT::renderDataTable(
    datameans(),
    escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 10))
  output$tablesum <- DT::renderDataTable(
    traitarrange() %>%
      mutate(across(where(is.numeric), function(x) signif(x, 4))),
    escape = FALSE,
    options = list(scrollX = TRUE, pageLength = 10))
  output$tablename <- renderUI({
    filename <- req(input$datatype)
    shiny::textInput("tablename", "Summary File Prefix", filename)
  })
  output$downloadMean <- shiny::downloadHandler(
    filename = function() {
      paste0(shiny::req(input$plotname), ".csv") },
    content = function(file) {
      utils::write.csv(datameans(), file, row.names = FALSE)
    }
  )
  output$downloadTable <- shiny::downloadHandler(
    filename = function() {
      req(input$datatype)
      paste0(shiny::req(input$tablename), ".csv") },
    content = function(file) {
      utils::write.csv(traitarrange(), file, row.names = FALSE)
    }
  )
  
  output$pair <- renderUI({
    req(input$trait)
    if(length(input$trait) > 1) {
      selectInput("pair", "Select pair for scatterplots",
                  choices = input$trait, selected = input$trait[1:2],
                  multiple = TRUE)
    }
  })
  scatdata <- reactive({
    req(input$trait, datatraitslong(), input$pair)
    if(length(input$pair) < 2) {
      return(NULL)
    } else {
      traitpair <- rep(input$pair, length = 2)
    }
    dat <- datatraitslong() %>%
      filter(trait %in% traitpair) %>%
      select(strain, number, sex, diet, trait, value) %>%
      pivot_wider(names_from = "trait", values_from = "value")
  })
  output$scatPlot <- renderUI({
    req(input$trait, input$datatype, input$order)
    tagList(
      uiOutput("pair"),
      plotly::plotlyOutput("scatplot")
    )
  })
  output$scatplot <- plotly::renderPlotly({
    req(scatdata(), input$pair)
    if(is.null(scatdata())) {
      return(ggplot())
    }
    if(!all(input$pair[1:2] %in% names(scatdata()))) {
       return(ggplot())
    }
    plotly::ggplotly(
      ggplot(scatdata()) +
        aes(.data[[input$pair[1]]], .data[[input$pair[2]]],
            color = strain) +
        geom_smooth(method = "lm", se = FALSE) +
        geom_point(size = 2) +
        scale_color_manual(values = CCcolors) +
        facet_grid(sex ~ diet))
  })
  
}

shiny::shinyApp(ui = ui, server = server)
