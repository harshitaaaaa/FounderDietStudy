userDatasets <- function() {
  list(
    "This study has two diets (HC_LF = high carb, low fat; HF_LC = high fat, low carb) with three measurement sets collected on 192 mice:",
    tags$ul(
      tags$li("physio: physiological data"),
      tags$li("liver: RNA-seq on liver"),
      tags$li("plasma: concentrations of circulating metabolites")),
    "See also",
    shiny::a("Attie Lab Diabetes Database", href = "http://diabetes.wisc.edu/"),
    "and",
    "GigHub:", shiny::a("byandell/FounderDietStudy",
                        href = "https://github.com/byandell/FounderDietStudy"))
}
