foundrIntro <- function() {
  renderUI({
    tagList(
      "This founder dataset consists of",
      shiny::a("8 CC mice strains,",
               href = "https://www.jax.org/news-and-insights/2009/april/the-collaborative-cross-a-powerful-systems-genetics-tool"),
      "two diets (HC_LF = high carb, low fat; HF_LC = high fat, low carb) and both sexes with three measurement sets collected on 192 mice:",
      tags$ul(
        tags$li("physio: physiological data"),
        tags$li("liver: RNA-seq on liver"),
        tags$li("plasma: concentrations of circulating metabolites")),
      "Select one or more traits after deciding measurement set(s) and trait order. Traits window supports partial matching to find desired traits.",
      "Facet plots by strain or sex_diet and subset strains if desired.",
      "Plots and data means (for selected traits) and data summaries (for whole measurement set) can be downloaded.",
      "See",
      shiny::a("Attie Lab Diabetes Database", href = "http://diabetes.wisc.edu/"),
      "for earlier study.",
      "GigHub:", shiny::a("byandell/FounderDietStudy",
                          href = "https://github.com/byandell/FounderDietStudy"))
  })
}

foundrScatplot <- function(trait, traitdata, pair) {
  dat <- 
    purrr::map(
      pair,
      function(x) {
        # Split trait pair by colon
        x <- stringr::str_split(x, " ON ")[[1]][2:1]
        # create out with columns for each trait pair
        out <- tidyr::unite(traitdata, sex_diet, sex, diet)
        # create plot
        foundr::scatplot(out, x[1], x[2], shape_sex = FALSE) +
          ggplot2::facet_grid(. ~ sex_diet)
      })
  
  # Patch plots together by rows
  patchwork::wrap_plots(dat, nrow = length(dat))
}

foundrData <- function(datatraits, trait) {
  ltrait <- length(trait)
  tidyr::unite(
    dplyr::mutate(
      datatraits,
      trait = abbreviate(trait, ceiling(60 / ltrait))),
    sex_diet, sex, diet)
}

foundrMean <- function(datatraits) {
  dplyr::arrange(
    tidyr::pivot_wider(
      dplyr::mutate(
        dplyr::ungroup(
          dplyr::summarize(
            dplyr::group_by(datatraits, strain, sex_diet, trait),
            value = mean(value, na.rm = TRUE), .groups = "drop")),
        value = signif(value, 4)),
      names_from = "strain", values_from = "value"),
    trait, sex_diet)
}
