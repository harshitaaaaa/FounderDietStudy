linkpath <- function(dataset, datadir = "data") {
  # Data for this repository are identified by `data/source.csv`,
  # which is not saved with the repo.
  
  filename <- (links %>% filter(object == dataset))$address
  if(datadir != "")
    filename <- file.path(datadir, filename)
  filename
}

overallfit <- function(traitdata) {
  formful <- formula(value ~ strain * sex * diet)
  formred <- formula(value ~ strain * sex + sex * diet)
  fitful <- stats::lm(formful, traitdata)
  fitred <- stats::lm(formred, traitdata)
  ((broom::tidy(stats::anova(fitred, fitful)) %>%
      select(p.value))[2,])$p.value
}

broomit <- function(traitsdata) {
  map(
    split(traitsdata, traitsdata$trait),
    function(traitdata) {
      ct <- distinct(traitdata, strain, sex, diet)
      ct <- count(ct, sex, diet)
      if(nrow(ct) < 4 | min(ct$n) < 2) {
        # Do nothing if any combination of sex, diet is empty.
        return(NULL)
      }
      form <- formula(value ~ strain * sex * diet)
      fit <- stats::lm(form, traitdata)
      as.tibble(data.frame(
        rawSD = sd(traitdata$value, na.rm = TRUE),
        overall = overallfit(traitdata),
        broom::tidy(stats::drop1(fit, fit, test = "F")) %>%
          filter(grepl("strain:", term)) %>%
          select(term, p.value) %>%
          pivot_wider(names_from = "term",
                      values_from = "p.value")))
    }) %>%
    bind_rows(.id = "trait")
}

residit <- function(traitsdata) {
  redfit <- function(traitdata) {
    formred <- formula(value ~ strain * sex + sex * diet)
    fitred <- stats::lm(formred, traitdata)
    resids <- rep(NA, nrow(traitdata))
    resids[!is.na(traitdata$value)] <- resid(fitred) + mean(traitdata$value, na.rm = TRUE)
    traitdata %>%
      mutate(residred = resids)
  }
  
  map(
    split(traitsdata, traitsdata$trait),
    function(traitdata) {
      ct <- distinct(traitdata, strain, sex, diet)
      ct <- count(ct, sex, diet)
      if(nrow(ct) < 4 | min(ct$n) < 2) {
        # Do nothing if any combination of sex, diet is empty.
        return(NULL)
      }
      redfit(traitdata)
    }) %>%
    bind_rows(.id = "trait")
}
