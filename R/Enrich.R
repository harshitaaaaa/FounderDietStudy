EnrichHarmony <- function(dataset, links, annot, ...) {
  filename <- linkpath(dataset, links)
  
  # Get trait names from 2nd row.
  traits <- unlist(read_excel(filename, sheet = 1,
                      skip = 1, n_max = 1, col_names = FALSE)[1,])
  traits <- traits[!is.na(traits)]
  names(traits) <- NULL
  
  # Read data and pivot to longer format; rename animal and sex.
  out <- read_excel(filename, sheet = 1, skip = 2,
             .name_repair = "minimal") %>% 
    pivot_longer(-(strain:Sexes),
                 names_to = "minutes", values_to = "value") %>%
    
    # Rename animal and sex.
    rename(animal = "number",
           sex = "Sexes")
    
  # Add diet column
  out <- left_join(
    out,
    annot %>%
      rename(animal = "number",
             condition = "diet") %>%
      select(strain, animal, sex, condition),
    by = c("strain","animal","sex")) 
  
  # Determine number of samples and number of time points.
  nsample <- out %>%
    distinct(strain,animal,sex) %>%
    count()
  ntime <- out %>%
    distinct(minutes) %>% count()
  
  # Add trait names from row 2 of file.
  # Assume here the same number of time points per trait.
  out <- out %>%
    mutate(
      trait =
        rep(
          rep(
            traits,
            rep(
              ntime,
              length(traits))),
          nsample))
  
  # Area under curve
  auc <- area_under_curve(out)
  
  
  # These are harmonized columns and their names.
  bind_rows(
    out %>%
      
      # Unite trait and minutes to form new trait by minutes.
      unite(
        trait,
        trait, minutes) %>%
      select(strain, sex, animal, condition, trait, value),
    auc %>%
      select(strain, sex, animal, condition, trait, value)) %>%
    
    # Make sure animal is character.
    # Add `_18wk` to end of trait names.
    mutate(animal = as.character(animal),
           trait = paste(trait, "18wk", sep = "_"))
}
