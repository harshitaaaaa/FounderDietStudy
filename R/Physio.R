PhysioHarmony <- function(dataset, links, ...) {
  filename <- linkpath(dataset, links)
  
  # Data are in sheet 2 starting on line 1
  out <- read_excel(filename, sheet = 2) %>%
    
    # Traits begin in column 5
    pivot_longer(-(1:4), names_to = "trait", values_to = "value") %>%
    
    # Diets coded in file.
    mutate(diet = ifelse(as.character(diet) == "200339",
                         "HC_LF", "HF_LC")) %>%
    
    # Rename columns to harmonize data.
    rename(condition = "diet",
           animal = "number") %>%
    mutate(animal = as.character(animal)) %>%
    
    # These are harmonized columns and their names.
    select(strain, sex, animal, condition, trait, value)
  
  # Add GTT area under curve traits
  GTT <- out %>%
    filter(grepl("GTT", trait)) %>%
    separate_wider_delim(
      trait,
      delim = "_",
      names = c("gtt","trait","minutes","week")) %>%
    select(-gtt) %>%
    area_under_curve() %>%
    unite(trait, trait, week) %>%
    mutate(trait = paste0("GTT_", trait)) %>%
    select(strain, sex, animal, condition, trait, value)

  bind_rows(out, GTT)
}

GTT_look <- function(dataset, links, ...) {
  # Write a CSV file with various AUC calculations.
  out <- PhysioGTTHarmony(dataset, links)
  
  out <- out %>%
    filter(grepl("AUC", trait),
           trait != "cpep_ins_molar_ratio_iAUC_14wk") %>%
    mutate(trait = ifelse(grepl("GTT", trait),
                          str_remove(trait, "^GTT_"),
                          trait)) %>%
    separate_wider_delim(
      trait,
      delim = "_",
      names = c("trait","auc","week")) %>% 
    pivot_wider(names_from = "auc", values_from = "value")
  
  write.csv(out, "auc.csv")
  invisible(out)
}