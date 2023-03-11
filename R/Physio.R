PhysioHarmony <- function(dataset, links, ...) {
  filename <- linkpath(dataset, links)
  
  # Data are in sheet 2 starting on line 1
  read_excel(filename, sheet = 2) %>%
    
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
    select(strain, sex, animal, condition, trait, value) %>%
    
    # Normal scores by trait
    group_by(trait) %>%
    mutate(value = foundr::nqrank(value, jitter = TRUE)) %>%
    ungroup()
}