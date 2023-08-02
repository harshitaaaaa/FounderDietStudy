Enrich3Harmony <- function(dataset, links, annot, ...) {
  filename <- linkpath(dataset, links)
  
  # Find all the sheets (3 of import).
  all_sheets <- excel_sheets(filename)
  
  # Data processing for all sheets in the file except first.
  processed_datasets <- 
    lapply(all_sheets[-1], 
           function(sheet) {
             sheet_data <- read_excel(filename, sheet = sheet)
             timepoint_data_cleaning(sheet_data)
           })
  
  # Combine all the processed datasets into a single dataframe
  combined_dataset <- do.call(rbind, processed_datasets)
  
  # Renaming Strains to mouse_id  
  combined_dataset <- combined_dataset %>%
    rename(mouse_id = Strains)
  
  # Adding strain, animal,sex and condition based on annotation file
  left_join(
    combined_dataset,
    annot %>% 
      rename(animal = "number", condition = "diet") %>% 
      select(mouse_id,  strain, animal, sex, condition),
    by = c("mouse_id")
  ) %>%
    rename(value = "NA Corrected with zero") %>%
    select(strain, sex, animal, condition, trait, value)
}

# Data Cleaning function 
timepoint_data_cleaning <- function(dataset) {
  dataset <- dataset %>%
    # Rename labels as L0,L1,L2,L3,L4,L5,L6
    mutate(Label = gsub("PARENT", "L0", Label),
           Label = gsub("C13-label-1", "L1", Label),
           Label = gsub("C13-label-2", "L2", Label),
           Label = gsub("C13-label-3", "L3", Label),
           Label = gsub("C13-label-4", "L4", Label),
           Label = gsub("C13-label-5", "L5", Label),
           Label = gsub("C13-label-6", "L6", Label))
  
  # Correcting format of Timepoints column
  dataset %>%
    mutate(Timepoints = as.numeric(gsub("min", "", Timepoints)),
           trait = paste(Name, Label, Timepoints, "wk18", sep = "_"))
}