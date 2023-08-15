LipidHarmony <- function(dataset, links, annot, sheetNum, skipNum, minCol,maxCol,charCols,...,sampleKey) {

  filename <- linkpath(dataset, links)

  # Get trait names
  traits <- unlist(read_excel(filename, sheet = sheetNum,
                              skip = skipNum, n_max = 1, col_names = FALSE)[1,])
  traits <- traits[!is.na(traits)]
  names(traits) <- NULL

  # Get data
  library(dplyr)

  out <- read_excel(filename, sheet = sheetNum, skip = skipNum,
                    .name_repair = "minimal")
  out<-out%>%
    mutate(mouse_id = paste(Strains, Number, sep = "-"))
  colnames(out)
  out <- out %>%
    filter(!is.na(Sexes))


  # Remove unwanted columns
  out <- out[, -c(minCol:maxCol)]

  # Adding diet column
  out <- left_join(out,
                   annot %>%
                     rename(animal = "number", condition = "diet") %>%
                     select(mouse_id, strain, animal, sex, condition),
                   by = "mouse_id")

  # Pivot to longer format
  out <- out %>%
    pivot_longer(cols = !charCols,
                 names_to = "lipids",
                 values_to = "value")

  out %>%
    rename(trait = "lipids") %>%
    select(strain, sex, animal, condition, trait, value)
}
