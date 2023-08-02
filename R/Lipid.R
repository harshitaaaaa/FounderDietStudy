LiverLipHarmony <- function(dataset, annot, links, sheetNum, skipNum, minCol,maxCol,charCols,...,sampleKey) {
  # Get trait names
  traits <- unlist(read_excel(dataset, sheet = sheetNum,
                              skip = skipNum, n_max = 1, col_names = FALSE)[1,])
  traits <- traits[!is.na(traits)]
  names(traits) <- NULL

  # Get data
  out <- read_excel(dataset, sheet = sheetNum, skip = skipNum,
                    .name_repair = "minimal")

  # Remove unwanted columns
  out <- out[, -c(minCol:maxCol)]

  # adding mouse_id if absent
  if (!missing(sampleKey)) {
    #print("entered")
    out <- out%>%
      left_join(sampleKey %>%
                  select(`label on vial`, `sample name`),
                by = c("Name" = "label on vial")) %>%
      rename(mouse_id = `sample name`)

  }

  # Add diet column
  out <- left_join(
    out,
    annot %>%
      rename(animal = "number", condition = "diet") %>%
      select(mouse_id, strain, animal, sex, condition),
    by = "mouse_id"
  )

  # pivot to longer format
  out <- out %>%
    pivot_longer(cols = !charCols,
                 names_to = "lipids", values_to = "value")



  return(out)
}


