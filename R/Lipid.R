
LiverLipHarmony <- function(dataset, annot, links, sheetNum, skipNum, minCol, maxCol, charCols, ..., sampleKey) {
  # Get trait names
  traits <- unlist(readxl::read_excel(dataset, sheet = sheetNum, skip = skipNum, n_max = 1, col_names = FALSE)[1,])
  traits <- traits[!is.na(traits)]
  names(traits) <- NULL

  # Get data
  out <- readxl::read_excel(dataset, sheet = sheetNum, skip = skipNum, .name_repair = "minimal")

  # Remove unwanted columns
  out <- out[, -c(minCol:maxCol)]

  # adding mouse_id if absent
  if (!missing(sampleKey)) {
    out <- out %>%
      dplyr::left_join(sampleKey %>% dplyr::select(`label on vial`, `sample name`), by = c("Name" = "label on vial")) %>%
      dplyr::rename(mouse_id = `sample name`)
  }

  # Add diet column
  out <- dplyr::left_join(out, annot %>% dplyr::rename(animal = "number", condition = "diet") %>% dplyr::select(mouse_id, strain, animal, sex, condition), by = "mouse_id")

  # pivot to longer format
  out <- out %>%
    dplyr::pivot_longer(cols = !charCols, names_to = "lipids", values_to = "value")

  return(out)
}


