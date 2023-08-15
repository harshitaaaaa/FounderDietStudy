LipidHarmony <- function(dataset, links, annot, sheetNum, skipNum, minCol,maxCol,charCols,...,sampleKey) {
  filename <- linkpath(dataset, links)

  # Get trait names
  traits <- unlist(read_excel(filename, sheet = sheetNum,
                              skip = skipNum, n_max = 1, col_names = FALSE)[1,])
  traits <- traits[!is.na(traits)]
  names(traits) <- NULL

  # Get data
  out <- read_excel(filename, sheet = sheetNum, skip = skipNum,
                    .name_repair = "minimal")

  # Remove unwanted columns
  out <- out[, -c(minCol:maxCol)]

  # Adding mouse_id if absent
  if (!missing(sampleKey)) {
    out <- out %>%
      left_join(sampleKey %>%
                  rename(Name = "label on vial",
                         mouse_id = "sample name"),
                by = "Name")
  }

  # Check for missing mouse IDs
  m <- match(out$mouse_id, annot$mouse_id)
  mm <- match(annot$mouse_id, out$mouse_id)
  if(any(is.na(m)) | any(is.na(mm)))
    stop(paste("missing mouse ids: sampleKey",
               paste(out$mouse_id[is.na(m)], collapse = ", "),
               " annot",
               paste(annot$mouse_id[is.na(mm)], collapse = ", ")))

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
    mutate(animal = as.character(animal)) %>%
    select(strain, sex, animal, condition, trait, value)
}
