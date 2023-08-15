
#' Read Metabolite File for Harmonization
#'
#' There are  minor differences among metabolite files.
#' To harmonize, we assume the following:
#'    Some number of rows (3-4) at top of file are skipped
#'    Columns from “compound” to “data_type” precede mouse data
#'    Mouse ID always begins with FFF-nn with “FFF” = founder, “nn” = animal
#'    (the rest of Mouse ID seems to change from file to file)
#'    Trait names with " (n)" (n = 1,2,3) at the end have this removed.

#' @param dataset name of dataset
#' @param links dataframe of links to data files
#' @param annot annotation data frame
#' @param skiprows number of rows to skip
#' @param ... ignore
#'
#' @return data frame with columns for strain, sex, animal, condition, trait, value
#' @export
#'
MetHarmony <- function(dataset, links, annot, skiprows = 4, ...) {
  filename <- linkpath(dataset, links)

  # Join with `annot` to get strain, number, sex, diet
  dplyr::left_join(

    # Data are in sheet 1 starting on line 5.
    read_excel(filename, sheet = 1, skip = skiprows) %>%

      # Pivot traits, which begin in column 5.
      pivot_longer(-(compound:data_type), names_to = "mouse_id", values_to = "value") %>%
      mutate(Timepoints = sub(".*_(\\d+)min.*", "\\1", mouse_id))%>%

      # Decode `strain`, `animal`, `sex` and `condition` from `mousecode`.
      mutate(mouse_id = str_remove(mouse_id, "_.*"))
    ,

    # Annotation
    annot,
    by = "mouse_id") %>%

    # Rename `compound` as `trait`, number as `animal`
    rename(
      trait = "compound",
      animal = "number",
      condition = "diet") %>%
    mutate(
      # Make sure animal is character.
      animal = as.character(animal)) %>%
    #      # Remove trailing (1) appearing on some trait names.
    #      trait = #str_to_title(
    #        str_remove(trait, " \\([0-9]\\)$"))) %>%

    # These are harmonized columns and their names.
    select(strain, sex, animal, condition,Timepoints,trait, value)
}

