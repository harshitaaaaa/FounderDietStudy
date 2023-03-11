MetHarmony <- function(dataset, links, skiprows = 4, skipcols = 4, ...) {
  filename <- linkpath(dataset, links)
  
  # Data are in sheet 1 starting on line 5.
  plasmaData <- read_excel(filename, sheet = 1, skip = skiprows) %>%
    
    # Pivot traits, which begin in column 5.
    pivot_longer(-seq_len(skipcols), names_to = "mousecode", values_to = "value") %>%
    
    # Decode `strain`, `animal`, `sex` and `condition` from `mousecode`.
    mutate(
      strain = str_remove(str_extract(mousecode, "^.*-"), "-"),
      animal = str_extract(str_remove(mousecode, "^.*-"), "^[0-9]+"),
      sex = str_extract(str_remove(mousecode, "^.*-[0-9]+_"), "^[MF]"),
      condition = str_remove(str_remove(mousecode, "^.*-[0-9]+_"), "^[MF]_")) %>%
    
    # Rename `compound` as `trait`.
    rename(trait = "compound") %>%
    
    # These are harmonized columns and their names.
    select(strain, sex, animal, condition, trait, value) %>%
    
    # Normal scores by trait
    group_by(trait) %>%
    mutate(value = foundr::nqrank(value, jitter = TRUE)) %>%
    ungroup()
}