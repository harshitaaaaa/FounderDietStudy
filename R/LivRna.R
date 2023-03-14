LivRnaHarmony <- function(dataset, links, annot, liverAnnot, ...) {
  filename <- linkpath(dataset, links)
  
  # Data are in sheet 1 starting on line 5.
  out <- left_join(
    left_join(
      read_csv(filename) %>%
        
        # The `ENTREZID` uniquely identifies genome region (may be multiple per gene).
        # See `liverAnnot` below.
        rename(ENTREZID = "...1") %>%
        mutate(ENTREZID = as.character(ENTREZID)) %>%
        
        # Traits begin in column 2.
        pivot_longer(-1, names_to = "strain_number", values_to = "value") %>%
        
        # Separate `strain_number` into `strain` and `number` (`animal`).
        separate_wider_delim(strain_number, delim = "_",
                             names = c("strain","number")) %>%
        
        # Fix `strain` name for `129`, which begins with the number `1`.
        mutate(strain = ifelse(strain == "A129", "129", strain)),
      
      # Annotation file `annot` relates `animal` (`number`) to `sex` and `condition` (`diet`).
      annot %>%
        mutate(number = as.character(number)),
      by = c("strain", "number")),
    
    # liverAnnot relates `ENTREZID` to gene `SYMBOL`
    liverAnnot[,-1] %>%
      mutate(ENTREZID = as.character(ENTREZID)),
    by = "ENTREZID") %>%
    
    # Filter out minimum value, which is stand-in for missing value.
    filter(value > min(value)) %>% 
    
    # Rename columns to harmonize.
    rename(animal = "number",
           condition = "diet",
           trait = "SYMBOL") %>%
    
    # Combine `trait` (`SYMBOL`) and `ENTREZID`.
    mutate(trait = paste(trait, ENTREZID, sep = "_")) %>%
    
    # These are harmonized columns and their names.
    select(strain, sex, animal, condition, trait, value)
  
  # Find genes with duplicated symbols.
  dupGenes <-
    (out %>%
       distinct(trait) %>%
       separate_wider_delim(trait, "_", names = c("symbol", "entrezid")) %>%
       mutate(dup = duplicated(symbol)) %>%
       filter(dup))$symbol
  
  # Change `trait` back to gene `SYMBOL` for unique gene entries.
  out <- dplyr::mutate(
    out,
    trait = ifelse(str_remove(trait, "_.*") %in% dupGenes,
                   trait, str_remove(trait, "_.*")))
  
  out
}