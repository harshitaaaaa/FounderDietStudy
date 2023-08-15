
# Pull dataset names from `source.csv`
dataset <- read.csv(file.path("data/RawData", "source.csv"))
dataset <- dataset[dataset$longname != "", c(1,3)]
rownames(dataset) <- NULL
datasets <- dataset$longname
names(datasets) <- dataset$shortname
saveRDS(datasets, "big/datasets.rds")

rmarkdown::render("help.Rmd", rmarkdown::md_document(),  output_file = "help.md")
