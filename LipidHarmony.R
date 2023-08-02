library(tidyr)
library(readxl)
library(tidyverse)
library(foundr)
library(dplyr)
source("R/Lipid.R")

filename<-"Z:\\General\\founder_diet_study\\Primary data from Mark\\HollandW AtkinsonD 20230327 founder LIVER LIPIDS.xlsx"

sampleKey<- read_excel("C:/Users/ADMIN/Documents/GitHub/FounderDietStudy/data/RawData/192 livers sample key.xlsx")

annot <- read_excel("Z:\\General\\founder_diet_study\\Primary data from Mark\\mouse annotations for founder diet study.xlsx") %>%
                    mutate(diet = ifelse(as.character(diet_no) == "200339", "HC_LF", "HF_LC"))
rawdir <- ""

#Sheet 1
LiverLipHarmony(dataset = filename,
                annot = annot,
                #links = links,
                sheetNum = 1  ,
                skipNum = 1,
                minCol=2,
                maxCol=5,
                charCols=c("mouse_id", "Total Protein (µg)", "Tissue Mass (mg)", "strain", "animal", "sex", "condition","Name"),
                sampleKey=sampleKey)

#Sheet 2
LiverLipHarmony(dataset = filename,
                annot = annot,
                #links = links,
                sheetNum = 2  ,
                skipNum = 1,
                minCol=2,
                maxCol=5,
                charCols=c("mouse_id", "Total Protein (µg)", "Tissue Mass (mg)", "strain", "animal", "sex", "condition","Name"),
                sampleKey=sampleKey)

#Sheet 3
LiverLipHarmony(dataset = filename,
                annot = annot,
                #links = links,
                sheetNum = 3  ,
                skipNum = 0,
                minCol=2,
                maxCol=14,
                charCols=c("mouse_id", "Total Protein (µg)", "Tissue Mass (mg)", "strain", "animal", "sex", "condition")
)
