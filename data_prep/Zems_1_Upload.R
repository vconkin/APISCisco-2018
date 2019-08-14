##############################################################
##  
##  APIS Cisco (Lucke et al.) manuscript
##  
## Data files are from Lake Superior larval coregonid study, 
##  script modified from Brian O'Malley. 
## The following output tables will be created and saved in your 
##  project folder:
##    "All.Samples.csv" = merged file of all raw data files
## 
##############################################################

## CLEAR ENVIRONMENT ============================================

rm(list = ls(all.names=TRUE))


## LOAD PACKAGES ================================================

library(dplyr)         # manipulating data
library(data.table)    # for fread() - super fast load-in of data
library(tidyr)         # for replace_na()

## LOAD DATA ====================================================

## Notes:
##  Read in all the files you want and combine into one long merged file.
##  Use the loop to read in all the pooled files.
##  Your working directory is set to the project folder.

## Make a list of all the files in the folder for loop to read
datafiles <- list.files(path = "data/Superior_Files", pattern = "Superior.*csv", full.names = TRUE)

## Now use your list to read in each data file
summaryfile <- do.call(rbind, lapply(datafiles, function(i) {
  ## print filename in console
  print(i)
  
  ## load data and combine into data frame using do.call()
  fread(file = i)
}))


## CALCULATE SUBSAMPLE EXPANSION COEF. ==========================

all.samples <- summaryfile %>% 
  mutate(subSampleExpansionCoef = subSampleTotal / amountSubSampled) %>% 
  ## Select (keep) only columns with data
  select(subSampleID, amountSubSampled, subSampleTotal, subSampleExpansionCoef, species, organismCount, length)

## Make a data frame with the copepodite adult length threshold for each copepod genus
## Sources: https://www.glsc.usgs.gov/greatlakescopepods/ & Conway 1977	
copepodite.length <- tibble(species = c("Acanthocyclops", "Diacyclops", "Epischura", "Leptodiaptomus", "Limnocalanus", "Mesocyclops"),
                            order = c("Cyclopoid copepodid", "Cyclopoid copepodid", "Calanoid copepodid", "Calanoid copepodid", "Calanoid copepodid", "Cyclopoid copepodid"),
                            length.threshold = c(1.26, 0.8, 1.6, 0.9, 2.2825, 0.8425))

## Join length threshold and rename genus based on length
all.samples.copepodite <- left_join(all.samples, copepodite.length) %>% 
  mutate(copep.diff = replace_na(length.threshold - length, -0.1),  ## use replace_na to maintain all non copepod organisms
         copep.logical = ifelse(copep.diff >= 0, order, species)) %>% 
  select(subSampleID, amountSubSampled, subSampleTotal, subSampleExpansionCoef, species = copep.logical, organismCount, length)


## SAVE MERGED FILE =============================================

write.csv(all.samples.copepodite, "data/Superior_Files/Summaries/All_Samples.csv", row.names = FALSE)

## ---------------------------END--------------------------------
