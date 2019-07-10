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


## SAVE MERGED FILE =============================================

write.csv(all.samples, "data/Superior_Files/Summaries/All_Samples.csv", row.names = FALSE)

## ---------------------------END--------------------------------
