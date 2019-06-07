##############################################################
##############################################################
##  
##  APIS Cisco (Lucke et al.) manuscript
##  
## Data files are from Lake Superior larval coregonid study, script modified from Brian O'Malley. 
## The following output tables will be created and dumped in your working directory folder:
## 
## "Bytho.Samples.csv" = merged file of all raw data files (Step 1)
## This is a modified version of Zems_1, dealing only with the Bythotrephes data
## Since Bythotrephes were counted across the whole sample rather than in a subset,
## they have their own upload process that results in different calculations.
##############################################################
##############################################################
## ===========================================================
## Clear the environment first ===============================
## ===========================================================
rm(list = ls(all = TRUE))


## ===========================================================
## Load Packages =============================================
## ===========================================================
library(dplyr)         # manipulating data
library(data.table)    # for fread() - super fast load-in of data

## ===========================================================
## Step 1: Load in the data ==================================
## ===========================================================
## -----------------------------------------------------------
## Notes:
## Read in all the files you want and combine into one long merged file.
## Use the loop to read in all the pooled files.
## Make sure your directory is set to the lowest folder in the path with the files.
## -----------------------------------------------------------

## -----------------------------------------------------------
## Make a list of all the files in the folder for loop to read
## -----------------------------------------------------------
datafiles <- list.files(path = "data/Superior_Files/Bytho", pattern = ".csv", full.names = TRUE)

## -----------------------------------------------------------
## Now use your list to read in each data file
## -----------------------------------------------------------
summaryfile <- do.call(rbind, lapply(datafiles, function(i) {
  ## print filename in console
  print(i)
  
  ## load data and combine into data frame using do.call()
  fread(file = i)
}))


## ===========================================================
# Step 2: Fill in columns for subsample expansion coefficient.
## ===========================================================
bytho.samples <- summaryfile %>% 
  mutate(subSampleExpansionCoef = subSampleTotal / amountSubSampled) %>% 
  ## Select (keep) only columns with data
  select(subSampleID, amountSubSampled, subSampleTotal, subSampleExpansionCoef, species, organismCount, length)


## ===========================================================
## Step 3: Save merged file to working dir.
## ===========================================================
write.csv(bytho.samples, "data/Superior_Files/Summaries/Bytho_Samples.csv", row.names = FALSE)

#----------------------------------------END----------------------------------------
