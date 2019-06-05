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
## 
## ===========================================================
# Make a list of all the files in the folder
datafiles <- dir(path = "data/Superior_Files/Bytho", pattern="*.csv", full.names = TRUE)

# Make an empty vector where you can put the names of each file
filelist <- rep(NA, length(datafiles))

# Now use your list to read in each data file
for (i in 1:length(datafiles)){
  
  # Save the name of the filepath so you can reference it later 
  filename <- paste(datafiles[i])
  
  # Also save the filename without the .csv extension
  dataframename <- substr(paste(datafiles[i]), 1, nchar(paste(datafiles[i]))-4)
  
  # Record the name of the data file 
  filelist[i] <- dataframename
  
  # Read in data 
  freqdata <- read.table(filename, sep=",", header=TRUE)
  
  # Add columns at the end with lake name (which is in the filename)
  #tempstring <- unlist(strsplit(dataframename, split="_", fixed = FALSE, perl = FALSE, useBytes = FALSE))
  #freqdata$Lake <- tempstring[2]
  
  # Assign the data that you have pulled out to the filename where you got it
  assign(dataframename,freqdata)
  
  if(i==1){ # If you're looking at the first raw data file, start a new data frame 
    # to store all of your data
    summaryfile <- freqdata
  } else{ # Or else add to the data frame you already started
    summaryfile <- rbind(summaryfile,freqdata)
  }
}


#-----------------------------------------------------------------------------------# 
# Step 2: Fill in column for subsample expansion coefficient. Keep only columns with 
# data stored in them and get rid of blank/unused columns to clean up a bit. Then export 
# merged file to working dir.


summaryfile$subSampleExpansionCoef= summaryfile$subSampleTotal / summaryfile$amountSubSampled

Bytho.Samples <- subset(summaryfile, select = c(subSampleID,
                                              amountSubSampled, 
                                              subSampleTotal,
                                              subSampleExpansionCoef,
                                              species,
                                              organismCount,
                                              length))
write.csv(Bytho.Samples, "data/Superior_Files/Summaries/Bytho.Samples.csv", row.names = FALSE)

