rm(list=ls(all=TRUE)) #wipes your R workspace clean.

# Data files are from Lake Superior larval coregonid study, script modified from Brian O'Malley. 
# The following output tables will be created 
# and dumped in your working directory folder:

# "All.Samples.csv" = merged file of all raw data files (Step 1)


#-----------------------------------------------------------------------------------# 
# Step 1: First read in all the files you want and rename to a shortened version of
# file name, you will need to combine all .csv sample files into one long merged file
# use this loop (courtesy of Alison) to read in all the pooled files
# make sure your directory is set to the lowest folder in the path with the files
## Use the same path for Zems 1-3
setwd("C:/Users/vsluc/Documents/UVM/Data Analysis/Zems")
# Make a list of all the files in the folder
datafiles <- dir(path = ".", pattern="Superior.*csv")

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

All.Samples <- subset(summaryfile, select = c(subSampleID,
                                              amountSubSampled, 
                                              subSampleTotal,
                                              subSampleExpansionCoef,
                                              species,
                                              organismCount,
                                              length))
write.csv(All.Samples, "All.Samples.csv", row.names=FALSE)

#----------------------------------------END----------------------------------------