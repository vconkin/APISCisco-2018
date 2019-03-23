rm(list=ls(all=TRUE)) #wipes your R workspace clean.

## Note that aside from needing data files "All.Samples.csv" to execute this code, 
##  you need the following csv file in your working directory:
## "LWtable.csv" , and "Bytho.Samples", as well as the summary csv for the 
## larval fish tows and the zoop tows, "Effort" and "Zoop"

##  The following output tables will be created and dumped in your working 
#directory folder:

## "TotalZoopTable.csv" = list of total ZP density & biomass by sampleID (Step ##)
## "ZTrawl.csv" = Basic summary of trawl and species data
## "ZDenstity.csv" = Built from ZTrawl, calculates density and other parameters
## "ZLength.csv" = Trawl info, and all lengths for every zoop that was measured
## "SumBySampleSpecies.csv" = #/L, ug/L, mean/sd lengths and weights by trawl
## "TotalZoopTable18.csv" = Total biomass and density by trawl across all species

#--------------Read & Merge------------------------------------------------
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
        
## Read in that merged file created in  part 1
All.Samples=read.csv("All.Samples.csv", header=TRUE)
Bytho.Samples=read.csv("Bytho/Bytho.Samples.csv", header=TRUE)

## We'll need information about the fish and zooplankton efforts
## These files are trimmed down from the original to minimze extraneous
## data and columns we need to manage
Effort<-read.csv("APIS_Cut.csv", header=TRUE)
Zoop<-read.csv("Zoop_Cut.csv", header=TRUE)

#--------------Summarize all length data---------------------------------------
# First we compile the length data
# ZLength contains all the individual length data for the zoops counted
# Use left_join to bring in all the trawl numbers from APIS, then link volume 
# filtered to the Zems data using Trawl number, then reorder the columns
# We only need to retain date/trawl info, and the values needed to calculate density

ZLength<-left_join(All.Samples, Effort, by=c("subSampleID" = "SerialIn")) %>% 
  rename("Serial" = "subSampleID")
ZLength<-left_join(ZLength, Zoop, by="Trawl")

# Separately join Bythotrephes to the effort and zoop data
Bytho_L<-left_join(Bytho.Samples, Effort, by=c("subSampleID" = "SerialIn")) %>% 
  rename("Serial" = "subSampleID")
Bytho_L<-left_join(Bytho_L, Zoop, by="Trawl")

# Bind the length data together and trim to to the appropriate size
# This will coerce a number of variables to character vectors, don't worry
All.Lengths<-bind_rows(ZLength, Bytho_L) %>% 
  select(Serial, Trawl:Group,subSampleExpansionCoef:length, Volume_L)

#-----------Date Format----------------------------------------------------------------
# Step 8: Reformat date to All.Lengths file
# R is pretty picky about date format.
# It recognizes excel default date format as factor and not Date.
# Need to change the date foramt to YYYY-MM-DD using:
strDates <- c(levels(All.Lengths$Date))
dates <- as.Date(strDates, "%m/%d/%Y")
x=as.Date(All.Lengths$Date,"%m/%d/%Y")
All.Lengths$Date=x

# We will do this a couple times to make sure all the files
# have the right format before they are written

write.csv(All.Lengths, "ZLength.csv", row.names = FALSE)
#--------------Combine the Zooplankton Data---------------------------------
# Since this data set is built only of observed species, we want to add in zeros
# for any species not observed to fill out our time series
# We accomplish this by the spread() function from dplyr, which will transpose
# the genera in the species column into their own columns
sample_long<-group_by(All.Samples, subSampleID, species) %>%
  summarize(Ex.Coef= mean(subSampleExpansionCoef), Total = sum(organismCount)) %>% 
  spread(species, Total)

## Convert NA to zero for all columns 
sample_long[is.na(sample_long)] <- 0
# And next we can flip the dataframe back so each genus is a factor within
# the species column

Sample_mod<-gather(sample_long, species, Total, Acanthocyclops:Nauplii) %>%
  select(subSampleID, Ex.Coef, species, Total)

## We have to add in zeros for Bythotrephes, since there are less than 10
## trawls with Bythotrephes present
## NOTE: Full join requires we take out any samples that have a trawl/serial
## but no zoop tow (July 24), so we remove that row
B_Long<-full_join(Bytho.Samples, Effort, by=c("subSampleID" = "SerialIn")) %>% 
  rename("Serial" = "subSampleID")
B_Long<-B_Long[-133,]

B_Long<-left_join(B_Long, Zoop, by="Trawl")%>%
  group_by(Serial, species) %>%
  summarize(Ex.Coef= mean(subSampleExpansionCoef), Total = sum(organismCount)) %>% 
  spread(species, Total)

## Convert NA to zero for all columns 
B_Long[is.na(B_Long)] <- 0
# And next we can flip the dataframe back keeping only Bythotrephes
B_mod<-gather(B_Long, species, Total, Bythotrephes) %>%
  select(Serial,  Ex.Coef, species, Total)

# Again, we use left join here to bring in all the info from the effort and 
# trawl sheets.
ztrawls<-left_join(Sample_mod, Effort, by = c("subSampleID" = "SerialIn")) %>%
  rename("Serial"="subSampleID") 
ztrawls<- left_join(ztrawls, Zoop,by="Trawl") 
ztrawls$species<-as.factor(ztrawls$species)  

B_mod<-left_join(B_mod, Effort, by = c("Serial" = "SerialIn"))
B_mod<- left_join(B_mod, Zoop,by="Trawl") 
B_mod$species<-as.factor(B_mod$species)  

# Then we join the two dataframes together
ZTrawl<-bind_rows(ztrawls, B_mod) %>% 
  select(Serial:Group, Volume_L)

write.csv(ZTrawl, "ZTrawl.csv", row.names = FALSE)

#-------Calculate Density----------------------------------------------------
## Use ZTrawl to calculate density and other sample parameters
ZDensity<-select(ZTrawl, Trawl:Group, Serial:Total, Volume_L) %>%
  group_by(Trawl, species) %>%
  mutate(N.Measured  = sum(Total),
            N.Per.Jar   = sum(Total)*(Ex.Coef),
            Density_L   = sum((Total)*(Ex.Coef/Volume_L)))

write.csv(ZDensity, "ZDensity.csv", row.names = FALSE)

#-------Calculate Biomass----------------------------------------------------
# Step 7a-e: Calculate biomass per individual using L:W equations 
# This follows the formula "ln(W)=ln(a)+B*ln(L)" See Watkins et al 2011 for suggestions on equations. 
# where "W" equals dry mass in micrograms, "L" equals length in mm, 
# and "a" and "B" are coefficients called from a separate table.

## Step 7a: Read table containing species names and LW coefficeints.
LW= read.csv("LWtable_forHannah.csv", header=TRUE)


## Step 7b: Merge LW coefficents to resepctive species names for each row in the All.Lengths file. 
ZLW<- left_join(All.Lengths,LW,by="species")

## Step 7c: Add columns calculating biomass (ie ln_W) using length,ln(a),and B.
attach(ZLW)
ZLW$ln_W= lnA + B * log(length)
ZLW$IndDWug= exp(ZLW$ln_W)
detach(ZLW)

#--------------Export Biomass----------------------------------------------------------
# Step 9: Export individual-based table, which has each L and W for each zoop measured.
write.csv(ZLW, "LWTable.csv", row.names = FALSE)

#---------Summarize by Species----------------------------------------------
# Step 10: Summarize count and biomass data per sample  
# Use the dplyr package to summarize data by species for each trawl
# This creates a separate table called SumBySampleSpecies 

# Note that the number of animals per Liter is described by the
# formula: density_L = animals collected / water volume filtered. 
# Make sure you first install the 'plyr' package first.

#change species to a factor
ZLW$species<-as.factor(ZLW$species)

#group by trawl and species to summarize species in each sample
#then remove any species listed as NA (where none were found in the sample)
#and finally create the summary variables
SumBySampleSpecies <- group_by(ZLW, Trawl,species) %>%
  filter(!is.na(species)) %>%
  summarise(Exp.Coeff   = mean(subSampleExpansionCoef),
          N.Measured  = sum(organismCount),
          N.Per.Jar   = sum(organismCount)*mean(subSampleExpansionCoef),
          Density_L   = sum(organismCount)*mean(subSampleExpansionCoef)/mean(Volume_L),          
          BioDWugL    = mean(IndDWug, na.rm = TRUE)*Density_L,
          mnIndivDW   = mean(IndDWug, na.rm = TRUE),
          sd_mnDW     = sd(IndDWug, na.rm = TRUE),
          mnIndvLength  = mean(length, na.rm = TRUE),
          sd_mnLength   = sd(length, na.rm = TRUE))

# Since many of the nauplii were counted but not measured, all the biomass
# calculations need to use the mean individual length and exclude and NAs 
# in the data (for the ones with no measured length). If there are NAs left
# in these rows, R will not properly calculate these values

# Then we export it
write.csv(SumBySampleSpecies, "SumBySampleSpecies.csv", row.names=F)

#---------------------- By total ZP density/L & biomass ug DW/L--------------------- 
TotalZoopTable= ddply(SumBySampleSpecies,.(Trawl), summarize,
                      TotalBiomass_ug_L = sum(BioDWugL),
                      TotalDensity_L = sum(Density_L),
                      .drop=F)

write.csv(TotalZoopTable, "TotalZoopTable18.csv", row.names=F)
#----------------------------------------END----------------------------------------