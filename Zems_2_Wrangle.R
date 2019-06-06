##############################################################
##############################################################
##  
##  APIS Cisco (Lucke et al.) manuscript
##  
## Note that aside from needing data files "All.Samples.csv" to execute this code, 
##  you need the following csv file in your working directory:
## "LWtable.csv" , and "Bytho.Samples", as well as the summary csv for the 
## larval fish tows and the zoop tows, "Effort" and "Zoop"
##
##  The following output tables will be created and dumped in your working 
## directory folder:
## 
## "TotalZoopTable.csv" = list of total ZP density & biomass by sampleID (Step ##)
## "ZTrawl.csv" = Basic summary of trawl and species data
## "ZDensity.csv" = Built from ZTrawl, calculates density and other parameters
## "ZLength.csv" = Trawl info, and all lengths for every zoop that was measured
## "SumBySampleSpecies.csv" = #/L, ug/L, mean/sd lengths and weights by trawl
## "TotalZoopTable18.csv" = Total biomass and density by trawl across all species
## ===========================================================
## Clear the environment first ===============================
## ===========================================================
rm(list = ls(all.names=TRUE))


## ===========================================================
## Load Packages =============================================
## ===========================================================
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
    
    
## ===========================================================
## Load in the data ==========================================
## ===========================================================
All.Samples <- read.csv("data/Superior_Files/Summaries/All.Samples.csv", header = TRUE)
Bytho.Samples <- read.csv("data/Superior_Files/Summaries/Bytho.Samples.csv", header = TRUE)

## We'll need information about the fish and zooplankton efforts
## These files are trimmed down from the original to minimze extraneous
## data and columns we need to manage
Effort <- read.csv("data/Superior_Files/APIS_Cut.csv", header = TRUE)
Zoop <- read.csv("data/Superior_Files/Zoop_Cut.csv", header = TRUE)

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
All.Lengths$Date=as.Date(All.Lengths$Date,"%m/%d/%Y")

# We will do this a couple times to make sure all the files
# have the right format before they are written

#write.csv(All.Lengths, "data/Superior_Files/Summaries/ZLength.csv", row.names = FALSE)
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

#write.csv(ZTrawl, "data/Superior_Files/Summaries/ZTrawl.csv", row.names = FALSE)

#-------Calculate Density----------------------------------------------------
## Use ZTrawl to calculate density and other sample parameters
ZDensity<-select(ZTrawl, Trawl:Group, Serial:Total, Volume_L) %>%
  group_by(Trawl, species) %>%
  mutate(N_Counted  = sum(Total),
            N_Jar   = N_Counted*Ex.Coef,
            Density_L   = N_Jar/Volume_L)

#write.csv(ZDensity, "data/Superior_Files/Summaries/ZDensity.csv", row.names = FALSE)

#-------Calculate Biomass----------------------------------------------------
# Step 7a-e: Calculate biomass per individual using L:W equations 
# This follows the formula "ln(W)=ln(a)+B*ln(L)" See Watkins et al 2011 for suggestions on equations. 
# where "W" equals dry mass in micrograms, "L" equals length in mm, 
# and "a" and "B" are coefficients called from a separate table.

## Step 7a: Read table containing species names and LW coefficeints.
LW= read.csv("data/Superior_Files/LWtable_forHannah.csv", header=TRUE)


## Step 7b: Merge LW coefficents to resepctive species names for each row in the All.Lengths file. 
ZLW<- left_join(All.Lengths,LW,by="species")

## Step 7c: Add columns calculating biomass (ie ln_W) using length,ln(a),and B.
attach(ZLW)
#ZLW$ln_W= lnA + B * log(length)
ZLW$IndDWug= exp(lnA + B * log(length))
detach(ZLW)

#--------------Export Biomass----------------------------------------------------------
# Step 9: Export individual-based table, which has each L and W for each zoop measured.
#write.csv(ZLW, "data/Superior_Files/Summaries/LWTable.csv", row.names = FALSE)

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
  mutate(N_Measured = length(!is.na(length))) %>%
  summarise(N_Measured  = mean(N_Measured),
          Mn_IndDW_ug   = mean(IndDWug, na.rm = TRUE),
          SD_Mn_IndDW_ug     = sd(IndDWug, na.rm = TRUE),
          Mn_IndLength_mm  = mean(length, na.rm = TRUE),
          SD_MnLength_mm   = sd(length, na.rm = TRUE))



Zoop_Summary<-left_join(SumBySampleSpecies,ZDensity,by = c("Trawl","species")) 

#This adds all observed species to each trawl so we can include 0s
Zoop_Summary<-Zoop_Summary %>% complete(Trawl,species = unique(.$species)) 

#Assigns taxa group names to each species
Species_Group<-ZLW %>% 
  distinct(species,Group.y) %>% 
  rename(Taxa_Group = Group.y)

#Add taxa group names for each species for later binning if necessary, and then selects subset of
#data that does not include effort data (which will be added later)
Zoop_Summary<-left_join(Zoop_Summary,Species_Group) %>% 
  select(Trawl,-Serial,-Date,-Week,-Jday,-Station,-Group,species,Taxa_Group,N_Counted,N_Jar,Density_L,N_Measured,Mn_IndDW_ug,
         SD_Mn_IndDW_ug,Mn_IndLength_mm,SD_MnLength_mm)

#Add biomass to the data set
attach(Zoop_Summary)
Zoop_Summary$BiomassDW_ugL = Density_L*Mn_IndDW_ug
detach(Zoop_Summary)

#Adds in the effort data
Zoop_Summary <- left_join(Zoop_Summary,Effort, by = c("Trawl")) %>% 
  select(Trawl,SerialIn,Date,Week,Jday,Station,Group,species,Taxa_Group,N_Counted,N_Jar,Density_L,N_Measured,Mn_IndDW_ug,
         SD_Mn_IndDW_ug,Mn_IndLength_mm,SD_MnLength_mm,BiomassDW_ugL) %>% 
  rename("Serial" = "SerialIn")

#Add in 0s for taxa that were not found in each sample
Zoop_Summary$N_Counted[is.na(Zoop_Summary$N_Counted)] <- 0
Zoop_Summary$N_Jar[is.na(Zoop_Summary$N_Jar)] <- 0
Zoop_Summary$Density_L[is.na(Zoop_Summary$Density_L)]<-0
Zoop_Summary$N_Measured[is.na(Zoop_Summary$N_Measured)] <- 0
Zoop_Summary$BiomassDW_ugL[is.na(Zoop_Summary$BiomassDW_ugL)] <- 0



# Then we export it
write.csv(Zoop_Summary, "data/Superior_Files/Summaries/Zoop_Summary.csv", row.names=F, quote = FALSE)

#---------------------- By total ZP density/L & biomass ug DW/L--------------------- 
#TotalZoopTable= ddply(SumBySampleSpecies,.(Trawl), summarize,
                      TotalBiomass_ug_L = sum(BioDWugL),
                      TotalDensity_L = sum(Density_L),
                      .drop=F)

#write.csv(TotalZoopTable, "data/Superior_Files/Summaries/TotalZoopTable18.csv", row.names=F)
#----------------------------------------END----------------------------------------
