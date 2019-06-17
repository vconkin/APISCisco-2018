##############################################################
##############################################################
##  
##  APIS Cisco (Lucke et al.) manuscript
##  
## Note that aside from needing data files "All_Samples.csv" to execute this code, 
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
library(readxl)        # reading Excel data
library(dplyr)         # manipulating data
library(magrittr)      # for %<>% - doble pipe function, reads and saves from/to same object
library(tidyr)         # transforming data arrangement (tidy data!!)
library(ggplot2)       # visualizations


## ===========================================================
## Load in the data ==========================================
## ===========================================================
## Zooplankton samples
all.samples <- read.csv("data/Superior_Files/Summaries/All_Samples.csv", header = TRUE) %>% 
  filter(!is.na(species))
bytho.samples <- read.csv("data/Superior_Files/Summaries/Bytho_Samples.csv", header = TRUE)

## Load fish and zooplankton effort data files
## These files are trimmed down to include only neccessary variables
effort <- read.csv("data/Superior_Files/APIS_Effort_Cut.csv", header = TRUE)
zoop <- read.csv("data/Superior_Files/Zoop_Effort_Cut.csv", header = TRUE)


## ===========================================================
## Join the length and effort data =========================== 
## ===========================================================
## -----------------------------------------------------------
## Zooplankton
## -----------------------------------------------------------
## samples with trawl effort
zoop.length <- left_join(all.samples, effort, by = c("subSampleID" = "SerialIn")) %>% 
  rename(serial = subSampleID)

## trawl effort samples with zoop effort
zoop.length <- left_join(zoop.length, zoop) %>% 
  rename(trawl = Trawl)

## -----------------------------------------------------------
## Bythotrephes
## -----------------------------------------------------------
bytho.length <- left_join(bytho.samples, effort, by = c("subSampleID" = "SerialIn")) %>% 
  rename(serial = subSampleID)

bytho.length <- left_join(bytho.length, zoop) %>% 
  rename(trawl = Trawl)

## -----------------------------------------------------------
## Combine zoop and bytho length data (ignore coercing warning)
## -----------------------------------------------------------
all.lengths <- bind_rows(zoop.length, bytho.length) %>% 
  select(serial, trawl:Group, subSampleExpansionCoef:length, Volume_L)

## -----------------------------------------------------------
## Change date format into something R likes (YYYY-MM-DD)
## -----------------------------------------------------------
all.lengths %<>% mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y"))


## ===========================================================
## Summarize the zooplankton data and fill in zeros ==========
## ===========================================================
sample.long <- all.samples %>% group_by(subSampleID, species) %>%
  summarize(ex.coef = mean(subSampleExpansionCoef), 
            total = sum(organismCount)) %>% 
  ## Create "matrix style" data frame
  spread(species, total)

## Convert NA to zero for all columns 
sample.long[is.na(sample.long)] <- 0

## Flip the data frame back to stacked column format
sample.long %<>% gather(species, total, Acanthocyclops:Nauplii) %>%
  select(serial = subSampleID, species, ex.coef, total)

## -----------------------------------------------------------
## Summarize the bytho data and fill in zeros ----------------
## -----------------------------------------------------------
## Only 10 tows were found with bythos present. 
## We need to create zeros for all trawls were a zooplankton tow was done (every day except July 24).
bytho.long <- full_join(bytho.samples, effort, by = c("subSampleID" = "SerialIn")) %>% 
  rename("Serial" = "subSampleID") %>% 
  filter(Date != "7/24/2018") %>% 
  mutate(species = "Bythotrephes")

## join with zooplankton effort and summarize by serial
bytho.long <- left_join(bytho.long, zoop) %>%
  group_by(Serial, species) %>%
  summarize(ex.coef = mean(subSampleExpansionCoef), 
            total = sum(organismCount)) %>%
  select(serial = Serial, species, ex.coef, total)

## Convert NA to zero for all columns 
bytho.long[is.na(bytho.long)] <- 0


## ===========================================================
## Join the sample and effort data =========================== 
## ===========================================================
## -----------------------------------------------------------
## Zooplankton
## -----------------------------------------------------------
## samples with trawl effort
zoop.trawl <- left_join(sample.long, effort, by = c("serial" = "SerialIn"))

## trawl effort samples with zoop effort
zoop.trawl <- left_join(zoop.trawl, zoop) %>% 
  rename(trawl = Trawl)

## -----------------------------------------------------------
## Bythotrephes
## -----------------------------------------------------------
bytho.trawl <- left_join(bytho.long, effort, by = c("serial" = "SerialIn"))

bytho.trawl <- left_join(bytho.trawl, zoop) %>% 
  rename(trawl = Trawl)

## -----------------------------------------------------------
## Combine zoop and bytho length data (ignore coercing warning)
## -----------------------------------------------------------
all.trawl <- bind_rows(zoop.trawl, bytho.trawl) %>% 
  select(serial:Group, Volume_L)


## ===========================================================
## Calculate Density =========================================
## ===========================================================
zoop.density <- all.trawl %>% group_by(trawl, species) %>%
  mutate(n.counted  = sum(total),
         n.jar   = n.counted * ex.coef,
         density.l   = n.jar / Volume_L)


## ===========================================================
## Calculate Individual Dry Weight ===========================
## ===========================================================
## -----------------------------------------------------------
## Notes:
## Calculate biomass per individual using L:W equations 
## This follows the formula "ln(W) = ln(a) + B * ln(L)" 
## where "W" equals dry mass in micrograms, "L" equals length in mm, 
## and "a" and "B" are coefficients called from a separate table.
## ## See Watkins et al. 2011 for suggestions on equations. 
## -----------------------------------------------------------

## -----------------------------------------------------------
## Load LW coefficeints
## -----------------------------------------------------------
lw <- read.csv("data/Superior_Files/LWtable_forHannah.csv", header = TRUE)

## -----------------------------------------------------------
## Join LW coefficents to resepctive species
## -----------------------------------------------------------
zoop.lw <- left_join(all.lengths, lw, by = "species")

## -----------------------------------------------------------
## Calculate individual dry weight using ln(length), ln(A), and B.
## -----------------------------------------------------------
zoop.lw %<>% mutate(indiv.dry.wt.ug = exp(lnA + B * log(length)))

## -----------------------------------------------------------
## Summarize by trawl and species
## -----------------------------------------------------------
sum.sample.species <- zoop.lw %>% group_by(trawl, species) %>%
  mutate(n.measured = length(!is.na(length))) %>%
  summarise(n.measured  = mean(n.measured),
            mean.indiv.dry.wt.ug   = mean(indiv.dry.wt.ug, na.rm = TRUE),
            sd.indiv.dry.wt.ug     = sd(indiv.dry.wt.ug, na.rm = TRUE),
            mean.indiv.length.mm  = mean(length, na.rm = TRUE),
            sd.indiv.length.mm   = sd(length, na.rm = TRUE))


## ===========================================================
## Combine biomass and density ===============================
## ===========================================================
zoop.summary <- left_join(sum.sample.species, zoop.density)


## ===========================================================
## Last data manipulation ====================================
## ===========================================================
## -----------------------------------------------------------
## This adds all observed species to each trawl so we can include 0s
## -----------------------------------------------------------
zoop.summary %<>% complete(trawl, species = unique(.$species)) 

## -----------------------------------------------------------
## Assign taxa group names to each species
## -----------------------------------------------------------
species.group <- zoop.lw %>% 
  distinct(species, Group.y) %>% 
  rename(taxa.group = Group.y)

## -----------------------------------------------------------
## Add taxa group names for each species for later binning if necessary, 
## and then selects subset of data that does not include effort data (which will be added later)
## -----------------------------------------------------------
zoop.summary <- left_join(zoop.summary, species.group) %>% 
  select(trawl, species, taxa.group, n.counted, n.jar, density.l, n.measured, mean.indiv.dry.wt.ug,
         sd.indiv.dry.wt.ug, mean.indiv.length.mm, sd.indiv.length.mm)

## -----------------------------------------------------------
## Add biomass to the data set
## -----------------------------------------------------------
zoop.summary %<>% mutate(biomass.dry.wt.ugL = density.l * mean.indiv.dry.wt.ug)

## -----------------------------------------------------------
## Add in the effort data
## -----------------------------------------------------------
zoop.summary <- left_join(zoop.summary, effort, by = c("trawl" = "Trawl")) %>% 
  select(trawl, serial = "SerialIn", date = "Date", week = "Week", j.day = "Jday", station = "Station", group = "Group", 
         species, taxa.group, n.counted, n.jar, density.l, n.measured, mean.indiv.dry.wt.ug, 
         sd.indiv.dry.wt.ug, mean.indiv.length.mm, sd.indiv.length.mm, biomass.dry.wt.ugL)

## -----------------------------------------------------------
## Add in 0s for taxa that were not found in each sample
## Lengths/weights and s.d. were left blank intentionally - a species 
##  missing from a sample does not have a length or s.d. of 0
## -----------------------------------------------------------
zoop.summary %<>% mutate(n.counted = replace_na(n.counted, 0),
                         n.jar = replace_na(n.jar, 0),
                         density.l = replace_na(density.l, 0),
                         n.measured = replace_na(n.measured, 0),
                         biomass.dry.wt.ugL = replace_na(biomass.dry.wt.ugL, 0))


## ===========================================================
## Save summary file
## ===========================================================
write.csv(zoop.summary, "data/Superior_Files/Summaries/Zoop_Summary.csv", row.names = F)

#----------------------------------------END----------------------------------------
