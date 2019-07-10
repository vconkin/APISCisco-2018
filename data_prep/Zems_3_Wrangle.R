#################################################################
##  
##  APIS Cisco (Lucke et al.) manuscript
##  
##  
##  This script combines the zooplankton and bythos files to 
##    calculate biomass and density.
##  
##  You need the following files in your project folder:
##    "All_Samples.csv"
##    "Bytho_Samples.csv"
##    "APIS_Coregonus_2018.xlsx" 
##
##  The following output tables will be created and save in your 
##    project folder:
##      "Zoop_Summary.csv" = Total biomass and density by trawl across all species
##      
#################################################################

## CLEAR ENVIRONMENT ============================================

rm(list = ls(all.names=TRUE))


## LOAD PACKAGES ================================================

library(readxl)        # reading Excel data
library(dplyr)         # manipulating data
library(magrittr)      # for %<>% - doble pipe function, reads and saves from/to same object
library(tidyr)         # transforming data arrangement (tidy data!!)
library(ggplot2)       # visualizations


## LOAD DATA ====================================================

## Zooplankton samples
all.samples <- read.csv("data/Superior_Files/Summaries/All_Samples.csv", header = TRUE) %>% 
  filter(!is.na(species))
bytho.samples <- read.csv("data/Superior_Files/Summaries/Bytho_Samples.csv", header = TRUE)

## Load fish and zooplankton effort data files
##  These files are trimmed down to include only neccessary variables
effort <- read_excel("data/APIS_Coregonus_2018.xlsx", sheet = "Neuston Effort") %>% 
  select(trawl, date, week, j.day, station, group, serial.in, serial.out)
zoop <- read_excel("data/APIS_Coregonus_2018.xlsx", sheet = "Zoop Effort") %>% 
  select(trawl, volume.l)


## JOIN LENGTH AND EFFORT DATA ============================== 

## Zooplankton
## samples with trawl effort
zoop.length <- left_join(all.samples, effort, by = c("subSampleID" = "serial.in")) %>% 
  rename(serial = subSampleID)

## trawl effort samples with zoop effort
zoop.length <- left_join(zoop.length, zoop)

## Bythotrephes
## samples with trawl effort
bytho.length <- left_join(bytho.samples, effort, by = c("subSampleID" = "serial.in")) %>% 
  rename(serial = subSampleID)

## trawl effort samples with zoop effort
bytho.length <- left_join(bytho.length, zoop)

## Combine zoop and bytho length data (ignore coercing warning)
all.lengths <- bind_rows(zoop.length, bytho.length) %>% 
  select(serial, trawl:group, subSampleExpansionCoef:length, volume.l)

## Change date format into something R likes (YYYY-MM-DD)
all.lengths %<>% mutate(date = as.POSIXct(date, format = "%m/%d/%Y"))


## DATA MANIPULATION ============================================

## Summarize the zooplankton data and fill in zeros
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

## Summarize the bytho data and fill in zeros
##  Only 10 tows were found with bythos present. 
##  We need to create zeros for all trawls were a zooplankton tow 
##  was done (every day except July 24).
bytho.long <- full_join(bytho.samples, effort, by = c("subSampleID" = "serial.in")) %>% 
  rename(serial = subSampleID) %>% 
  filter(date != "2018-07-24") %>% 
  mutate(species = "Bythotrephes")

## Join with zooplankton effort and summarize by serial
bytho.long <- left_join(bytho.long, zoop) %>%
  group_by(serial, species) %>%
  summarize(ex.coef = mean(subSampleExpansionCoef), 
            total = sum(organismCount)) %>%
  select(serial, species, ex.coef, total)

## Convert NA to zero for all columns 
bytho.long[is.na(bytho.long)] <- 0


## JOIN SAMPLE AND EFFORT DATA ================================== 

## Zooplankton
## samples with trawl effort
zoop.trawl <- left_join(sample.long, effort, by = c("serial" = "serial.in"))

## trawl effort samples with zoop effort
zoop.trawl <- left_join(zoop.trawl, zoop)

## Bythotrephes
## samples with trawl effort
bytho.trawl <- left_join(bytho.long, effort, by = c("serial" = "serial.in"))

## trawl effort samples with zoop effort
bytho.trawl <- left_join(bytho.trawl, zoop)

## Combine zoop and bytho length data (ignore coercing warning)
all.trawl <- bind_rows(zoop.trawl, bytho.trawl) %>% 
  select(serial:group, volume.l)


## CALCULATE DENSITY ============================================

zoop.density <- all.trawl %>% group_by(trawl, species) %>%
  mutate(n.counted  = sum(total),
         n.jar   = n.counted * ex.coef,
         density.l   = n.jar / volume.l)


## CALCULATE INDIVIDUAL DRY WEIGHT ==============================

## Notes:
##  Calculate biomass per individual using L:W equations 
##  This follows the formula "ln(W) = ln(a) + B * ln(L)" 
##  where "W" equals dry mass in micrograms, "L" equals length in mm, 
##  and "a" and "B" are coefficients called from a separate table.
##  See Watkins et al. 2011 for suggestions on equations. 

## Load LW coefficeints
lw <- read.csv("data/Superior_Files/LWtable_forHannah.csv", header = TRUE)

## Join LW coefficents to resepctive species
zoop.lw <- left_join(all.lengths, lw, by = "species")

## Calculate individual dry weight using ln(length), ln(A), and B.
zoop.lw %<>% mutate(indiv.dry.wt.ug = exp(lnA + B * log(length)))

## Summarize by trawl and species
sum.sample.species <- zoop.lw %>% group_by(trawl, species) %>%
  mutate(n.measured = length(!is.na(length))) %>%
  summarise(n.measured  = mean(n.measured),
            mean.indiv.dry.wt.ug   = mean(indiv.dry.wt.ug, na.rm = TRUE),
            sd.indiv.dry.wt.ug     = sd(indiv.dry.wt.ug, na.rm = TRUE),
            mean.indiv.length.mm  = mean(length, na.rm = TRUE),
            sd.indiv.length.mm   = sd(length, na.rm = TRUE))


## COMBINE BIOMASS AND DENSITY ==================================
zoop.summary <- left_join(sum.sample.species, zoop.density)


## LAST DATA MANIPULATION =======================================

## This adds all observed species to each trawl so we can include 0s
zoop.summary %<>% complete(trawl, species = unique(.$species)) 

## Assign taxa group names to each species
species.group <- zoop.lw %>% 
  distinct(species, Group) %>% 
  rename(taxa.group = Group)

## Add taxa group names for each species for later binning if necessary, 
##  and then selects subset of data that does not include effort data (which will be added later)
zoop.summary <- left_join(zoop.summary, species.group) %>% 
  select(trawl, species, taxa.group, n.counted, n.jar, density.l, n.measured, mean.indiv.dry.wt.ug,
         sd.indiv.dry.wt.ug, mean.indiv.length.mm, sd.indiv.length.mm)

## Add biomass to the data set
zoop.summary %<>% mutate(biomass.dry.wt.ugL = density.l * mean.indiv.dry.wt.ug)

## Add in the effort data
zoop.summary <- left_join(zoop.summary, effort) %>% 
  select(trawl, serial = serial.in, date, week, j.day, station, group, species, taxa.group, 
         n.counted, n.jar, density.l, n.measured, mean.indiv.dry.wt.ug, 
         sd.indiv.dry.wt.ug, mean.indiv.length.mm, sd.indiv.length.mm, biomass.dry.wt.ugL)

## Add in 0s for taxa that were not found in each sample
## Lengths/weights and s.d. were left blank intentionally - a species 
##  missing from a sample does not have a length or s.d. of 0
zoop.summary %<>% mutate(n.counted = replace_na(n.counted, 0),
                         n.jar = replace_na(n.jar, 0),
                         density.l = replace_na(density.l, 0),
                         n.measured = replace_na(n.measured, 0),
                         biomass.dry.wt.ugL = replace_na(biomass.dry.wt.ugL, 0))


## SAVE SUMMARY FILE ============================================

write.csv(zoop.summary, "data/Superior_Files/Summaries/APIS_Zoop_Summary.csv", row.names = F)

## ---------------------------END--------------------------------
