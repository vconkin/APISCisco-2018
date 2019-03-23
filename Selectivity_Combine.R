## This file combines the Zems data with length and diet data
## making it the last step of the Zems workflow. We start by
## calculating the indices we're interested in, and then we 
## make a few additional plots that combine the diet and 
## Zems data

##=====Set up/format====================================
## Set the directory to the folder before the Data/Zems split
setwd("C:/Users/vsluc/Documents/UVM/Data Analysis/")
## Clear environment, exclude all blank cells
rm(list = ls(all.names = TRUE))
na.rm=TRUE
##=====Load Packages ===================================
library(car)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(readxl)
library(grid)
library(gridExtra)

##=====Load in the data =================================
## Let's try to calculate Chesson's alpha and Strauss' Linear Selection Index
## Twe start with Chesson's alpha
## To do this we must first calculate forage ratio
## We need the processing data for lengths and stomach data
## We'll also need the density information and effort data later
excel_sheets("Stomachs/Larval_Coregonus_processing.xlsx")
PLength<-read_excel("Stomachs/Larval_Coregonus_Processing.xlsx", sheet="Length_Condition")
Contents<-read_excel("Stomachs/Larval_Coregonus_Processing.xlsx", sheet="Stomach_Mod")
ZDensity = read.csv("Zems/ZDensity.csv", header=TRUE)
Effort<-read.csv("Zems/APIS_Cut.csv", header=TRUE)
##=====Stomach Content Proportion=========================

### Build the stomach table by pulling out the variables we want, removing the totals
# at the bottom, and create a new variable
# Stomach will group the data, select the prey species and gather the columns of prey
diet<-select(Contents,Sample_Date, Trawl, Site, Length_Bin,
             InBin, Nauplii:Chironomid_pupae) 
diet<-diet[-270,]
diet[is.na(diet)]<-0
colnames(diet)<-c("Sample_Date","Trawl","Station","Length_Bin","In_Bin","Nauplii",
                  "Calanoid Copepodite", "Cyclopoid Copepodite",
                  "Unknown/Fragment Calanoid","Unknown/Fragment Cyclopoid",
                  "Rotifera","Limnocalanus","Holopedium","L.minutus",
                  "L.sicilis","Daphnia","Diacyclops","Eucyclops","Bythotrephes",
                  "Bosmina","Diaphanosoma","Invertebrate eggs","Epischura",
                  "Acanthocyclops","Senecella calanoides","Leptodora kindi",
                  "Chironomid pupae")

## Combine sicilis and minutus to genera, then drop the old columns
## Same stroke, let's collapse the species columns into a 2: prey species and count
drop.col<-c('L.minutus', 'L.sicilis')
diet_comp<-dplyr::mutate(diet, Leptodiaptomus = L.minutus +L.sicilis) %>%
  select(-one_of(drop.col))%>%
  gather(Prey_species, Prey_count, Nauplii:Leptodiaptomus)

## Group the data by trawl number and species and calculate how many of each species
## we see at that site that day. Then remove all the zeros so we're only looking at 
## species observed for that site and day rather than overall
stomach<-group_by(diet_comp,Trawl, Prey_species)%>%
  summarize(Prey_count=sum(Prey_count)) %>%
  filter(Prey_count >0)

## Ungroup to remove the metadata around the dataframe, and then regroup by trawl only
## Calculate the total number of items for that trawl, and then rejoin the two dataframes
ungroup(diet_comp)
Totals<-group_by(diet_comp,Trawl) %>% 
  summarize(Trawl_Total=sum(Prey_count))

st<-inner_join(Totals, stomach, by="Trawl") %>%
  mutate(Prey_Prop = Prey_count/Trawl_Total) %>% 
  select(Trawl, Prey_species, Prey_Prop)

st$Trawl<-as.factor(st$Trawl)

##=====Zooplankton Proportion=========================
## Now let's try to create proportions with the zoop data!
## We start by grouping by trawl and species, then calculating a number of zoops
## from the density. We remove all the zeros, and add a column for number of species
## observed at that site on that day
zoop<-group_by(ZDensity, Trawl, species) %>%
  summarize(Env_count = sum(Density_L)) %>%
  filter(Env_count >0) %>%
  mutate(n = n_distinct(species))

ungroup(ZDensity)
ZTotals<-group_by(ZDensity, Trawl) %>%
  summarize(Trawl_Total = sum(Density_L))

zt<-inner_join(ZTotals, zoop, by = "Trawl") %>%
  mutate(Env_Prop = Env_count/Trawl_Total) %>%
  select(Trawl, species, Env_Prop, n)

zt$Trawl<-as.factor(zt$Trawl)

##=====Electivity Calculations==============================
## This might be messy but let's bind them together!
## We start with joining only the stomach rows with a match in zoops
## Then we need to group by date to allow for calculations to be done by day
## And finally we round out to 4 decimal places
Li<-inner_join(st, zt, by = c("Prey_species" = "species", "Trawl" = "Trawl")) %>%
  group_by(Trawl) %>%
  mutate(Selection_Index = Prey_Prop-Env_Prop, 
         Forage_ratio = Prey_Prop/Env_Prop, 
         Alpha = Forage_ratio/sum(Forage_ratio),
         E = (Alpha-(1/n))/(Alpha +(1/n)))

Li$Trawl<-as.numeric(Li$Trawl)

##=====Average the Selectivity Values=======================
Li_week<-left_join(Li, Effort, by = "Trawl") %>%
  select(Date, Week, Trawl:E) %>%
  group_by(Week, Prey_species) %>%
  summarize(Linear = mean(Selection_Index), Forage_Ratio = mean(Forage_ratio),
            Alpha_avg = mean(Alpha), E = mean(E)) %>%
  mutate_if(is.numeric, round, 2)

write.csv(Li_week, "Selection_Indices.csv", row.names = FALSE)

##=====Plot Electivity Trends===============================
## Alpha average by week
ggplot(Li_week, aes(Week, Alpha_avg, fill=Prey_species))+
  geom_line(aes(group=Prey_species,color=Prey_species), size=1.2)+
  scale_x_continuous(name = "\nWeek of", 
                     breaks=seq(20,30, by=2), 
                     labels = c("14 May","28 May",
                                "11 June","25 June",
                                "9 July", "23 July"))+
  scale_y_continuous(name = "Weekly Alpha average\n", limits = c(-0.2,1))+
  geom_hline(linetype = "dashed", yintercept = 0)+
  theme_classic() +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10))

## E* average by week
ggplot(Li_week, aes(Week, E, fill=Prey_species))+
  geom_line(aes(group=Prey_species,color=Prey_species), size=1.2)+
  scale_x_continuous(name = "\nWeek of",
                     breaks=seq(20,30, by=2), 
                     labels = c("14 May","28 May",
                                "11 June","25 June",
                                "9 July", "23 July"))+
  scale_y_continuous(name = "Weekly E average\n",limits = c(-1,1))+
  labs(color = "Prey Species") +
  geom_hline(linetype = "dashed", yintercept = 0) +
  theme_classic() +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10))

## Quick comparison of the different indices
Methods<-gather(Li_week, Method, Value, Linear:E)
index<-c("Alpha_avg" = "Chesson's Alpha",
         "E" = "V&S's E*",
         "Forage_Ratio" = "Forage Ratio",
         "Linear" = "Strauss's Linear Index")

ggplot(Methods, aes(Week, Value, fill=Prey_species))+
  geom_line(aes(group=Prey_species,color=Prey_species), size=1.2)+
  scale_x_continuous(name = "\nWeek of",
                     breaks=seq(20,30, by=2), 
                     labels = c("14 May","28 May",
                                "11 June","25 June",
                                "9 July", "23 July"))+
  scale_y_continuous(name = "Index Values\n")+
  labs(color = "Prey Species") +
  facet_wrap(nrow=2, Method ~., scales="free_y", labeller = as_labeller(index)) +
  geom_hline(linetype = "dashed", yintercept = 0) +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10),
        strip.text = element_text(size=12),
        strip.background = element_rect(color = "white", fill = "white"))