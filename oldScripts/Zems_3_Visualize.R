# This file uses outputs from Zems_2_Wrangle to visualize the data we've gathered
rm(list=ls(all=TRUE)) #wipes your R workspace clean.

library(plyr)
library(dplyr)
library(ggplot2)
library(ggridges)
library(ggpubr)

# Start by loading them in
ZTrawl =read.csv("data/Superior_Files/Summaries/ZTrawl.csv", header=TRUE)
ZLength =read.csv("data/Superior_Files/Summaries/ZLength.csv", header=TRUE)
ZDensity =read.csv("data/Superior_Files/Summaries/ZDensity.csv", header=TRUE)
BMass =read.csv("data/Superior_Files/Summaries/SumBySampleSpecies.csv", header=TRUE)

#--------------Density across time------------------------------------
# Plot species by trawl

ggplot(ZDensity,aes(x=Trawl, y= Density_L, fill=species)) +
  geom_bar(position="stack", stat="identity")+
  scale_y_continuous(name = "No. per L", expand = c(0,0)) +
  scale_fill_discrete(name = "Species")+
  theme_classic() +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"))

ggplot(ZDensity,aes(x=Trawl, y= Density_L,fill=species)) +
  geom_bar(position="fill", stat="identity") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(name = "No. per L\n", expand = c(0,0)) +
  scale_fill_discrete(name = "Species")+
  theme_classic()+
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"))

# We need to reorder Date to the appropriate levels
ZDensity$Date =factor(ZDensity$Date, 
                      levels=c("5/14/2018","5/15/2018","5/21/2018",
                               "5/23/2018","5/29/2018","6/4/2018" ,
                               "6/5/2018","6/12/2018","6/13/2018",
                               "6/18/2018","6/20/2018","6/26/2018",
                               "6/28/2018", "7/2/2018","7/5/2018",
                               "7/9/2018","7/11/2018","7/16/2018",
                               "7/17/2018","7/23/2018","7/25/2018" ))

ZDensity$Station =factor(ZDensity$Station, 
                         levels=c("71","66","44","45","87",
                                  "139","525","33","526","527"))
#Then look at mean density by date
ZDensity %>% group_by(Date, species) %>%
  summarize(Mean_Density = mean(Density_L)) %>%
ggplot(aes(x=Date, y= Mean_Density,fill=species)) +
  geom_bar(position="fill", stat="identity") + 
  scale_x_discrete(name = "Date",
                   breaks = c("5/15/2018", "5/29/2018","6/12/2018",
                              "6/26/2018","7/9/2018","7/23/2018"),
                   labels = c("15 May","29 May","12 June",
                              "26 June","9 July", "23 July")) +
  scale_y_continuous(name = "Proportion of Density\n", expand = c(0,0)) +
  scale_fill_discrete(name = "Species")+
  theme_classic() +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"))


ZDensity %>% group_by(Date, species) %>%
  summarize(Mean_Density = mean(Density_L)) %>%
ggplot(aes(x=Date, y= Mean_Density,fill=species)) +
  geom_bar(position="stack", stat="identity") + 
  scale_x_discrete(breaks = c("5/15/2018", "5/29/2018","6/12/2018",
                              "6/26/2018","7/9/2018","7/23/2018"),
                   labels = c("15 May","29 May","12 June",
                              "26 June","9 July", "23 July"))+
  scale_y_continuous(name = "Density (no./L)",expand = c(0,0)) +
  scale_fill_discrete(name = "Species")+
    theme_classic()+
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"))

## Individual station zooplankton composition
ggplot(ZDensity, aes(x= Week, y = Density_L)) +
  geom_bar(aes(fill = species), stat="identity")+
  scale_x_continuous(name = "Week of", breaks = seq(20,30, by = 2),
                     labels = c("5/14", "5/29", "6/12", 
                                "6/26", "7/9","7/23"))+
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_brewer(palette="Paired", direction = -1)+
  facet_wrap(nrow=2, ~Station, scales="free") +
  theme_classic() +
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10),
        strip.text = element_text(size=10),
        strip.background = element_rect(color = "white", fill = "white"))

## Look at island groups instead
ZDensity %>% group_by(Week, species, Group) %>%
  summarize(Mean_Density = mean(Density_L)) %>%
ggplot( aes(x= Week, y = Mean_Density)) +
  geom_bar(aes(fill = species), stat="identity")+
  scale_x_continuous(name = "Week of", breaks = seq(20,30, by = 2),
                     labels = c("5/14", "5/29", "6/12", 
                                "6/26", "7/9","7/23"))+
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_brewer(palette="Paired", direction = -1)+
  theme(axis.text.x=element_text(angle=30)) +
  facet_wrap(nrow=2, ~Group, scales="free") +
  theme_classic() +
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10),
        strip.text = element_text(size=10),
        strip.background = element_rect(color = "white", fill = "white"))

## Filled proportion
ZDensity %>% group_by(Week, species, Group) %>%
  summarize(Mean_Density = mean(Density_L)) %>%
ggplot( aes(x= Week, y = Mean_Density)) +
  geom_bar(aes(fill = species), stat="identity", position = "fill")+
  scale_x_continuous(name = "Week of", breaks = seq(20,30, by = 2),
                     labels = c("5/14", "5/29", "6/12", 
                                "6/26", "7/9","7/23"))+
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_brewer(palette="Paired", direction = -1)+
  theme(axis.text.x=element_text(angle=30)) +
  facet_wrap(nrow=2, ~Group, scales="free") +
  theme_classic() +
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10),
        strip.text = element_text(size=10),
        strip.background = element_rect(color = "white", fill = "white"))

#total numbers of prey groups over time

ZDate<-select(ZTrawl, Jday, Station, Serial, species, Total) %>%
  group_by(Jday, species) %>%
  summarize(sp_total = sum(Total))

ggplot(ZDate, aes(Jday, y=sp_total, color=species, fill=species))+
  geom_density(alpha=0.8, position="fill", stat="identity")+
  scale_x_continuous(name = "Date", expand = c(0,0),
                     breaks = seq(134,204,14),
                     labels = c("14 May","28 May","11 June","25 June","9 July", "22 July")) +
  scale_y_continuous(name = "Proportion of total\n", expand = c(0,0)) +
  labs(color = "Species", fill= "Species") +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"))

ggplot(ZDate, aes(Jday, y=sp_total, color=species, fill=species))+
  geom_density(alpha=0.9, position="stack", stat="identity") +
  scale_x_continuous(name = "Date", expand = c(0,0),
                     breaks = seq(134,204,14),
                     labels = c("14 May","28 May","11 June",
                                "25 June","9 July", "22 July")) +
  scale_y_continuous(name = "Total Zooplankton\n", expand = c(0,0)) +
  labs(color = "Species", fill= "Species") +
  theme_classic()+
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"))

#-------Smoothed Density----------------------------------------------------
# Line plot of density by date
ZTrawl$Station =factor(ZTrawl$Station, 
                       levels=c("71","66","44","45","87",
                                "139","525","33","526","527"))
strDates <- c(levels(ZTrawl$Date))
dates <- as.Date(strDates, "%m/%d/%Y")
x=as.Date(ZTrawl$Date,"%m/%d/%Y")
ZTrawl$Date=x

DDensity<-group_by(ZDensity, Date, species)%>%
  summarize(Avg_Density = mean(Density_L))

DDensity$Date<-as.Date(DDensity$Date, format = "%m/%d")

ggplot(DDensity, aes(Date, Avg_Density, color=species))+
  geom_line(aes(group= species),size=1.1) +
  geom_point(size=2)+
  scale_y_sqrt(name = "Average Density (no./L)\n") +
  labs(color = "Species") +
  theme_classic() +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10))

# Area plots show stacked values for density by species
ggplot(DDensity, aes(x=Date, y=Avg_Density, color=species))+
  geom_area(aes(group=species, fill=species)) +
  scale_y_continuous(name = "Average Density per L\n", expand = c(0,0))+
  labs(color = "Species", fill= "Species") +
  theme_classic() +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10))

WDensity<-group_by(ZDensity, Week, species)%>%
  summarize(Avg_Density = mean(Density_L))

ggplot(WDensity, aes(x=Week, y=Avg_Density, color=species))+
  geom_area(aes(group=species, fill=species))+
  scale_x_continuous(name = "Week of", expand = c(0,0),
                     breaks=seq(20,30, by=2), 
                     labels = c("14 May","28 May",
                                "11 June","25 June",
                                "9 July", "23 July"))+
  scale_y_continuous(name = "Average Density per L\n",
                     expand = c(0,0))+
  labs(color = "Species", fill= "Species") +
  theme_classic()+
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10))

#--------------Length Distributions----------------------------------------

#overall
ggplot(ZLength, aes(x=length, color= species)) +
  geom_freqpoly(binwidth=0.1, size = 1)+
  scale_x_log10 (name = "Length (mm)", expand = c(0,0)) +
  scale_y_sqrt(name = "Count\n", expand = c(0,0))+
  labs(color = "Species")+
  theme_classic()+
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold")) 
  
  
ggplot(ZLength, aes(x=length, fill= species)) +
  geom_density(alpha=0.8) +
  scale_fill_brewer(palette="Paired")+
  scale_x_log10(name = "Length (mm)", expand = c(0,0)) +
  scale_y_continuous(name = "Density\n", expand = c(0,0))+
  labs(color = "Species", fill = "Species")+
  theme_classic()+
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"))

#-------Lengths Ridge plot------------------------------------------------
ZLength %>% mutate(Week = as.factor(Week)) %>%
  ggplot(aes(y = Week)) +
  geom_density_ridges(aes(x = length, fill = species),alpha=0.7)+
  scale_x_sqrt(name = "Length (mm)", breaks = c(0.1, 0.5, 1, 2, 3, 4))+
  scale_y_discrete(name = "Week\n") +
  scale_fill_brewer(palette="Paired", name = "Species")+
  theme_classic() +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10))

# Group into nauplii, calanoids, cyclopoids, and cladocerans
# This isn't really very interesting, but I'm keeping it here in case

#Explore categorizing them in a single data frame and using facet wrap
#to ensure they all have the same y axis for weeks
Naups<-filter(ZLength, species == "Nauplii")
Cal<-filter(ZLength, species == "Epischura" | species == "Leptodiaptomus" | species == "Limnocalanus")
Cyc<-filter(ZLength, species == "Acanthocyclops" | species == "Diacyclops" | species == "Mesocyclops")
Clad<-filter(ZLength, species == "Bosmina" | species == "Daphnia" | species == "Holopedium" | species=="Bythotrephes")
p1<-c("#7fcdbb","#edf8b1","#2c7fb8")
p2<-c("#e66101","#fdb863","#b2abd2")
p3<-c("#a6611a","#dfc27d","#80cdc1","#018571")

Cal %>% mutate(Week = as.factor(Week)) %>%
  ggplot(aes(y = Week)) +
  geom_density_ridges(aes(x = length, fill = species),alpha=0.8)+
  scale_x_log10(name = "Length (mm)")+
  scale_fill_manual(values = p1, name = "Species") +
  theme_classic() +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10))->Ca

Cyc %>% mutate(Week = as.factor(Week)) %>%
  ggplot(aes(y = Week)) +
  geom_density_ridges(aes(x = length, fill = species),alpha=0.8)+
  scale_x_log10(name = "Length (mm)")+
  scale_fill_manual(values=p2, name = "Species")+
  theme_classic() +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10))->Cy

Clad %>% mutate(Week = as.factor(Week)) %>%
  ggplot(aes(y = Week)) +
  geom_density_ridges(aes(x = length, fill = species),alpha=0.8)+
  scale_x_log10(name = "Length (mm)")+
  scale_fill_manual(values=p3, name = "Species")+
  theme_classic() +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10))->Cl

Naups %>% mutate(Week = as.factor(Week)) %>%
  ggplot(aes(y = Week)) +
  geom_density_ridges(aes(x = length, fill = species),alpha=0.8)+
  scale_fill_brewer(palette="Accent", name = "Species")+
  scale_x_continuous(name = "Length (mm)")+
  theme_classic() +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10))->N

ggarrange(Ca,Cy,Cl,N, ncol=2, nrow=2)

#-------Zoop Length bin & Density-----------------------------------------
#When plotted with ZLength, there will be NA rows for nauplii that were
#counted but never measured
ggplot(ZLength, aes(length, group=species)) +
  geom_histogram(aes(y= ..density..,fill=species),
                 breaks =c(0,0.25,0.5,0.75,1,1.5,2,3,4),
                 position = "stack")+
  scale_x_sqrt(name = "Length (mm)",
               breaks =c(0,0.25,0.5,0.75,1,1.5,2,3,4), 
               labels =c(0,0.25,0.5,0.75,1,1.5,2,3,4),
               limits = c(0,4), expand = c(0,0)) +
  scale_y_continuous(name = "Length Bin Density\n",expand = c(0,0))+
  scale_fill_brewer(palette = "Paired", name = "Species")+
  theme_classic()+
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"))

#Make a dataframe with the bin information and make a new column for 
#number of individuals in a bin in a trawl by species
ZBin<-mutate(ZLength, Bin = cut(length, 
                                breaks = c(0,0.25,0.5,0.75,1,1.5,2,3,4),
                                labels = c(0.25, 0.5, 0.75, 1, 1.5,2,3,4))) %>%
  na.omit() %>%
  group_by(Trawl, species, Bin) %>%
  summarize(In_Bin = n())

#Regroup by trawl and species only to calculate the number of individuals
#in a species measured in each trawl
ZBin<-ungroup(ZBin) %>%
  group_by(Trawl, species) %>%
  mutate(Measured_Total = sum(In_Bin))

#Calculate the total number observed in the from ZDensity
ungroup(ZDensity)
ZTotals<-group_by(ZDensity, Trawl, species) %>%
  summarize(Trawl_Density = sum(Density_L))

#Bind the two together and calculate the density for each species
#in each size bin
zb<-inner_join(ZTotals, ZBin, by = c("Trawl", "species"))%>%
  mutate(Prop = In_Bin/Measured_Total)
zb<-group_by(zb, Trawl, species, Bin) %>%
  mutate(No.PerBin = Trawl_Density*Prop)

## Now we look at length distributions by bin going week by week
Effort<-read.csv("APIS_Cut.csv", header=TRUE)

Weeks<-select(Effort, Trawl:Week)
Weeks<-left_join(zb, Weeks, by = "Trawl")

Week_names<-c("20"="May 14-15",
              "21"="May 21-23", 
              "22"="May 29", 
              "23"="June 4-5", 
              "24"="June 12-13", 
              "25"="June 18-20",
              "26"="June 26-28",
              "27"="July 2-5", 
              "28"="July 9-11", 
              "29"="July 16-17", 
              "30"="July 23-25")

ggplot(Weeks, aes(Bin, No.PerBin, fill=species))+
  geom_bar(stat = "identity", position="stack")+
  scale_y_continuous(name = "No. Per L\n") +
  scale_x_discrete(name = "Size Bin")+
  scale_fill_brewer(palette="Paired", name = "Species")+
  facet_wrap(nrow=4, Week ~., labeller = as_labeller(Week_names)) +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10),
        strip.text = element_text(size=12),
        strip.background = element_rect(color = "white", fill = "white"))

## Remove Nauplii from the plot to get a clearer idea of trends
Weeks2<-filter(Weeks, species != "Nauplii")
ggplot(Weeks2, aes(Bin, No.PerBin, fill=species))+
  geom_bar(stat = "identity", position="stack")+
  scale_y_continuous(name = "No. Per L\n") +
  scale_x_discrete(name = "Size Bin")+
  scale_fill_brewer(palette="Paired", name = "Species")+
  facet_wrap(nrow=4, Week ~.,labeller = as_labeller(Week_names)) +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10),
        strip.text = element_text(size=12),
        strip.background = element_rect(color = "white", fill = "white"))

## Add later: sort into groups like in the stomach processing

#-------Biomass Data by Trawl-------------------------------------------
#scatter plot of species by lengths
ggplot(BMass, aes(species, mnIndvLength))+
  geom_boxplot(aes(fill = species), color = "black", 
               alpha=0.2, lwd= 0.1, varwidth = TRUE) +
  geom_jitter(aes(color=species), width=0.25) +
  scale_y_continuous(name = "Trawl Mean Individual Length (mm)\n") +
  scale_x_discrete(name = "Species") +
  labs(fill="Species", color ="Species") +
  theme_classic()+
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10))

  
dates<-select(Effort, Trawl:Week)
bioweek<-left_join(BMass, dates, by = "Trawl")

bioweek$Date =factor(bioweek$Date, 
                      levels=c("5/14/2018","5/15/2018","5/21/2018",
                               "5/23/2018","5/29/2018","6/4/2018" ,
                               "6/5/2018","6/12/2018","6/13/2018",
                               "6/18/2018","6/20/2018","6/26/2018",
                               "6/28/2018", "7/2/2018","7/5/2018",
                               "7/9/2018","7/11/2018","7/16/2018",
                               "7/17/2018","7/23/2018","7/25/2018" ))

# The plots below look at averages across weeks rather than individual dates
# Biomass across weeks with free y axis
bioweek%>% group_by(Week, species) %>%
  summarize(species_avg = mean(BioDWugL)) %>%
  ggplot(aes(Week, species_avg)) + 
  geom_bar(aes(fill=species), stat = "identity") +
  scale_x_continuous(name = "Week of", expand = c(0,0),
                     breaks=seq(20,30, by=2), 
                     labels = c("14 May","28 May",
                                "11 June","25 June",
                                "9 July", "23 July")) +
  scale_y_continuous(name = "Average Biomass (ug/L)\n") +
  scale_fill_discrete(name = "Species") +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10),
        strip.text = element_text(size=12),
        strip.background = element_rect(color = "white", fill = "white")) +
  facet_wrap(nrow=4, species~., scales = "free_y")

# With fixed y axis
bioweek%>% group_by(Week, species) %>%
  summarize(species_avg = mean(BioDWugL)) %>%
  ggplot(aes(Week, species_avg)) + 
  geom_bar(aes(fill=species), stat = "identity") +
  scale_x_continuous(name = "Week of", expand = c(0,0),
                     breaks=seq(20,30, by=2), 
                     labels = c("14 May","28 May",
                                "11 June","25 June",
                                "9 July", "23 July")) +
  scale_y_continuous(name = "Average Biomass (ug/L)\n") +
  scale_fill_discrete(name = "Species") +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10),
        strip.text = element_text(size=12),
        strip.background = element_rect(color = "white", fill = "white")) +
  facet_wrap(nrow=4, species~.)

#Species biomass over time by Date
bioweek%>% group_by(Date, species) %>%
  summarize(species_avg = mean(BioDWugL)) %>%
  ggplot(aes(Date, species_avg)) + 
  geom_bar(aes(fill=species), stat = "identity")+
  scale_x_discrete(name = "Date",
                   breaks = c("5/15/2018", "5/29/2018","6/12/2018",
                              "6/26/2018","7/9/2018","7/23/2018"),
                   labels = c("15 May","29 May","12 June",
                              "26 June","9 July", "23 July")) +
  scale_y_continuous(name = "Average Biomass (ug/L)\n") +
  scale_fill_discrete(name = "Species") +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=10),
        strip.text = element_text(size=12),
        strip.background = element_rect(color = "white", fill = "white")) +
  facet_wrap(nrow=4, species ~.)
  
