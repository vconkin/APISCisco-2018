rm(list=ls(all=TRUE)) #wipes your R workspace clean.

library(plyr)
library(dplyr)
library(ggplot2)
library(ggridges)
library(ggpubr)


data = read.csv("data/Superior_Files/Summaries/Zoop_Summary.csv", header=TRUE)

# R is pretty picky about date format.
# It recognizes excel default date format as factor and not Date.
# Need to change the date foramt to YYYY-MM-DD using:
data$Date=as.Date(data$Date,"%m/%d/%Y")

newdata<-data

newdata$Taxa_Group2 = case_when(newdata$species == "Acanthocyclops" ~ "Cyclopoids",
  newdata$species == "Bosmina" ~ "Other_Cladocera",
  newdata$species == "Bythotrephes" ~ "Bythotrephes",
  newdata$species == "Daphnia" ~ "Daphnia",
  newdata$species == "Diacyclops" ~ "Cyclopoids",
  newdata$species == "Diaphanosoma" ~ "Other_Cladocera",
  newdata$species == "Epischura" ~ "Calanoids",
  newdata$species == "Holopedium" ~ "Other_Cladocera",
  newdata$species == "Limnocalanus" ~ "Calanoids",
  newdata$species == "Leptodiaptomus" ~ "Calanoids",
  newdata$species == "Mesocyclops" ~ "Cyclopoids",
  newdata$species == "Nauplii" ~ "Nauplii",
)

newdata$Taxa_Group2 <- as.factor(newdata$Taxa_Group2)

weekly_data <- newdata %>% group_by(Week, Station, Taxa_Group2) %>%
  summarize(Sum_Density = sum(Density_L), Sum_Biomass = sum(BiomassDW_ugL)) %>% 
  group_by(Week, Taxa_Group2) %>% 
  summarize(Mean_Density = mean(Sum_Density), Mean_Biomass = mean(Sum_Biomass)) 

####PLOT MEAN DENSITY####
ggplot(weekly_data, aes(x = Week, y = Mean_Density)) +
  geom_point(size = 1.5) +
  geom_line(aes(linetype = Taxa_Group2, colour = Taxa_Group2), size = 0.85) +
  scale_y_continuous(limits = c(0, 4), expand = c(0, 0)) +
  scale_x_continuous(limits = c(19.5, 30.5), breaks = seq(20, 30, 1), expand = c(0, 0))+
  
labs(x = 'Week', y = 'Mean density (#/L)') +
theme_bw() + 
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        legend.title=element_blank(),
        legend.text =element_text(size=10),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15, margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(size = 15, margin = margin(10, 0, 0, 0)),
        axis.ticks.length = unit(1.0, 'mm'), 
        plot.margin = unit(c(5, 5, 5, 5), 'mm'))



#PLOT MEAN BIOMASS####
ggplot(weekly_data, aes(x = Week, y = Mean_Biomass)) +
  geom_point(size = 1.5) +
  geom_line(aes(linetype = Taxa_Group2, colour = Taxa_Group2), size = 0.85) +
  scale_y_continuous(limits = c(0, 4), expand = c(0, 0)) +
  scale_x_continuous(limits = c(19.5, 30.5), breaks = seq(20, 30, 1), expand = c(0, 0))+
        
  labs(x = 'Week', y = 'Mean biomass (ug dry/L)') +
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        legend.title=element_blank(),
        legend.text =element_text(size=10),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15, margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(size = 15, margin = margin(10, 0, 0, 0)),
        axis.ticks.length = unit(1.0, 'mm'), 
        plot.margin = unit(c(5, 5, 5, 5), 'mm'))


#PLOT PROPORTIONAL STACKED AREA PLOT OF DENSITY####
ggplot(weekly_data, aes(x = Week, y = Mean_Density, fill = as.factor(Taxa_Group2))) +
  geom_area(stat = "identity", position = "fill") + 
  scale_x_continuous(limits = c(19.5, 30.5), breaks = seq(20, 30, 1), expand = c(0, 0)) +
  labs(x = 'Week', y = 'Proportional density') +
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        legend.title=element_blank(),
        legend.text =element_text(size=10),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15, margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(size = 15, margin = margin(10, 0, 0, 0)),
        axis.ticks.length = unit(1.0, 'mm'), 
        plot.margin = unit(c(5, 5, 5, 5), 'mm'))
  
#PLOT PROPORTIONAL STACKED AREA PLOT OF BIOMASS####
ggplot(weekly_data, aes(x = Week, y = Mean_Biomass, fill = as.factor(Taxa_Group2))) +
  geom_area(stat = "identity", position = "fill") + 
  scale_x_continuous(limits = c(19.5, 30.5), breaks = seq(20, 30, 1), expand = c(0, 0)) +
  labs(x = 'Week', y = 'Proportional biomass') +
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        legend.title=element_blank(),
        legend.text =element_text(size=10),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15, margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(size = 15, margin = margin(10, 0, 0, 0)),
        axis.ticks.length = unit(1.0, 'mm'), 
        plot.margin = unit(c(5, 5, 5, 5), 'mm'))



####PLOT MEAN DENSITY ON LOG10 Y SCALE####

#convert 0s to low value for plotting on log scale
logplot_weekly_data <- weekly_data %>% 
  mutate(Mean_Density = ifelse(Mean_Density == 0, 0.00001, Mean_Density))  %>% 
  mutate(Mean_Biomass = ifelse(Mean_Biomass == 0,0.0001,Mean_Biomass))

ggplot(logplot_weekly_data, aes(x = Week, y = Mean_Density)) +
  geom_point(size = 1.5) +
  geom_line(aes(linetype = Taxa_Group2, colour = Taxa_Group2), size = 0.85) +
  scale_y_continuous(limits = c(0.00001, 5), trans = "log10", minor_breaks = waiver(), expand = c(0, 0)) +
  scale_x_continuous(limits = c(19.5, 30.5), breaks = seq(20, 30, 1), expand = c(0, 0))+
  
  labs(x = 'Week', y = 'Mean density (#/L)') +
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        legend.title=element_blank(),
        legend.text =element_text(size=10),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15, margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(size = 15, margin = margin(10, 0, 0, 0)),
        axis.ticks.length = unit(1.0, 'mm'), 
        plot.margin = unit(c(5, 5, 5, 5), 'mm'))

