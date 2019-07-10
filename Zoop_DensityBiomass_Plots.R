#################################################################
##  
##  APIS Cisco (Lucke et al.) manuscript
##  
##  
##      
#################################################################

## CLEAR ENVIRONMENT ============================================

rm(list = ls(all.names=TRUE))


## LOAD PACKAGES ================================================

library(readxl)        # reading Excel data
library(dplyr)         # manipulating data
library(ggplot2)       # visualizations


## LOAD DATA ====================================================

zoop <- read_excel("data/APIS_Zooplankton_2018.xlsx", sheet = "Zoop Biomass-Density") 



## DATA MANIPULATION ============================================

zoop %<>% mutate(taxa.group.2 = ifelse(species == "Acanthocyclops", "  Cyclopoids",
                                ifelse(species == "Bosmina", "  Other Cladocera",
                                ifelse(species == "Bythotrephes", "  Bythotrephes",
                                ifelse(species == "Daphnia", "  Daphnia",
                                ifelse(species == "Diacyclops", "  Cyclopoids",
                                ifelse(species == "Diaphanosoma", "  Other Cladocera",
                                ifelse(species == "Epischura", "  Calanoids",
                                ifelse(species == "Holopedium", "  Other Cladocera",
                                ifelse(species == "Limnocalanus", "  Calanoids",
                                ifelse(species == "Leptodiaptomus", "  Calanoids",
                                ifelse(species == "Mesocyclops", "  Cyclopoids",
                                ifelse(species == "Nauplii", "  Nauplii", "")))))))))))),
                 tax.group.2 = factor(taxa.group.2))
  

## Summarize weekly and weekly proporational density and biomass for each taxa group
weekly.data <- zoop %>% group_by(week, station, taxa.group.2) %>%
  summarize(sum.density = sum(density.l), 
            sum.biomass = sum(biomass.dry.wt.ugL)) %>% 
  group_by(week, taxa.group.2) %>% 
  summarize(mean.density = mean(sum.density), 
            mean.biomass = mean(sum.biomass)) %>% 
  group_by(week) %>% 
  mutate(sum.density = sum(mean.density), 
         sum.biomass = sum(mean.biomass),
         prop.density = mean.density/sum.density,
         prop.biomass = mean.biomass/sum.biomass)


## VISUALIZATION ================================================

## Mean density
ggplot(weekly.data, aes(x = week, y = mean.density)) +
  geom_point(size = 1.5) +
  geom_line(aes(linetype = taxa.group.2, colour = taxa.group.2), size = 0.85) +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1), expand = c(0, 0)) +
  scale_x_continuous(limits = c(20, 30), breaks = seq(20, 30, 1), expand = c(0, 0)) +
  labs(x = 'Week', y = 'Mean Density (#/L)') +
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        legend.title = element_blank(),
        legend.text = element_text(size=10),
        legend.key.size = unit(0.75, 'cm'),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15, margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(size = 15, margin = margin(10, 0, 0, 0)),
        axis.ticks.length = unit(1.0, 'mm'), 
        plot.margin = unit(c(5, 5, 5, 5), 'mm'))

## Save figure
ggsave("figures/apis_zoop_density.png", width = 12, height = 8, dpi = 300)


## Mean biomass
ggplot(weekly.data, aes(x = week, y = mean.biomass)) +
  geom_point(size = 1.5) +
  geom_line(aes(linetype = taxa.group.2, colour = taxa.group.2), size = 0.85) +
  scale_y_continuous(limits = c(0, 4), expand = c(0, 0)) +
  scale_x_continuous(limits = c(20, 30), breaks = seq(20, 30, 1), expand = c(0, 0)) +
  labs(x = 'Week', y = 'Mean Biomass (ug dry/L)') +
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.75, 'cm'),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15, margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(size = 15, margin = margin(10, 0, 0, 0)),
        axis.ticks.length = unit(1.0, 'mm'), 
        plot.margin = unit(c(5, 5, 5, 5), 'mm'))

## Save figure
ggsave("figures/apis_zoop_biomass.png", width = 12, height = 8, dpi = 300)


## Proportional stacked area plot of density
ggplot(weekly.data, aes(x = week, y = prop.density, fill = taxa.group.2)) +
  geom_area(position = "fill", color = "black") + 
  scale_x_continuous(limits = c(20, 30), breaks = seq(20, 30, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(x = 'Week', y = 'Proportional Density') +
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.75, 'cm'),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15, margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(size = 15, margin = margin(10, 0, 0, 0)),
        axis.ticks.length = unit(1.0, 'mm'), 
        plot.margin = unit(c(5, 5, 5, 5), 'mm'))

## Save figure
ggsave("figures/apis_zoop_propDensity.png", width = 12, height = 8, dpi = 300)


## Proportional stacked area plot of biomass
ggplot(weekly.data, aes(x = week, y = prop.biomass, fill = taxa.group.2)) +
  geom_area(stat = "identity", position = "fill", color = "black") + 
  scale_x_continuous(limits = c(20, 30), breaks = seq(20, 30, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(x = 'Week', y = 'Proportional Biomass') +
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.75, 'cm'),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 15, margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(size = 15, margin = margin(10, 0, 0, 0)),
        axis.ticks.length = unit(1.0, 'mm'), 
        plot.margin = unit(c(5, 5, 5, 5), 'mm'))

## Save figure
ggsave("figures/apis_zoop_propBiomass.png", width = 12, height = 8, dpi = 300)

