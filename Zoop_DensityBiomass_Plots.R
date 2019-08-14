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

library(dplyr)         # manipulating data
library(magrittr)      # for %<>%
library(ggplot2)       # visualizations
library(grid)          # plot matrix text
library(gridExtra)     # plot matrix layout
library(gtable)        # plot matrix legend


## LOAD DATA ====================================================

zoop <- read.csv("data/APIS_Zooplankton_2018.csv", header = TRUE)



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
                                ifelse(species == "Calanoid copepodid", "  Calanoid Copepodites",
                                ifelse(species == "Cyclopoid copepodid", "  Cyclopoid Copepodites",
                                ifelse(species == "Nauplii", "  Nauplii", "")))))))))))))),
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


## ABBREVIATE TAXA NAMES ========================================

weekly.data$taxa.group.2 <- gsub('Cyclopoids', "CY", weekly.data$taxa.group.2)
weekly.data$taxa.group.2 <- gsub('Cyclopoid Copepodites', "CY*", weekly.data$taxa.group.2)
weekly.data$taxa.group.2 <- gsub('Bythotrephes', "BY", weekly.data$taxa.group.2)
weekly.data$taxa.group.2 <- gsub('Daphnia', "DA", weekly.data$taxa.group.2)
weekly.data$taxa.group.2 <- gsub('Calanoids', "CA", weekly.data$taxa.group.2)
weekly.data$taxa.group.2 <- gsub('Calanoid Copepodites', "CA*", weekly.data$taxa.group.2)
weekly.data$taxa.group.2 <- gsub('Nauplii', "NA", weekly.data$taxa.group.2)
weekly.data$taxa.group.2 <- gsub('Other Cladocera', "OC", weekly.data$taxa.group.2)


## VISUALIZATION ================================================

## Mean density
zoop.density <- ggplot(weekly.data, aes(x = week, y = mean.density)) +
  geom_line(aes(colour = taxa.group.2), size = 1) +
  geom_point(size = 1.5) +
  scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1), labels = scales::number_format(accuracy = 0.01), expand = c(0, 0)) +
  scale_x_continuous(limits = c(20, 30), breaks = seq(20, 30, 1), expand = c(0, 0)) +
  labs(x = 'Week', y = 'Mean Density (#/L)') +
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.key.size = unit(0.75, 'cm'),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 25, margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(size = 25, margin = margin(10, 0, 0, 0)),
        axis.ticks.length = unit(2.0, 'mm'), 
        plot.margin = unit(c(5, 10, 10, 5), 'mm'))

## Save figure
ggsave("figures/apis_zoop_density.png", plot = zoop.density, width = 12, height = 8, dpi = 300)


## Mean biomass
zoop.biomass <- ggplot(weekly.data, aes(x = week, y = mean.biomass)) +
  geom_line(aes(colour = taxa.group.2), size = 1) +
  geom_point(size = 1.5) +
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, 0.5), labels = scales::number_format(accuracy = 0.01), expand = c(0, 0)) +
  scale_x_continuous(limits = c(20, 30), breaks = seq(20, 30, 1), expand = c(0, 0)) +
  labs(x = 'Week', y = 'Mean Biomass (µg dry/L)') +
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.key.size = unit(0.75, 'cm'),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 25, margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(size = 25, margin = margin(10, 0, 0, 0)),
        axis.ticks.length = unit(2.0, 'mm'), 
        plot.margin = unit(c(5, 5, 10, 10), 'mm'))

## Save figure
ggsave("figures/apis_zoop_biomass.png", plot = zoop.biomass, width = 12, height = 8, dpi = 300)


## Proportional stacked area plot of density
zoop.prop.density <- ggplot(weekly.data, aes(x = week, y = prop.density, fill = taxa.group.2)) +
  geom_area(position = "fill", color = "black") + 
  scale_x_continuous(limits = c(20, 30), breaks = seq(20, 30, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(x = 'Week', y = 'Proportional Density') +
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.key.size = unit(0.75, 'cm'),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 25, margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(size = 25, margin = margin(10, 0, 0, 0)),
        axis.ticks.length = unit(2.0, 'mm'), 
        plot.margin = unit(c(5, 10, 10, 5), 'mm'))

## Save figure
ggsave("figures/apis_zoop_propDensity.png", plot = zoop.prop.density, width = 12, height = 8, dpi = 300)


## Proportional stacked area plot of biomass
zoop.prop.biomass <- ggplot(weekly.data, aes(x = week, y = prop.biomass, fill = taxa.group.2)) +
  geom_area(stat = "identity", position = "fill", color = "black") + 
  scale_x_continuous(limits = c(20, 30), breaks = seq(20, 30, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(x = 'Week', y = 'Proportional Biomass') +
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.key.size = unit(0.75, 'cm'),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 25, margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(size = 25, margin = margin(10, 0, 0, 0)),
        axis.ticks.length = unit(2.0, 'mm'), 
        plot.margin = unit(c(5, 5, 10, 10), 'mm'))

## Save figure
ggsave("figures/apis_zoop_propBiomass.png", plot = zoop.prop.biomass, width = 12, height = 8, dpi = 300)


## PANELED VISUALIZATION ========================================

## Create common legend
legend <- get_legend(zoop.prop.density + theme(legend.position = "right"))

# arrange the three plots in a single row
zoop.grid <- plot_grid(zoop.density + theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()),
                       zoop.biomass + theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()),
                       zoop.prop.density + theme(legend.position = "none", axis.title.x = element_blank()),
                       zoop.prop.biomass + theme(legend.position = "none", axis.title.x = element_blank()),
                       nrow = 2
)

## add legend to grid
zoop.grid.legend <- plot_grid(zoop.grid, legend, ncol = 2, rel_widths = c(2, 0.2))

## add common x-axis label
ggdraw(add_sub(zoop.grid.legend, "Week", vpadding = grid::unit(0,"lines"), y = 0.75, x = 0.48, size = 30))

ggsave("figures/apis_zoop_gridded.png", width = 18, height = 10, dpi = 300)

