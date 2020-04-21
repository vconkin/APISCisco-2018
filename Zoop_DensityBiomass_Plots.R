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
library(cowplot)       # plot matrix layout
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
options(scipen=999)
weekly.data <- zoop %>% group_by(week, station, taxa.group.2) %>%
  summarize(sum.density = sum(density.l), 
            sum.biomass = sum(biomass.dry.wt.ugL)) %>% 
  group_by(week, taxa.group.2) %>% 
  summarize(mean.density = mean(sum.density), 
            mean.biomass = mean(sum.biomass),
            sd.density = sd(sum.density),
            sd.biomass = sd(sum.biomass),
            se.density = sd.density/sqrt(n()),
            se.biomass = sd.biomass/sqrt(n())) %>% 
  group_by(week) %>% 
  mutate(sum.density = sum(mean.density), 
         sum.biomass = sum(mean.biomass),
         prop.density = mean.density/sum.density,
         prop.biomass = mean.biomass/sum.biomass)

weekly.data.total <- zoop %>% group_by(week, station) %>%
  summarize(sum.density = sum(density.l), 
            sum.biomass = sum(biomass.dry.wt.ugL)) %>% 
  group_by(week) %>% 
  summarize(mean.density = round(mean(sum.density),3), 
            mean.biomass = round(mean(sum.biomass),3),
            se.density = round(sd(sum.density)/sqrt(n()),3),
            se.biomass = round(sd(sum.biomass)/sqrt(n()),3))


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

## Define a colorblind safe(ish) palette for 7-classes
color <- c("gray30", "#e69f00", "#56b4e9", "#009e73", "#f0e442", "#0072b2", "#d55e00", "#cc79a7")
shape <- c(9, 10, 8, 1, 15, 16, 17, 18)

## Mean density
zoop.density <- ggplot(weekly.data, aes(x = week, y = mean.density)) +
  geom_line(aes(colour = taxa.group.2), size = 1) +
  geom_point(size = 3.5, aes(shape = taxa.group.2)) +
  scale_y_continuous(limits = c(0, 3.5), breaks = seq(0, 3.5, 0.5), labels = scales::number_format(accuracy = 0.01), expand = c(0, 0)) +
  scale_x_continuous(limits = c(20, 30), breaks = seq(20, 30, 1), expand = c(0, 0)) +
  scale_color_manual(values = color) +
  scale_shape_manual(values = shape) +
  labs(x = 'Week', y = 'Mean Density (#/L)') +
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1.0, 'cm'),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 25, margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(size = 25, margin = margin(10, 0, 0, 0)),
        axis.ticks.length = unit(2.0, 'mm'), 
        plot.margin = unit(c(10, 5, 15, 5), 'mm'))

## Mean biomass
zoop.biomass <- ggplot(weekly.data, aes(x = week, y = mean.biomass)) +
  geom_line(aes(colour = taxa.group.2), size = 1) +
  geom_point(size = 3.5, aes(shape = taxa.group.2)) +
  scale_y_continuous(limits = c(0, 3.5), breaks = seq(0, 3.5, 0.5), labels = scales::number_format(accuracy = 0.01), expand = c(0, 0)) +
  scale_x_continuous(limits = c(20, 30), breaks = seq(20, 30, 1), expand = c(0, 0)) +
  scale_color_manual(values = color) +
  scale_shape_manual(values = shape) +
  labs(x = 'Week', y = 'Mean Biomass (µg dry/L)') +
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1.0, 'cm'),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 25, margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(size = 25, margin = margin(10, 0, 0, 0)),
        axis.ticks.length = unit(2.0, 'mm'), 
        plot.margin = unit(c(10, 5, 15, 5), 'mm'))

## Proportional stacked area plot of density
zoop.prop.density <- ggplot(weekly.data, aes(x = week, y = prop.density, fill = taxa.group.2)) +
  geom_area(position = "fill", color = "black") + 
  scale_x_continuous(limits = c(20, 30), breaks = seq(20, 30, 1), expand = c(0, 0),
                     labels = c('May 14', 'May 21', 'May 28','June 4',
                                'June 11', 'June 18','June 25', 'July 2',
                                'July 9','July 16', 'July 23')) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_manual(values = color) +
  labs(x = 'Week', y = 'Proportional Density') +
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1.0, 'cm'),
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 25, margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(size = 25, margin = margin(10, 0, 0, 0)),
        axis.ticks.length = unit(2.0, 'mm'), 
        plot.margin = unit(c(-2, 5, 5, 5), 'mm'))

## Proportional stacked area plot of biomass
zoop.prop.biomass <- ggplot(weekly.data, aes(x = week, y = prop.biomass, fill = taxa.group.2)) +
  geom_area(stat = "identity", position = "fill", color = "black") + 
  scale_x_continuous(limits = c(20, 30), breaks = seq(20, 30, 1), expand = c(0, 0),
                     labels = c('May 14', 'May 21', 'May 28','June 4',
                                'June 11', 'June 18','June 25', 'July 2',
                                'July 9','July 16', 'July 23')) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_manual(values = color) +
  labs(x = 'Week', y = 'Proportional Biomass') +
  theme_bw() + 
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.key.size = unit(1.0, 'cm'),
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 25, margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(size = 25, margin = margin(10, 0, 0, 0)),
        axis.ticks.length = unit(2.0, 'mm'), 
        plot.margin = unit(c(-2, 5, 5, 5), 'mm'))


## PANELED VISUALIZATION ========================================

## Create common legend
legend.mean <- get_legend(zoop.density + theme(legend.position = c(0.5, 0.535)))
legend.prop <- get_legend(zoop.prop.density + theme(legend.position = c(0.5, 0.64)))

# arrange the three plots in a single row
zoop.grid <- plot_grid(zoop.density + theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()),
                       zoop.biomass + theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank()),
                       nrow = 1)

## add legend to grid
zoop.grid.legend <- plot_grid(zoop.grid, legend.mean, ncol = 2, rel_widths = c(2, 0.2))

# arrange the three plots in a single row
zoop.grid2 <- plot_grid(zoop.prop.density + theme(legend.position = "none", axis.title.x = element_blank()),
                        zoop.prop.biomass + theme(legend.position = "none", axis.title.x = element_blank()),
                        nrow = 1)

## add legend to grid
zoop.grid.legend2 <- plot_grid(zoop.grid2, legend.prop, ncol = 2, rel_widths = c(2, 0.2))

## combine plots
zoop.grid.all <- plot_grid(zoop.grid.legend, zoop.grid.legend2, nrow = 2, rel_widths = c(2, 0.2))
  
## add common x-axis label
ggdraw(add_sub(zoop.grid.all, "Week", vpadding = grid::unit(0,"lines"), y = 0.75, x = 0.48, size = 30))

ggsave("figures/apis_zoop_gridded.png", width = 18, height = 10, dpi = 300)

