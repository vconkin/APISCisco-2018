#################################################################
##
##  APIS Cisco (Lucke et al.) manuscript
##  
## This file calculates the average diet for larvae that had
## food in the gut at time of capture, broken down into the
## five most abundant prey groups by numbers. This also includes
## calculations for average diet for fish that fed during the
## first and last two weeks of sampling
## 
#################################################################

## CLEAR ENVIRONMENT ============================================

rm(list = ls(all.names=TRUE))


## LOAD PACKAGES ================================================

library(readxl)        # reading Excel data
library(dplyr)         # manipulating data
library(magrittr)      # for %<>%
library(tidyr)         # transforming data arrangement (tidy data!!)
library(ggplot2)       # visualizations


## LOAD DATA ====================================================

effort <- read_excel("data/APIS_Coregonus_2018.xlsx", sheet = "Neuston_Effort") %>% 
  select(trawl, week, tow.distance.km)
larval.fish <- read_excel("data/APIS_Coregonus_2018.xlsx", sheet = "Larval_Length_Yolk") %>% 
  filter(include == "Y") %>% 
  select(trawl, tl.mm)


## CALCULATE DENSITY ============================================

effort %<>% mutate(volume.km3 = 0.25 * tow.distance.km,
                   volume.m3 = volume.km3 * 1000)

larval.trawl.abundance <- larval.fish %>% group_by(trawl) %>% 
  summarize(n.fish = n())
  
larval.trawl.density <- left_join(effort, larval.trawl.abundance) %>% 
  mutate(density.m3 = n.fish/volume.m3,
         density.1000.m3 = (n.fish/volume.m3)*1000,
         n.fish = replace_na(n.fish, 0),
         density.m3 = replace_na(density.m3, 0),
         density.1000.m3 = replace_na(density.1000.m3, 0)) 

larval.trawl.density.table <- larval.trawl.density %>% group_by(week) %>% 
  summarize(mean = mean(density.1000.m3),
            median = median(density.1000.m3),
            quantile.100 = quantile(density.1000.m3, 1.0),
            quantile.75 = quantile(density.1000.m3, 0.75),
            quantile.50 = quantile(density.1000.m3, 0.50),
            quantile.25 = quantile(density.1000.m3, 0.25),
            quantile.0 = quantile(density.1000.m3, 0.0),
            )

## EXPAND WEEK LABELS ===========================================

larval.trawl.density$week <- gsub('20', 'May 14', larval.trawl.density$week)
larval.trawl.density$week <- gsub('25', 'June 18', larval.trawl.density$week)
larval.trawl.density$week <- gsub('23', 'June 4', larval.trawl.density$week)
larval.trawl.density$week <- gsub('30', 'July 23', larval.trawl.density$week)
larval.trawl.density$week <- gsub('21', 'May 21', larval.trawl.density$week)
larval.trawl.density$week <- gsub('29', 'July 16', larval.trawl.density$week)
larval.trawl.density$week <- gsub('28', 'July 9', larval.trawl.density$week)
larval.trawl.density$week <- gsub('22', 'May 28', larval.trawl.density$week)
larval.trawl.density$week <- gsub('24', 'June 11', larval.trawl.density$week)
larval.trawl.density$week <- gsub('27', 'July 2', larval.trawl.density$week)
larval.trawl.density$week <- gsub('26', 'June 25', larval.trawl.density$week)

larval.trawl.density %<>% mutate(week = factor(week, levels = c('May 14', 'May 21', 'May 28','June 4', 
                                                                'June 11', 'June 18','June 25', 'July 2',
                                                                'July 9','July 16', 'July 23'),
                                               ordered = TRUE))


## VISUALIZATION ================================================

ggplot(larval.trawl.density, aes(x = week, y = density.1000.m3)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +  
  geom_boxplot(size = 1.0, outlier.size = 2.5) + 
  scale_x_discrete(expand = c(0.05, 0))+
  scale_y_continuous(limits = c(-2, 260), breaks = seq(0, 250, 50), expand = c(0, 0)) +
  labs(x = "Week", y = expression(Larvae~per~1000~m^{3})) +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        strip.text = element_blank(),
        legend.title =  element_blank(),
        legend.text = element_text(size = 15),
        legend.key.size = unit(0.75, 'cm'),
        legend.position = "none",
        axis.ticks.length = unit(2.0, 'mm'),
        axis.text.y = element_text(size = 21, colour = "black"),
        axis.text.x = element_text(size = 21, colour = "black"),
        axis.title.y = element_text(size = 30, margin = margin(0, 20, 0, 0)),
        axis.title.x = element_text(size = 30, margin = margin(20, 0, 0, 0)),
        panel.spacing = unit(2, "lines"),
        plot.margin = unit(c(5, 5, 5, 5), "mm"))

ggsave("figures/Fig_4_density.tiff", width = 15, height = 10, dpi = 300)
  
