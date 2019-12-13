##############################################################
##  
##  APIS Cisco (Lucke et al.) manuscript
##  
## 
##############################################################

## Clear the environment first ===============================

rm(list = ls(all.names=TRUE))

## Load Packages =============================================

library(readxl)        # reading Excel data
library(dplyr)         # manipulating data
library(magrittr)      # for %<>%
library(tidyr)         # transforming data arrangement (tidy data!!)
library(ggplot2)       # visualizations
library(lemon)         # for facet_rep_wrap()


## Load in the data ==========================================

larval.tl <- read_excel("data/APIS_Coregonus_2018.xlsx", sheet = "Larval_Length_Yolk") %>% 
  filter(include == "Y") %>% 
  mutate(tl.mm = as.numeric(tl.mm)) %>% 
  select(trawl, serial, week, loc.bin.id, tl.mm, tl.bin, yolk.cond)


## Calculate weekly length frequency for each length bin =====

larval.tl.freq <- larval.tl %>% group_by(week, tl.bin) %>% 
  summarize(n.tl = n()) %>% 
  ungroup() %>% 
  group_by(week) %>% 
  mutate(n.week = sum(n.tl)) %>% 
  ungroup()


## Expand week numbers

larval.tl.freq %<>% mutate(week = paste0('Week ', week))


## Visualization =============================================

## Length Frequency

ggplot(larval.tl.freq, aes(x = tl.bin, y = n.tl)) +
  geom_bar(stat = "identity", width = 1, color = "black", fill = "gray80") +
  scale_x_continuous(limits = c(5, 26.55), breaks = seq(5, 25, 5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 60, 15), expand = c(0, 0)) +
  geom_text(aes(label = paste0("n = ", n.week)), x = Inf, y = Inf, hjust = 1.1, vjust = 1.1, size = 3.5) +
  labs(x = "Length Class (mm)", y = "Frequency") +
  theme(panel.grid = element_blank(), panel.background = element_blank(),
        panel.spacing = unit(1, "lines"), axis.line = element_line(),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 21, margin = margin(0, 15, 0, 0)),
        axis.title.x = element_text(size = 21, margin = margin(15, 0, 0, 0)),
        axis.ticks.length = unit(1.25, 'mm'),
        strip.text = element_text(size = 13),
        strip.background = element_blank()) +
  facet_rep_wrap(~week, dir = "v", ncol = 2)

## Save figure
ggsave("figures/apis_larval_freq_length_weekly.png", dpi = 300, width = 8, height = 10)
