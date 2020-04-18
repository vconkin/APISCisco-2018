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

## Expand week numbers

larval.tl$week <- gsub('20', 'May 14', larval.tl$week)
larval.tl$week <- gsub('25', 'June 18', larval.tl$week)
larval.tl$week <- gsub('23', 'June 4', larval.tl$week)
larval.tl$week <- gsub('30', 'July 23', larval.tl$week)
larval.tl$week <- gsub('21', 'May 21', larval.tl$week)
larval.tl$week <- gsub('29', 'July 16', larval.tl$week)
larval.tl$week <- gsub('28', 'July 9', larval.tl$week)
larval.tl$week <- gsub('22', 'May 28', larval.tl$week)
larval.tl$week <- gsub('24', 'June 11', larval.tl$week)
larval.tl$week <- gsub('27', 'July 2', larval.tl$week)
larval.tl$week <- gsub('26', 'June 25', larval.tl$week)

larval.tl %<>% mutate(week = factor(week, levels = c('May 14', 'May 21', 'May 28','June 4', 
                                                          'June 11', 'June 18','June 25', 'July 2',
                                                          'July 9','July 16', 'July 23'),
                                         ordered = TRUE))

## Calculate weekly length frequency for each length bin =====

larval.tl.freq.mean <- larval.tl %>% group_by(week) %>% 
  summarize(mean.tl = mean(tl.mm),
            sd.tl = sd(tl.mm),
            se.tl = sd.tl/sqrt(n()))

larval.tl.freq <- larval.tl %>% group_by(week, tl.bin) %>% 
  summarize(n.tl = n()) %>% 
  ungroup() %>% 
  group_by(week) %>% 
  mutate(n.week = sum(n.tl)) %>% 
  ungroup()


## Visualization =============================================

## Length Frequency

ggplot(larval.tl.freq, aes(x = tl.bin, y = n.tl)) +
  geom_bar(stat = "identity", width = 1, color = "black", fill = "gray80") +
  scale_x_continuous(limits = c(5, 26.55), breaks = seq(5, 25, 5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 60, 15), expand = c(0, 0)) +
  geom_text(aes(label = paste0("n = ", n.week)), x = Inf, y = Inf, hjust = 1.1, vjust = 1.1, size = 3.5) +
  geom_point(data = larval.tl.freq.mean, aes(x = mean.tl, y = 62.8)) +
  geom_errorbarh(data = larval.tl.freq.mean, height = 2.5, aes(y = 62.8, x = mean.tl, xmin = mean.tl-sd.tl, xmax = mean.tl+sd.tl)) +
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
