##############################################################
##############################################################
##  
##  APIS Cisco (Lucke et al.) manuscript
##  
## 
##############################################################
##############################################################
## ===========================================================
## Clear the environment first ===============================
## ===========================================================
rm(list = ls(all.names=TRUE))


## ===========================================================
## Load Packages =============================================
## ===========================================================
library(readxl)        # reading Excel data
library(dplyr)         # manipulating data
library(magrittr)      # for %<>%
library(tidyr)         # transforming data arrangement (tidy data!!)
library(ggplot2)       # visualizations
library(lemon)         # for facet_rep_wrap()


## ===========================================================
## Load in the data ==========================================
## ===========================================================
larval.tl <- read_excel("data/APIS_Coregonus_2018.xlsx", sheet = "Larval_Length_Yolk") %>% 
  filter(include == "Y") %>% 
  mutate(tl.mm = as.numeric(tl.mm)) %>% 
  select(trawl, serial, week, loc.bin.id, tl.mm, tl.bin, yolk.cond)


## ===========================================================
## Calculate weekly length frequency for each length bin =====
## ===========================================================
larval.tl.freq <- larval.tl %>% group_by(week, tl.bin) %>% 
  summarize(n.tl = n(),
            mean.tl = mean(tl.mm)) %>% 
  ungroup()


## ===========================================================
## Calculate freq for each tl bin and yolk-sac condition =====
## ===========================================================
larval.yolk.freq <- larval.tl %>% group_by(tl.bin, yolk.cond) %>% 
  summarize(n.tl = n()) %>% 
  ungroup()


## ===========================================================
## Rename yolk-sac conditions
## ===========================================================
larval.yolk.freq$yolk.cond <- gsub("Yolk sac and globule", "Yolk Sac Present", larval.yolk.freq$yolk.cond)
larval.yolk.freq$yolk.cond <- gsub('Oil globule only', 'Oil Globule Only', larval.yolk.freq$yolk.cond)

larval.yolk.freq %<>% mutate(yolk.cond = factor(yolk.cond, ordered = TRUE, levels = c('Yolk Sac Present', 'Oil Globule Only', 'Absorbed')))


## ===========================================================
## Expand week numbers to date ranges
## ===========================================================
larval.tl.freq$week <- gsub('23', 'June 4-5', larval.tl.freq$week)
larval.tl.freq$week <- gsub('20', 'May 14-15', larval.tl.freq$week)
larval.tl.freq$week <- gsub('25', 'June 18-20', larval.tl.freq$week)
larval.tl.freq$week <- gsub('30', 'July 23-25', larval.tl.freq$week)
larval.tl.freq$week <- gsub('21', 'May 21-23', larval.tl.freq$week)
larval.tl.freq$week <- gsub('29', 'July 16-17', larval.tl.freq$week)
larval.tl.freq$week <- gsub('22', 'May 29', larval.tl.freq$week)
larval.tl.freq$week <- gsub('24', 'June 12-13', larval.tl.freq$week)
larval.tl.freq$week <- gsub('27', 'July 2-5', larval.tl.freq$week)
larval.tl.freq$week <- gsub('28', 'July 9-11', larval.tl.freq$week)
larval.tl.freq$week <- gsub('26', 'June 26-28', larval.tl.freq$week)

larval.tl.freq %<>% mutate(week = factor(week, levels = c('May 14-15', 'May 21-23', 'May 29','June 4-5', 
                                                          'June 12-13', 'June 18-20','June 26-28', 'July 2-5',
                                                          'July 9-11','July 16-17', 'July 23-25'),
                                         ordered = TRUE))


## ===========================================================
## Visualization =============================================
## ===========================================================
## -----------------------------------------------------------
## Length Frequency
## -----------------------------------------------------------
ggplot(larval.tl.freq, aes(x = tl.bin, y = n.tl)) +
  geom_bar(stat = "identity", width = 1, color = "black", fill = "gray80") +
  scale_x_continuous(limits = c(5, 26.55), breaks = seq(5, 25, 5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 60, 15), expand = c(0, 0)) +
  labs(x = "Length Bin (mm)", y = "Frequency") +
  theme(panel.grid = element_blank(), panel.background = element_blank(),
        panel.spacing = unit(1, "lines"), axis.line = element_line(),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 25, margin = margin(0, 15, 0, 0)),
        axis.title.x = element_text(size = 25, margin = margin(15, 0, 0, 0)),
        axis.ticks.length = unit(1.25, 'mm'),
        strip.text = element_text(size = 13),
        strip.background = element_blank()) +
  facet_rep_wrap(~week, dir = "v", ncol = 2)

## Save figure
ggsave("figures/apis_larval_freq_length_weekly.png", dpi = 300, width = 8, height = 10)


## -----------------------------------------------------------
## Yolk/Length Frequency
## -----------------------------------------------------------
ggplot(larval.yolk.freq, aes(x = tl.bin, y = n.tl, fill = yolk.cond)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  scale_x_continuous(limits = c(5, 26.55), breaks = seq(5, 25, 5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 110, 10), expand = c(0, 0)) +
  scale_fill_grey(start = 0.3, end = 0.85) +
  labs(x = "Length Bin (mm)", y = "Frequency", fill = "Yolk-sac Condition") +
  theme(panel.grid = element_blank(), panel.background = element_blank(),
        panel.spacing = unit(1, "lines"), axis.line = element_line(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 25, margin = margin(0, 15, 0, 0)),
        axis.title.x = element_text(size = 25, margin = margin(15, 0, 0, 0)),
        axis.ticks.length = unit(2, 'mm'), legend.position = c(0.87, 0.9),
        legend.text = element_text(size = 13), legend.title = element_text(size = 15),
        plot.margin = unit(c(5, 5, 5, 5), "mm"))

## Save figure
ggsave("figures/apis_larval_freq_length_yolk.png", dpi = 300, width = 10, height = 10)

