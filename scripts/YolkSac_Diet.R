#################################################################
##  
##  APIS Cisco (Lucke et al.) manuscript
##  
##  
##  This file combines larval diet and larval length and yolk-sac conditions.
##  Script calculates the probabilty of four yolk sac (y/n) - diet (y/n) combinations
##    within each length bin. Weekly data was pooled to increase sample sizes.
##  
##  Data Limitations:
##  - No individual diet data. Negates direct comparison between fish 
##     that were examined for diet content and measured.
##  - If multiple yolk-sac conditions are present within each 
##     trawl and length bin, data needs to be removed because no individual 
##     diet data or fish IDs were recorded
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
library(lemon)         # for facet_rep_wrap()


## LOAD DATA ====================================================

## Length and yolk-sac condition
larval.tl <- read_excel("data/APIS_Coregonus_2018.xlsx", sheet = "Larval_Length_Yolk") %>% 
  filter(include == "Y") %>% 
  mutate(trawl = factor(trawl),
         week = factor(week)) %>% 
  dplyr::select(trawl, serial, week, loc.bin.id, tl.mm, tl.bin, yolk.cond)

## Larval diet
larval.diet <- read_excel("data/APIS_Coregonus_2018.xlsx", sheet = "Larval_Diet") %>% 
  mutate(trawl = factor(trawl)) %>%
  dplyr::select(trawl, tl.bin, loc.bin.id, n.fish, n.diet, diet.count) %>% 
  mutate(mean.diet.count = diet.count/n.diet,
         mean.diet.count = ifelse(mean.diet.count == "NaN", 0, mean.diet.count),
         trawl = factor(ifelse(trawl == "37.1", "37", trawl)))

## Tow effort (to match trawl numbers with week)
effort <- read_excel("data/APIS_Coregonus_2018.xlsx", sheet = "Neuston Effort") %>% 
  dplyr::select(trawl, week) %>% 
  mutate(trawl = factor(trawl))


## DIET DATA MANIPULATION =======================================

## Loop to expand number of fish;
##  Takes the recorded number of fish "n.fish" from the diet data and 
##  creates a row for each fish (no. of rows = "n.fish").
##  Assigns if food was present or not based on "n.diet" or "n.fish - n.diet".
## 
## IMPORTANT: No. of observations MUST match ourput from sum(larval.diet$n.fish)
larval.diet.full <- do.call(rbind, lapply(1:nrow(larval.diet), function(i) {
  tmp <- larval.diet[i,]
  
  n.rows <- tmp$n.fish
  n.diet.present <- tmp$n.diet
  n.diet.absent <- n.rows - n.diet.present
  
  df <- data.frame(trawl = factor(rep(tmp$trawl, n.rows)), tl.bin = rep(tmp$tl.bin, n.rows),
                   loc.bin.id = rep(tmp$loc.bin.id, n.rows), diet.logical = c(rep("Food Present", n.diet.present), rep("Food Absent", n.diet.absent)))
}))


## LENGTH/YOLK DATA MANIPULATION ================================

## Calculate the mean total length for each trawl
larval.tl.summary <- larval.tl %>% group_by(trawl, tl.bin, yolk.cond) %>% 
  summarize(n.tl = n()) %>% ungroup() %>% 
  mutate(trawl.bin = paste0(trawl, "-", tl.bin))

## Find the length bins that have more than one yolk-sac condition 
##  and remove from analysis. Convert to a vector for filtering.
larval.tl.yolk <- larval.tl.summary %>% group_by(trawl.bin) %>% 
  summarize(nrows = n()) %>%
  filter(nrows == 1) %>% 
  pull(trawl.bin)

## Filter by trawl-bin vector (only length bins that have a single yolk-sac condition)
larval.tl.summary %<>% filter(trawl.bin %in% c(larval.tl.yolk))


## LENGTH/YOLK AND DIET DATA MANIPULATION =======================

## Join, remove NAs, and calculate n for each length bin
larval.yolk.diet <- left_join(larval.diet.full, larval.tl.summary) %>% 
  filter(!is.na(yolk.cond)) %>% 
  mutate(trawl = factor(trawl),
         diet.logical = factor(diet.logical)) %>% 
  group_by(tl.bin) %>% 
  mutate(n.tl = n())
  ## Sample size decreases from 623 to 163 individuals :(


## Rename yolk-sac conditions and combine oil globule,
##  convert to a ordered factor, and create a combined group variable
larval.yolk.diet$yolk.cond <- gsub("Yolk sac and globule", "Yolk Sac", larval.yolk.diet$yolk.cond)
larval.yolk.diet$yolk.cond <- gsub('Oil globule only', 'Yolk Sac', larval.yolk.diet$yolk.cond)
larval.yolk.diet %<>% mutate(yolk.cond = factor(yolk.cond, ordered = TRUE, levels = c('Yolk Sac', 'Absorbed'))) %>% 
  left_join(effort) %>% 
  dplyr::select(-trawl, -loc.bin.id, -trawl.bin) %>% 
  mutate(group = factor(interaction(yolk.cond:diet.logical)))

## Create df with length bin sample size for plotting later
larval.yolk.n <- larval.yolk.diet %>% dplyr::select(tl.bin, n.tl) %>% 
  distinct()


## CALCULATE PROBABILITIES ======================================

## Calculate the probility of each "group" by length bin,
##  rename the "group" to simplified versions for plotting.
larval.yolk.diet.all.prob <- larval.yolk.diet %>% group_by(tl.bin, group) %>% 
  summarize(n = n()) %>% ungroup %>% 
  group_by(tl.bin) %>% 
  mutate(sum = sum(n),
         prob = n/sum) %>% ungroup() %>% 
  mutate(group = gsub(":", ", ", group),
         group = ifelse(group == "Yolk Sac, Food Absent", "Yolk Sac - No Food    ",
                 ifelse(group == "Yolk Sac, Food Present", "Yolk Sac - Food    ",
                 ifelse(group == "Absorbed, Food Absent", "No Yolk Sac - No Food    ",
                 ifelse(group == "Absorbed, Food Present", "No Yolk Sac - Food", "")))),
         group = factor(group, ordered = TRUE, levels = c("Yolk Sac - No Food    ", "Yolk Sac - Food    ",
                                                          "No Yolk Sac - No Food    ", "No Yolk Sac - Food")))


## VISUALIZATION ================================================

ggplot(larval.yolk.diet.all.prob, aes(x = tl.bin, y = prob, fill = group)) + 
  geom_bar(stat = "identity", width = 0.8, color = "black") +
  scale_x_continuous(limits = c(8.5, 19.5), breaks = seq(9, 19, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), expand = c(0, 0)) +
  #scale_fill_manual(values = c("#fdb863", "#e66101", "#b2abd2", "#5e3c99")) +
  scale_fill_manual(values = c("#f7f7f7", "#cccccc", "#969696", "#636363")) +
  #scale_fill_manual(values = c("#636363", "#969696", "#cccccc", "#f7f7f7")) +
  geom_text(data = larval.yolk.n, aes(x = tl.bin, y = 0.02, label = paste0("n=", n.tl)), size = 4, inherit.aes = FALSE) +
  labs(x = "Length Bin (mm)", y = "Probability", fill = "") +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        strip.text = element_blank(), 
        axis.ticks.length = unit(2, 'mm'),
        axis.text.y = element_text(size = 16, colour = "black"),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.title.y = element_text(size = 23, margin = margin(0, 20, 0, 0)),
        axis.title.x = element_text(size = 23, margin = margin(20, 0, 0, 0)),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.75, 'cm'),
        panel.spacing = unit(2, "lines"), legend.position = "top",
        plot.margin = unit(c(8, 5, 5, 5), "mm"))

## Save figure
ggsave("figures/apis_larval_yolkDiet_all.png", width = 12, height = 8, dpi = 300)


## SUPPLEMENTAL ========================================================

## Histogram to see sample size distribution
ggplot(larval.yolk.diet, aes(x = tl.bin, fill = group)) + 
  geom_histogram(binwidth = 1, color = "black") +
  scale_fill_manual(values = c("#e66101", "#fdb863", "#5e3c99", "#b2abd2")) +
  facet_wrap(~week, ncol = 2, dir = "v")

