##############################################################
##############################################################
##  
##  APIS Cisco (Lucke et al.) manuscript
##  
##  
##  Problems:
##  - No individual diet data. No direct comparison between fish that were examined for diet content and measured.
##  - If multiple yolk sac conditions are present within each length bin, you cannot use because no individual data
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
larval.tl <- read_excel("data/Larval_Coregonus_Processing.xlsx", sheet = "Length_Condition") %>% 
  filter(Include == "Y") %>% 
  mutate(Trawl = factor(Trawl),
         Week = factor(Week),
         TLength_mm = as.numeric(TLength_mm)) %>% 
  select(trawl = Trawl, serial = Serial, week = Week, fish.id = Label, tl.mm = TLength_mm, tl.bin = Length_Bin, 
         yolk.cond = Yolk_Sac_Cond)

larval.diet <- read_excel("data/Larval_Coregonus_Processing.xlsx", sheet = "Stomach_Contents")[-265,] %>% 
  mutate(Trawl = factor(Trawl),
         tl.bin = gsub('-', '', substr(Label, 7, length(Label))),
         tl.bin = as.numeric(gsub('mm', '', tl.bin))) %>%
  select(trawl = Trawl, tl.bin, loc.bin.id = Label, n.fish = InBin, n.diet = WithFood, diet.count = Total_food_items) %>% 
  mutate(mean.diet.count = diet.count/n.diet,
         mean.diet.count = ifelse(mean.diet.count == "NaN", 0, mean.diet.count))


## ===========================================================
## Calculate the mean total length for each trawl ============
## ===========================================================
larval.tl.summary <- larval.tl %>% group_by(trawl, tl.bin, yolk.cond) %>% 
  summarize(n.tl = n(),
            mean.tl = mean(tl.mm)) %>% ungroup() %>% 
  mutate(trawl.bin = paste0(trawl, "-", tl.bin))

## -----------------------------------------------------------
## Find the length bins that have more than one yolk-sac condition and remove from analysis
## -----------------------------------------------------------
larval.tl.yolk <- larval.tl.summary %>% group_by(trawl.bin) %>% 
  summarize(nrows = n()) %>%
  filter(nrows == 1) %>% 
  ## convert to a vector
  pull(trawl.bin)

## Filter by trawl-bin vector (only length bins that have a single yolk-sac condition)
larval.tl.summary <- larval.tl.summary %>% filter(trawl.bin %in% c(larval.tl.yolk))


## ===========================================================
## Join length/yolk and diet data ============================
## ===========================================================
larval.yolk.diet <- left_join(larval.diet, larval.tl.summary) %>% 
  filter(!is.na(yolk.cond)) %>% 
  mutate(yolk.cond = factor(yolk.cond))


## ===========================================================
## Fit model (mean.diet.count ~ tl.bin + yolk.cond)
## ===========================================================
lm_yolk <- lm(mean.diet.count ~ tl.bin * yolk.cond, data = larval.yolk.diet)
summary(lm_yolk)

##------------------------------------------------------------
## Check Assumptions
##------------------------------------------------------------
##  Normality
shapiro.test(filter(larval.yolk.diet, yolk.cond == "Yolk sac and globule")$mean.diet.count)   ## p < 0.001; fail normality
shapiro.test(filter(larval.yolk.diet, yolk.cond == "Oil globule only")$mean.diet.count)     ## p = 0.005; fail normality
shapiro.test(filter(larval.yolk.diet, yolk.cond == "Absorbed")$mean.diet.count)             ## p < 0.001; fail normality

## Homogeneity of Variance by yolk
## Flinger-Killeen Test is a non-parametric test which is very robust against departures from normality.
fligner.test(mean.diet.count ~ yolk.cond, data = larval.yolk.diet)  ## p < 0.001; fail equal variance

##------------------------------------------------------------
## ANOVA
##------------------------------------------------------------
kruskal.test(larval.yolk.diet$mean.diet.count, larval.yolk.diet$yolk.cond)

pairwise.wilcox.test(larval.yolk.diet$mean.diet.count, larval.yolk.diet$yolk.cond, p.adjust.method = "BH")



## ===========================================================
## Rename yolk-sac conditions
## ===========================================================
larval.yolk.diet$yolk.cond <- gsub("Yolk sac and globule", "Yolk Sac Present", larval.yolk.diet$yolk.cond)
larval.yolk.diet$yolk.cond <- gsub('Absorbed', 'Absorbed', larval.yolk.diet$yolk.cond)
larval.yolk.diet$yolk.cond <- gsub('Oil globule only', 'Oil Globule Only', larval.yolk.diet$yolk.cond)

larval.yolk.diet %<>% mutate(yolk.cond = factor(yolk.cond, ordered = TRUE, levels = c('Yolk Sac Present', 'Oil Globule Only', 'Absorbed')))


## ===========================================================
## Visualization =============================================
## ===========================================================
ggplot(larval.yolk.diet, aes(x = tl.bin, y = mean.diet.count, fill = yolk.cond)) +
  geom_point(position = position_jitter(width = 0.175), shape = 21, color = "black", size = 2) +
  stat_smooth(method = "lm", se = FALSE, aes(color = yolk.cond), show.legend  = FALSE) +
  scale_x_continuous(limits = c(5, 26.55), breaks = seq(5, 25, 5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-7, 354), breaks = seq(0, 350, 50), expand = c(0, 0)) +
  scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a")) +
  scale_color_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a")) +
  labs(x = "Length Bin (mm)", y = "Average No. of Diet Items", fill = "Yolk-sac Condition") +
  theme(panel.grid = element_blank(), panel.background = element_blank(),
        panel.spacing = unit(1, "lines"), axis.line = element_line(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 25, margin = margin(0, 15, 0, 0)),
        axis.title.x = element_text(size = 25, margin = margin(15, 0, 0, 0)),
        axis.ticks.length = unit(2, 'mm'), legend.position = c(0.15, 0.85),
        legend.text = element_text(size = 13), legend.title = element_text(size = 15),
        legend.key = element_rect(fill = "white"),
        plot.margin = unit(c(5, 5, 5, 5), "mm"))

## Save figure
ggsave("figures/apis_larval_length_diet.png", dpi = 300, width = 10, height = 6)

