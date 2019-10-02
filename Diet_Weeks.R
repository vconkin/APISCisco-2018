##  
##  APIS Cisco (Lucke et al.) manuscript
##  
## This file calculates the average diet for larvae that had
## food in the gut at time of capture, broken down into the
## five most abundant prey groups by numbers. This also includes
## calculations for average diet for fish that fed during the
## first and last two weeks of sampling
## 


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

diet.cont <- read_excel("data/APIS_Coregonus_2018.xlsx", sheet = "Larval_Diet") %>%
  mutate(trawl = as.numeric(ifelse(trawl == "37.1", "37", trawl)))
effort <- read_excel("data/APIS_Coregonus_2018.xlsx", sheet = "Neuston Effort") %>% 
  select(trawl, week) %>%
  mutate(trawl = as.numeric(ifelse(trawl == "37.1", "37", trawl)))


## Diet Content Data Prep ====================================

## Calculate sample sizes (no. of larvae with food) by week - save for plotting
diet.sample.size <- left_join(diet.cont, effort) %>% 
  group_by(week) %>% 
  filter(diet.count >0) %>%
  summarize(n = sum(n.diet),
            n.trawl = n_distinct(trawl))

## Restrict DF to the selected variables and rename variables
diet.cont.species <- diet.cont %>% select(trawl, Nauplii:Chironomid_pupae) %>% 
  rename("Acanthocyclops" = "Acanthocyclops_sp.", "Eucyclops" = "Eucyclops_sp.",
         "Holopedium" = "Holopedium_gibberum", "Daphnia" = "Daphnia_sp.", 
         "Bosmina" = "Bosmina_sp.")

## Make all NAs (blanks) zero
diet.cont.species[is.na(diet.cont.species)] <- 0

## Condense all food items down into types
diet.comp <- diet.cont.species %>% mutate("Calanoid Copepodid" = Cal_Copepodite,
                                  Calanoidae = L.minutus + L.sicilis + Limnocalanus_macrurus + 
                                    E.lacustrus + Senecella_calanoides + Unknown_Fragment_Cyclopoid,
                                  "Other" = Daphnia + Bosmina + Invertebrate_eggs +
                                    Chironomid_pupae + Rotifera + Bythotrephes + Diaphanosoma + 
                                    Leptodora_kindi + Cyc_Copepodite +Acanthocyclops + Diacyclops_thomasi + 
                                    Eucyclops + Unknown_Fragment_Cyclopoid) %>%
  select(-Acanthocyclops, -Diacyclops_thomasi, -Eucyclops, -Cyc_Copepodite,
         -L.minutus, -L.sicilis, -Limnocalanus_macrurus, -E.lacustrus, 
         -Senecella_calanoides, -Cal_Copepodite, -Unknown_Fragment_Calanoid, 
         -Unknown_Fragment_Cyclopoid, -Invertebrate_eggs, -Chironomid_pupae, 
         -Rotifera, -Daphnia, -Bosmina, -Bythotrephes, -Diaphanosoma, -Leptodora_kindi) %>%
  gather(species, diet.count, Nauplii:Other) %>% droplevels()

## Create a list of diet taxa and trawl numbers
species.list <- unique(unique(diet.comp$species))

##Group by weeks and summarize
diet.comp.weeks <- left_join(diet.comp, effort, by = "trawl") %>%
  group_by(week, species) %>%
  summarize(avg.diet = mean(diet.count),
            sd.diet = sd(diet.count)) %>% ungroup() %>% 
  complete(week, species = species.list, fill = list(mean.diet = 0, sd.diet = 0)) %>% 
  left_join(diet.sample.size) %>% 
  mutate(se.diet = sd.diet / sqrt(n.trawl)) %>%
  group_by(week) %>%
  mutate(perc = (avg.diet/sum(avg.diet)*100),
         perc = ifelse(perc =="NaN", 0, perc)) %>% ungroup()

diet.comp.weeks.avg<-left_join(diet.comp, effort, by = "trawl") %>%
  group_by(week, species) %>%
  summarize(avg.diet = mean(diet.count),
            sd.diet = sd(diet.count)) %>% ungroup() %>% 
  complete(week, species = species.list, fill = list(mean.diet = 0, sd.diet = 0)) %>% 
  left_join(diet.sample.size) %>% 
  mutate(se.diet = sd.diet / sqrt(n.trawl)) %>% ungroup()
  

## Plot type percent for fish with food
ggplot(diet.comp.weeks, aes(x = week, y = perc, fill = species )) +
  geom_bar(stat = "identity", color = "black", width = 1) +
  scale_x_continuous(name = "Week", limits = c(19.5, 30.5), breaks = seq(20, 30, 1), expand = c(0, 0))+
  scale_y_continuous(name ="Percent Composition of Diet by Item Type", expand = c(0,0)) +
  scale_fill_manual(name = "Prey Group", values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        strip.text = element_blank(), 
        axis.ticks.length = unit(2, 'mm'),
        legend.title =  element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.title.y = element_text(size = 23, margin = margin(0, 20, 0, 0)),
        axis.title.x = element_text(size = 23, margin = margin(20, 0, 0, 0)),
        legend.text = element_text(size = 14),
        legend.key.size = unit(0.75, 'cm'),
        panel.spacing = unit(2, "lines"),
        plot.margin = unit(c(8, 5, 5, 5), "mm"))

ggsave("figures/apis_diet_percent.png", width = 12, height = 8, dpi = 300)

ggplot(diet.comp.weeks.avg, aes(x = week, y = avg.diet, fill = species )) +
  geom_bar(stat = "identity", color = "black", width = 1) +
  scale_x_continuous(name = "Week", limits = c(19.5, 30.5), breaks = seq(20, 30, 1), expand = c(0, 0))+
  scale_y_continuous(name ="No. of Prey Items by Type", expand = c(0,0)) +
  scale_fill_manual(name = "Prey Group", values = c("#f7f7f7", "#cccccc", "#969696", "#636363", "#252525")) +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        strip.text = element_blank(), 
        axis.ticks.length = unit(2, 'mm'),
        legend.title =  element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.title.y = element_text(size = 23, margin = margin(0, 20, 0, 0)),
        axis.title.x = element_text(size = 23, margin = margin(20, 0, 0, 0)),
        legend.text = element_text(size = 14),
        legend.key.size = unit(0.75, 'cm'),
        panel.spacing = unit(2, "lines"),
        plot.margin = unit(c(8, 5, 5, 5), "mm"))

ggsave("figures/apis_average_diet.png", width = 12, height = 8, dpi = 300)

## Calculate average diet for fish that fed in first and last two weeks
diet.comp.weeks<-left_join(diet.cont, effort, by = "trawl") %>%
  select(week, trawl, n.fish, n.diet, diet.count) %>%
  filter(week <= 21 | week >= 29, diet.count >0) %>%
  mutate(week = ifelse(week <= 21,  "first", "last")) %>%
  group_by(week) %>%
  summarize(sum.diet = sum(diet.count),
            n.diet = sum(n.diet),
            avg.diet = (sum.diet/n.diet),
            sd.diet = sd(diet.count)) %>% ungroup() 


## Sample size for first and last two weeks
avg.diet.sample.size <- diet.sample.size %>% filter(week <=21 | week >=29) %>%
  mutate(week = (ifelse(week <= "21", "first", "last"))) %>%
  group_by(week) %>%
  summarize(n.trawl = sum(n.trawl))

avg.diet.weeks <- diet.comp.weeks %>% 
  left_join(avg.diet.sample.size, by = "week") %>% 
  mutate(se.diet = sd.diet / sqrt(n.trawl)) %>% ungroup()
