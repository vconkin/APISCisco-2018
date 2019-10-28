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
library(ggpubr)        # visualizations
library(lemon)         # for facet_rep_wrap()


## Load in the data ==========================================

diet.cont <- read_excel("data/APIS_Coregonus_2018.xlsx", sheet = "Larval_Diet") %>%
  mutate(trawl = as.numeric(ifelse(trawl == "37.1", "37", trawl)))
effort <- read_excel("data/APIS_Coregonus_2018.xlsx", sheet = "Neuston Effort") %>% 
  select(trawl, week) %>%
  mutate(trawl = as.numeric(ifelse(trawl == "37.1", "37", trawl)))


## Diet Content Data Prep ====================================

## Restrict DF to the selected variables and rename variables
diet.cont.species <- diet.cont %>% select(trawl, tl.bin, Nauplii:Chironomid_pupae) %>% 
  rename("Acanthocyclops" = "Acanthocyclops_sp.", "Eucyclops" = "Eucyclops_sp.",
         "Holopedium" = "Holopedium_gibberum", "Daphnia" = "Daphnia_sp.", 
         "Bosmina" = "Bosmina_sp.")

## Make all NAs (blanks) zero
diet.cont.species[is.na(diet.cont.species)] <- 0

## Condense all food items down into types
diet.comp <- diet.cont.species %>% mutate("Calanoid copepodid" = Cal_Copepodite,
                                          "Cyclopoid copepodid" = Cyc_Copepodite,
                                          Calanoidae = L.minutus + L.sicilis + Limnocalanus_macrurus + 
                                            E.lacustrus + Senecella_calanoides + Unknown_Fragment_Cyclopoid,
                                          Cyclopoidae = Cyc_Copepodite +Acanthocyclops + Diacyclops_thomasi + 
                                            Eucyclops + Unknown_Fragment_Cyclopoid,
                                          "Other" = Daphnia + Bosmina + Invertebrate_eggs + Chironomid_pupae + 
                                            Rotifera + Bythotrephes + Diaphanosoma + Leptodora_kindi) %>%
  select(-Acanthocyclops, -Diacyclops_thomasi, -Eucyclops, -Cyc_Copepodite,
         -L.minutus, -L.sicilis, -Limnocalanus_macrurus, -E.lacustrus, 
         -Senecella_calanoides, -Cal_Copepodite, -Unknown_Fragment_Calanoid, 
         -Unknown_Fragment_Cyclopoid, -Invertebrate_eggs, -Chironomid_pupae, 
         -Rotifera, -Daphnia, -Bosmina, -Bythotrephes, -Diaphanosoma, -Leptodora_kindi) %>%
  gather(species, diet.count, Nauplii:Other) %>% droplevels()

## ABBREVIATE TAXA NAMES ========================================

diet.comp$species <- gsub('Cyclopoidae', "CY", diet.comp$species)
diet.comp$species <- gsub('Cyclopoid copepodid', "CY*", diet.comp$species)
diet.comp$species <- gsub('Calanoidae', "CA", diet.comp$species)
diet.comp$species <- gsub('Calanoid copepodid', "CA*", diet.comp$species)
diet.comp$species <- gsub('Holopedium', "HO", diet.comp$species)
diet.comp$species <- gsub('Nauplii', "NA", diet.comp$species)
diet.comp$species <- gsub('Other', "OT", diet.comp$species)


## Create a list of diet taxa and trawl numbers
species.list <- unique(unique(diet.comp$species))

## Calculate sample sizes (no. of larvae with food) by week - save for plotting
diet.sample.size <- left_join(diet.cont, effort) %>% 
  mutate(week = as.factor(ifelse(week < 25, "first", "last"))) %>%
  group_by(week, tl.bin) %>% 
  summarize(n = sum(n.diet),
            n.trawl = n_distinct(trawl))

lengths<- c(6:26)

##Group by weeks and summarize
diet.comp.length <- left_join(diet.comp, effort, by = "trawl") %>%
  mutate(week = as.factor(ifelse(week < 25, "first", "last"))) %>%
  group_by(week, tl.bin, species) %>%
  summarize(diet.total = (sum(diet.count)),
            sd.diet = sd(diet.count)) %>% ungroup() %>% 
  complete(species = species.list, tl.bin = lengths, week = week, fill = list(diet.total = 0, sd.diet = 0)) %>% 
  ## Right join to show only the fish with food in their stomachs 
  left_join(diet.sample.size) %>%
  group_by(tl.bin, week) %>%
  mutate(se.diet = sd.diet / sqrt(n.trawl),
         n = replace_na(n, 0),
         avg.diet = diet.total/n,
         perc = (avg.diet/sum(avg.diet)*100),
         perc = ifelse(perc =="NaN", 0, perc)) %>% ungroup() 


first.lengths <- diet.comp.length %>% filter(week == "first") %>%
  mutate(label = paste0(tl.bin,'\n(', n, ")"), 
         label = factor(label, ordered = TRUE, levels = c("6\n(0)","7\n(0)", "8\n(0)", "9\n(0)", "10\n(6)", "11\n(23)", 
                                                          "12\n(42)","13\n(73)", "14\n(50)", "15\n(33)", "16\n(15)", 
                                                          "17\n(6)", "18\n(3)", "19\n(0)","20\n(0)", "21\n(1)", 
                                                          "22\n(0)", "23\n(0)", "24\n(0)", "25\n(0)", "26\n(0)")))



last.lengths <- diet.comp.length %>% filter(week =="last") %>%
  mutate(label = paste0(tl.bin,'\n(', n, ")"), 
         label = factor(label, ordered = TRUE, levels = c("6\n(1)","7\n(0)", "8\n(3)", "9\n(4)", "10\n(10)", "11\n(12)", 
                                                          "12\n(14)","13\n(19)", "14\n(28)", "15\n(22)", "16\n(22)", 
                                                          "17\n(33)", "18\n(19)", "19\n(18)","20\n(4)", "21\n(11)", 
                                                          "22\n(2)", "23\n(2)", "24\n(0)", "25\n(1)", "26\n(1)")))

## VISUALIZATION ================================================
## Define a colorblind safe(ish) palette for 7-classes
color <- c("gray30", "#e69f00", "#56b4e9", "#009e73", "#f0e442", "#0072b2", "#d55e00", "#cc79a7")


first <- ggplot(first.lengths, aes(x = label, y = perc, fill = species ))+
  geom_bar(stat = "identity", color = "black", width = 1) +
  scale_x_discrete(name = "Length (mm)", expand = c(0, 0)) +
  scale_y_continuous(name ="Percent Diet Composition", expand = c(0,0)) +
  labs(fill ="Prey Group") +
  scale_fill_manual(name = NULL, values = color) +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        strip.text = element_blank(), 
        axis.ticks.length = unit(2, 'mm'),
        legend.title =  element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.title.y = element_text(size = 20, margin = margin(0, 20, 0, 0)),
        axis.title.x = element_text(size = 20, margin = margin(20, 0, 0, 0)),
        legend.text = element_text(size = 14),
        legend.key.size = unit(0.75, 'cm'),
        panel.spacing = unit(2, "lines"),
        plot.margin = unit(c(8, 5, 5, 5), "mm"))

last <- ggplot(last.lengths, aes(x = label, y = perc, fill = species ))+
  geom_bar(stat = "identity", color = "black", width = 1) +
  scale_x_discrete(name = "Length (mm)", expand = c(0, 0)) +
  scale_y_continuous(name ="Percent Diet Composition", expand = c(0,0)) +
  labs(fill ="Prey Group") +
  scale_fill_manual(name = NULL, values = color) +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        strip.text = element_blank(), 
        axis.ticks.length = unit(2, 'mm'),
        legend.title =  element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.title.y = element_text(size = 20, margin = margin(0, 20, 0, 0)),
        axis.title.x = element_text(size = 20, margin = margin(20, 0, 0, 0)),
        legend.text = element_text(size = 14),
        legend.key.size = unit(0.75, 'cm'),
        panel.spacing = unit(2, "lines"),
        plot.margin = unit(c(8, 5, 5, 5), "mm"))


diet.weeks <- ggarrange(first, last, nrow = 2, common.legend = TRUE, legend = "right")

ggsave("figures/apis_diet_length_week.png", plot = diet.weeks, width = 12, height = 9, dpi = 300)

