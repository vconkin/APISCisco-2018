##############################################################
##############################################################
##  
##  APIS Cisco (Lucke et al.) manuscript
##  
## This file combines the Zems data with length and diet data
## making it the last step of the Zems workflow. We start by
## calculating the indices we're interested in, and then we 
## make a few additional plots that combine the diet and 
## Zems data
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


## ===========================================================
## Load in the data ==========================================
## ===========================================================
p.length <- read_excel("data/Larval_Coregonus_Processing.xlsx", sheet = "Length_Condition")
diet.cont <- read_excel("data/Larval_Coregonus_Processing.xlsx", sheet = "Stomach_Mod")[-270,] %>% 
  mutate(trawl = as.integer(Trawl))
envir.prey <- read.csv("data/Superior_Files/Summaries/Zoop_Summary.csv", header = TRUE) %>% 
  mutate(density.l = as.numeric(density.l),
         species = as.character(species)) %>% droplevels()
effort <- read.csv("data/Superior_Files/APIS_Effort_Cut.csv", header = TRUE) %>% 
  select(trawl = Trawl, week = Week)


## ===========================================================
## Diet Content Data Prep ====================================
## ===========================================================
## -----------------------------------------------------------
## Calculate sample sizes (no. of larvae examined) by week - save for plotting
## -----------------------------------------------------------
diet.sample.size <- left_join(diet.cont, effort) %>% 
  group_by(week) %>% 
  summarize(n = sum(InBin),
            n.trawl = n_distinct(trawl))

## -----------------------------------------------------------
## Restrict DF to the selected variables and rename variables
## -----------------------------------------------------------
diet.cont %<>% select(trawl, Nauplii:Chironomid_pupae) %>% 
  rename("Acanthocyclops" = "Acanthocyclops_sp.", "Eucyclops" = "Eucyclops_sp.",
         "Holopedium" = "Holopedium_gibberum", "Daphnia" = "Daphnia_sp.", 
         "Bosmina" = "Bosmina_sp.") %>% 
  ## Remove non-whole organisms
  select(-"Unknown_Fragment_Calanoid", -"Unknown_Fragment_Cyclopoid", -"Invertebrate_eggs", 
         -"Chironomid_pupae", -"Rotifera", -"Leptodora_kindi")

## -----------------------------------------------------------
## Make all NAs (blanks) zero
## -----------------------------------------------------------
diet.cont[is.na(diet.cont)] <- 0

## -----------------------------------------------------------
## Combine minutus and sicilis to genera, then drop the old columns
## Collapse the species columns into two: prey species and count
## -----------------------------------------------------------
diet.comp <- diet.cont %>% mutate(Cyclopidae = Acanthocyclops + Diacyclops_thomasi + Eucyclops + Cyc_Copepodite,
                                  Calanoidae = L.minutus + L.sicilis + Limnocalanus_macrurus + E.lacustrus + Senecella_calanoides + Cal_Copepodite) %>%
  select(-Acanthocyclops, -Diacyclops_thomasi, -Eucyclops, -Cyc_Copepodite,
         -L.minutus, -L.sicilis, -Limnocalanus_macrurus, -E.lacustrus, -Senecella_calanoides, -Cal_Copepodite) %>%
  gather(species, diet.count, Nauplii:Calanoidae) %>% droplevels()


## -----------------------------------------------------------
## Create a list of trawl numbers that have larvae with diet contents
## -----------------------------------------------------------
trawl.list <- unique(diet.comp$trawl)


## ===========================================================
## Zooplankton (Environment) Data Prep =======================
## ===========================================================
## -----------------------------------------------------------
## Restrict DF to the selected variables and rename variables
## -----------------------------------------------------------
envir.prey.filtered <- envir.prey %>% select(trawl, species, density.l) %>% 
  mutate(species = gsub("Leptodiaptomus", "Calanoidae", species),
         species = gsub("Limnocalanus", "Calanoidae", species),
         species = gsub("Epischura", "Calanoidae", species),
         species = gsub("Senecella_calanoides", "Calanoidae", species),
         species = gsub("Cal_Copepodite", "Calanoidae", species),
         species = gsub("Acanthocyclops", "Cyclopidae", species),
         species = gsub("Diacyclops", "Cyclopidae", species),
         species = gsub("Eucyclops", "Cyclopidae", species),
         species = gsub("Mesocyclops", "Cyclopidae", species),
         species = gsub("Cyc_Copepodite", "Cyclopidae", species)) %>% 
  ## remove "empty diet" trawls
  filter(trawl %in% c(trawl.list))

## -----------------------------------------------------------
## Create a list of diet taxa and trawl numbers
## -----------------------------------------------------------
species.list <- unique(c(unique(envir.prey.filtered$species), unique(diet.comp$species)))


## ===========================================================
## Diet Content Proportion ===================================
## ===========================================================
## -----------------------------------------------------------
## Group the data by trawl number and species to sum species counts for each trawl
## IMPORTANT: New DF's number of obs. must match the no. of trawls (66) times the no. of species (16)!
## -----------------------------------------------------------
diet.cont.species <- diet.comp %>% group_by(trawl, species)%>%
  summarize(diet.count.species = sum(diet.count)) %>%
  ungroup()

## -----------------------------------------------------------
## Group by trawl to sum total prey counts for each trawl
## -----------------------------------------------------------
diet.cont.total <- diet.comp %>% group_by(trawl) %>% 
  summarize(diet.count.trawl.total = sum(diet.count))

## -----------------------------------------------------------
## Join the total counts and species specific counts to calculate proportion of diet for each trawl
##  If proportion is NA, replace with zero
## -----------------------------------------------------------
diet.cont.prop <- full_join(diet.cont.species, diet.cont.total) %>%
  mutate(diet.prop = diet.count.species/diet.count.trawl.total,
         diet.prop = ifelse(is.na(diet.prop) == TRUE, 0, diet.prop)) %>% 
  select(trawl, species, diet.prop)

## -----------------------------------------------------------
## Clean up environment
## -----------------------------------------------------------
rm(diet.comp, diet.cont, diet.cont.species, diet.cont.total)


## ===========================================================
## Zooplankton (Environment) Proportion ======================
## ===========================================================
## -----------------------------------------------------------
## Create a loop function to add zeros for all prey taxa missing that was found in diet
## -----------------------------------------------------------
## Apply loop function
envir.prey.missing <-  data.frame(do.call(rbind, lapply(trawl.list, function(i) {
  envir.prey.trawl <- envir.prey.filtered %>% filter(trawl == i)
  ## True/false output if prey does not exist (zero value species)
  pl <- species.list[!species.list %in% envir.prey.trawl$species]
  ## Determine the number of life stages to be added
  n <- length(pl)
  ## Create data frame with all zero value life stages, repeat by 'n'
  data.frame(trawl = rep(i, n), species = pl, density.l = rep(0, n))
})))

## -----------------------------------------------------------
## Combine data and missing taxa
## -----------------------------------------------------------
envir.prey.all <- bind_rows(envir.prey.filtered, envir.prey.missing)

## -----------------------------------------------------------
## Group the data by trawl number and species to sum species counts for each trawl
## IMPORTANT: New DF's number of obs. must match the no. of trawls (66) times the no. of species (16)!
## -----------------------------------------------------------
envir.prey.species <- envir.prey.all %>% group_by(trawl, species)%>%
  summarize(prey.count.species = sum(density.l)) %>%
  ungroup()

## -----------------------------------------------------------
## Group by trawl to sum total prey counts for each trawl
## -----------------------------------------------------------
envir.prey.total <- envir.prey.all %>% group_by(trawl) %>% 
  summarize(prey.count.trawl.total = sum(density.l))

## -----------------------------------------------------------
## Join the total counts and species specific counts to calculate proportion of diet for each trawl
##  If proportion is NA, replace with zero
## -----------------------------------------------------------
envir.prey.prop <- full_join(envir.prey.species, envir.prey.total) %>% 
  mutate(envir.prop = prey.count.species / prey.count.trawl.total,
         envir.prop = ifelse(is.na(envir.prop) == TRUE, 0, envir.prop)) %>% 
  select(trawl, species, envir.prop)

## -----------------------------------------------------------
## Clean up environment
## -----------------------------------------------------------
rm(envir.prey, envir.prey.total, envir.prey.species, envir.prey.all, envir.prey.filtered, envir.prey.missing)


## ===========================================================
## Electivity Calculations ===================================
## ===========================================================
## -----------------------------------------------------------
## Combine diet and environment proportions, 
## filter out only data that has a zero in both diet and environment,
## and calculate diet:envir ratio
## -----------------------------------------------------------
larval.diet.env.prop <- left_join(diet.cont.prop, envir.prey.prop) %>%
  mutate(diet.envir = diet.prop / envir.prop)

## -----------------------------------------------------------
## Remove all infinate values (created by a zero divisor) and calculate alpha ((r/p)/sum(r/p))
## -----------------------------------------------------------
larval.diet.env.prop.woInf <- larval.diet.env.prop %>% filter(diet.envir != Inf) %>% 
  group_by(trawl) %>% 
  mutate(alpha = diet.envir / sum(diet.envir),
         alpha = ifelse(is.na(alpha) == TRUE, 0, alpha))

## -----------------------------------------------------------
## and manually assign alpha as 1 (assumes they consumed all available prey)
## -----------------------------------------------------------
larval.diet.env.prop.wInf <- larval.diet.env.prop %>% filter(diet.envir == Inf) %>% 
  mutate(alpha = 1)

## -----------------------------------------------------------
## Combine non-Inf and Inf DFs
## -----------------------------------------------------------
larval.diet.env.prop.all <- bind_rows(larval.diet.env.prop.woInf, larval.diet.env.prop.wInf) %>% 
  mutate(E = (alpha - (1 / length(species.list))) / (alpha + (1 / length(species.list)))) %>% 
  left_join(effort)


## ===========================================================
## Average the Selectivity Values ============================
## ===========================================================
larval.selectivity.week <- left_join(effort, larval.diet.env.prop.all) %>%
  group_by(week, species) %>%
  summarize(mean.alpha = mean(alpha), 
            mean.E = mean(E),
            sd.alpha = sd(alpha),
            sd.E = sd(E)) %>% ungroup() %>% 
  filter(!is.na(species)) %>% 
  complete(week, species = species.list, fill = list(mean.alpha = 0, mean.E = 0, sd.alpha = 0, sd.E = 0)) %>% 
  left_join(diet.sample.size) %>% 
  mutate(se.alpha = sd.alpha / sqrt(n.trawl),
         se.E = sd.E / sqrt(n.trawl),
         week = paste0("Week ", week, ": n=", n))


## -----------------------------------------------------------
## abbreviate taxa
## -----------------------------------------------------------
larval.selectivity.week$species <- gsub('Cyclopidae', 'CY', larval.selectivity.week$species)
larval.selectivity.week$species <- gsub('Bosmina', 'BO', larval.selectivity.week$species)
larval.selectivity.week$species <- gsub('Bythotrephes', 'BY', larval.selectivity.week$species)
larval.selectivity.week$species <- gsub('Daphnia', 'DA', larval.selectivity.week$species)
larval.selectivity.week$species <- gsub('Diaphanosoma', 'DI', larval.selectivity.week$species)
larval.selectivity.week$species <- gsub('Calanoidae', 'CA', larval.selectivity.week$species)
larval.selectivity.week$species <- gsub('Holopedium', 'HO', larval.selectivity.week$species)
larval.selectivity.week$species <- gsub('Nauplii', 'NA', larval.selectivity.week$species)

  
## -----------------------------------------------------------
## Find all weeks and prey that are NA - creates the DF for plotting "nf"
## -----------------------------------------------------------
larval.selectivity.week.zero <- larval.selectivity.week %>% 
  filter(mean.E == 0)


## ===========================================================
## Plot Electivity Trends ====================================
## ===========================================================
ggplot(larval.selectivity.week, aes(x = species, y = mean.alpha, group = species)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean.alpha + se.alpha, ymax = mean.alpha - se.alpha), width = 0.5) +
  geom_hline(yintercept = 0) +
  geom_text(data = larval.selectivity.week.zero, aes(x = species, y = 0.0375), label = "nf", size = 3) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0))+
  labs(x = "Prey Taxa", y = "Selectivity Index (W')") +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.background = element_blank(),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 25, margin = margin(0, 18, 0, 0)),
        axis.title.x = element_text(size = 25, margin = margin(15, 0, 0, 0)),
        axis.ticks.length = unit(2, 'mm'),
        strip.text = element_text(size = 10)) +
  facet_wrap(~week, dir = "v", ncol = 3)

ggsave("figures/apis_larval_selectivity_weekly.png", dpi = 300, width = 10, height = 10)


ggplot(larval.selectivity.week, aes(x = species, y = mean.E, group = species)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean.E + se.E, ymax = mean.E - se.E), width = 0.5) +
  geom_hline(yintercept = 0) +
  geom_text(data = larval.selectivity.week.zero, aes(x = species, y = 0.075), label = "nf", size = 3) +
  scale_y_continuous(limits = c(-1,1), expand = c(0, 0))+
  labs(x = "Prey Taxa", y = expression(paste("Electivity Index (", E["i"]^"*", ")", sep = ""))) +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.background = element_blank(),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 25, margin = margin(0, 15, 0, 0)),
        axis.title.x = element_text(size = 25, margin = margin(15, 0, 0, 0)),
        axis.ticks.length = unit(2, 'mm'),
        strip.text = element_text(size = 10)) +
  facet_wrap(~week, dir = "v", ncol = 3)

ggsave("figures/apis_larval_electivity_weekly.png", dpi = 300, width = 10, height = 10)

