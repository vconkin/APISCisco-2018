#################################################################
##  
##  APIS Cisco (Lucke et al.) manuscript
##  
## This file combines the Zems data with length and diet data
## making it the last step of the Zems workflow. We start by
## calculating the indices we're interested in, and then we 
## make a few additional plots that combine the diet and 
## Zems data
## 
#################################################################

## Clear the environment first ==================================

rm(list = ls(all.names=TRUE))


## Load Packages ================================================

library(readxl)        # reading Excel data
library(dplyr)         # manipulating data
library(magrittr)      # for %<>%
library(tidyr)         # transforming data arrangement (tidy data!!)
library(ggplot2)       # visualizations
library(lemon)         # for facet_rep_wrap()


## Load in the data =============================================

diet.cont <- read_excel("data/APIS_Coregonus_2018.xlsx", sheet = "Larval_Diet")
envir.prey <- read.csv("data/APIS_Zooplankton_2018.csv", header = TRUE)
effort <- read_excel("data/APIS_Coregonus_2018.xlsx", sheet = "Neuston Effort") %>% 
  select(trawl, week)


## Diet Content Data Prep =======================================

## Calculate sample sizes (no. of larvae examined) by week - save for plotting
diet.sample.size <- left_join(diet.cont, effort) %>% 
  group_by(week) %>% 
  summarize(n = sum(n.fish),
            n.trawl = n_distinct(trawl))

## Restrict DF to the selected variables and rename variables
diet.cont %<>% select(trawl, Nauplii:Chironomid_pupae) %>% 
  rename("Acanthocyclops" = "Acanthocyclops_sp.", "Eucyclops" = "Eucyclops_sp.",
         "Holopedium" = "Holopedium_gibberum", "Daphnia" = "Daphnia_sp.") %>% 
  ## Remove non-whole organisms
  select(-"Unknown_Fragment_Calanoid", -"Unknown_Fragment_Cyclopoid", -"Invertebrate_eggs", 
         -"Chironomid_pupae", -"Rotifera") %>% 
  ## remove low density organisms
  select(-Bythotrephes, -Leptodora_kindi, -Diaphanosoma, -"Bosmina_sp.")

## Make all NAs (blanks) zero
diet.cont[is.na(diet.cont)] <- 0

## Combine minutus and sicilis to genera, then drop the old columns
## Collapse the species columns into two: prey species and count
diet.comp <- diet.cont %>% mutate(Cyclopidae = Acanthocyclops + Diacyclops_thomasi + Eucyclops,
                                  "Calanoid Copepodid" = Cal_Copepodite,
                                  "Cyclopoid Copepodid" = Cyc_Copepodite,
                                  Calanoidae = L.minutus + L.sicilis + Limnocalanus_macrurus + E.lacustrus + Senecella_calanoides + Cal_Copepodite) %>%
  select(-Acanthocyclops, -Diacyclops_thomasi, -Eucyclops, -Cyc_Copepodite,
         -L.minutus, -L.sicilis, -Limnocalanus_macrurus, -E.lacustrus, -Senecella_calanoides, -Cal_Copepodite) %>%
  gather(species, diet.count, Nauplii:Calanoidae) %>% droplevels()


## Create a list of trawl numbers that have larvae with diet contents
trawl.list <- unique(diet.comp$trawl)


## Zooplankton (Environment) Data Prep ==========================

## Restrict DF to the selected variables and rename variables
envir.prey.filtered <- envir.prey %>% select(trawl, species, density.l) %>% 
  mutate(species = gsub("Leptodiaptomus", "Calanoidae", species),
         species = gsub("Limnocalanus", "Calanoidae", species),
         species = gsub("Epischura", "Calanoidae", species),
         species = gsub("Senecella_calanoides", "Calanoidae", species),
         species = gsub("Acanthocyclops", "Cyclopidae", species),
         species = gsub("Diacyclops", "Cyclopidae", species),
         species = gsub("Eucyclops", "Cyclopidae", species),
         species = gsub("Mesocyclops", "Cyclopidae", species),
         species = gsub("Cyclopoid copepodid", "Cyclopoid Copepodid", species),
         species = gsub("Calanoid copepodid", "Calanoid Copepodid", species)) %>% 
  ## remove "empty diet" trawls
  filter(trawl %in% c(trawl.list),
         species != "Bythotrephes",
         species != "Diaphanosoma",
         species != "Bosmina")


## Create a list of diet taxa and trawl numbers
species.list <- unique(c(unique(envir.prey.filtered$species), unique(diet.comp$species)))


## Diet Content Proportion ======================================

## Group the data by trawl number and species to sum species counts for each trawl
## IMPORTANT: New DF's number of obs. must match the no. of trawls (81) times the no. of species (11)!

diet.cont.species <- diet.comp %>% group_by(trawl, species)%>%
  summarize(diet.count.species = sum(diet.count)) %>%
  ungroup()

## Group by trawl to sum total prey counts for each trawl
diet.cont.total <- diet.comp %>% group_by(trawl) %>% 
  summarize(diet.count.trawl.total = sum(diet.count))

## Join the total counts and species specific counts to calculate proportion of diet for each trawl
##  If proportion is NA, replace with zero

diet.cont.prop <- full_join(diet.cont.species, diet.cont.total) %>%
  mutate(diet.prop = diet.count.species/diet.count.trawl.total,
         diet.prop = ifelse(is.na(diet.prop) == TRUE, 0, diet.prop)) %>% 
  select(trawl, species, diet.prop)

## Clean up environment
rm(diet.comp, diet.cont, diet.cont.species, diet.cont.total)


## Zooplankton (Environment) Proportion =========================

## Add zeros for all prey taxa missing in the environment that was found in diet
envir.prey.all <- envir.prey.filtered %>% 
  complete(trawl = trawl.list, species = species.list, fill = list(density.l = 0)) 
  
## Group the data by trawl number and species to sum species counts for each trawl
## IMPORTANT: New DF's number of obs. must match the no. of trawls (81) times the no. of species (11)!

envir.prey.species <- envir.prey.all %>% group_by(trawl, species)%>%
  summarize(prey.count.species = sum(density.l)) %>% ungroup()

## Group by trawl to sum total prey counts for each trawl
envir.prey.total <- envir.prey.all %>% group_by(trawl) %>% 
  summarize(prey.count.trawl.total = sum(density.l))

## Join the total counts and species specific counts to calculate proportion of diet for each trawl
##  If proportion is NA, replace with zero
envir.prey.prop <- full_join(envir.prey.species, envir.prey.total) %>% 
  mutate(envir.prop = prey.count.species / prey.count.trawl.total,
         envir.prop = ifelse(is.na(envir.prop) == TRUE, 0, envir.prop)) %>% 
  select(trawl, species, envir.prop)

## Clean up environment
rm(envir.prey, envir.prey.total, envir.prey.species, envir.prey.all, envir.prey.filtered)


## Selectivity Calculations =====================================

## Combine diet and environment proportions, 
## filter out only data that has a zero in both diet and environment,
## and calculate diet:envir ratio

larval.diet.env.prop <- left_join(diet.cont.prop, envir.prey.prop) %>%
  mutate(diet.envir = diet.prop / envir.prop) %>% 
  ## remove any no diet, no environment taxa
  filter(!is.na(diet.envir))

## Calculate alpha ((r/p)/max(r/p))
larval.diet.env.prop.nInf <- larval.diet.env.prop %>% filter(!is.infinite(diet.envir)) %>%
  group_by(trawl) %>% 
  mutate(alpha = diet.envir / max(diet.envir),
         alpha = ifelse(is.na(alpha) == TRUE, 0, alpha))

## manually assign alpha as 1 if prey is found in diet but not environment (assumes they consumed all available prey)
larval.diet.env.prop.Inf <- larval.diet.env.prop %>% filter(is.infinite(diet.envir)) %>% 
  mutate(alpha = 1)

## Combine non-Inf and Inf dataframes
larval.alpha <- bind_rows(larval.diet.env.prop.nInf, larval.diet.env.prop.Inf) %>% 
  arrange(trawl, species)

## Calculate electivity
larval.selectivity <- left_join(effort, larval.alpha) %>% 
  filter(!is.na(species)) %>% 
  group_by(week) %>% 
  mutate(n.species = n_distinct(species)) %>% ungroup() %>% 
  mutate(E = (alpha - (1 / n.species)) / (alpha + (1 / n.species)))


## Average the Selectivity Values ===============================
larval.selectivity.week <- larval.selectivity %>%
  group_by(week, species) %>% 
  summarize(n.species = mean(n.species),
            mean.alpha = mean(alpha), 
            mean.E = mean(E),
            sd.alpha = sd(alpha),
            sd.E = sd(E)) %>% ungroup() %>% 
  filter(!is.na(species)) %>% 
  complete(week, species = species.list, fill = list(mean.alpha = 0, mean.E = 0, sd.alpha = 0, sd.E = 0)) %>% 
  left_join(diet.sample.size) %>% 
  mutate(se.alpha = sd.alpha / sqrt(n.trawl),
         se.E = sd.E / sqrt(n.trawl))

## Abbreviate taxa names
larval.selectivity.week$species <- gsub('Cyclopidae', 'CY', larval.selectivity.week$species)
larval.selectivity.week$species <- gsub('Cyclopoid Copepodid', 'CY*', larval.selectivity.week$species)
larval.selectivity.week$species <- gsub('Daphnia', 'DA', larval.selectivity.week$species)
larval.selectivity.week$species <- gsub('Calanoidae', 'CA', larval.selectivity.week$species)
larval.selectivity.week$species <- gsub('Calanoid Copepodid', 'CA*', larval.selectivity.week$species)
larval.selectivity.week$species <- gsub('Holopedium', 'HO', larval.selectivity.week$species)
larval.selectivity.week$species <- gsub('Nauplii', 'NA', larval.selectivity.week$species)


## Expand week numbers to date ranges
larval.selectivity.week$week <- gsub('20', 'May 14', larval.selectivity.week$week)
larval.selectivity.week$week <- gsub('25', 'June 18', larval.selectivity.week$week)
larval.selectivity.week$week <- gsub('23', 'June 4', larval.selectivity.week$week)
larval.selectivity.week$week <- gsub('30', 'July 23', larval.selectivity.week$week)
larval.selectivity.week$week <- gsub('21', 'May 21', larval.selectivity.week$week)
larval.selectivity.week$week <- gsub('29', 'July 16', larval.selectivity.week$week)
larval.selectivity.week$week <- gsub('28', 'July 9', larval.selectivity.week$week)
larval.selectivity.week$week <- gsub('22', 'May 28', larval.selectivity.week$week)
larval.selectivity.week$week <- gsub('24', 'June 11', larval.selectivity.week$week)
larval.selectivity.week$week <- gsub('27', 'July 2', larval.selectivity.week$week)
larval.selectivity.week$week <- gsub('26', 'June 25', larval.selectivity.week$week)

larval.selectivity.week %<>% mutate(week = factor(week, levels = c('May 14', 'May 21', 'May 28','June 4', 
                                                     'June 11', 'June 18','June 25', 'July 2',
                                                     'July 9','July 16', 'July 23'),
                                    ordered = TRUE))


## Calculate alpha (preference/avoidance) and E (maximum selection) thresholds
larval.selectivity.threshold <- larval.selectivity.week %>% 
  filter(mean.E != 0) %>% 
  group_by(week) %>% 
  mutate(n.species = n_distinct(species),
         alpha.threshold = 1/n.species,
         E.threshold = (1 - (1 / n.species)) / (1 + (1 / n.species)))


## Find all weeks and prey that are NA - creates the dataframe for plotting "nf"
larval.selectivity.week.zero <- larval.selectivity.week %>% 
  filter(mean.E == 0)


## Visualization ================================================

## Plot Selectivity  
ggplot(larval.selectivity.week, aes(x = species, y = mean.alpha, group = species)) +
  geom_bar(stat = "identity", color = "black", fill = "gray80", width = 1) +
  geom_errorbar(aes(ymin = mean.alpha + se.alpha, ymax = mean.alpha - se.alpha), width = 0.3) +
  geom_hline(data = larval.selectivity.threshold, aes(yintercept = alpha.threshold), linetype = 'dashed') +
  geom_text(data = larval.selectivity.week.zero, aes(x = species, y = 0.06), label = "nf", size = 3) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0))+
  labs(x = "Prey Taxa", y = "Selectivity Index (W')") +
  theme(panel.grid = element_blank(), panel.background = element_blank(),
        panel.spacing = unit(1, "lines"), axis.line = element_line(),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 25, margin = margin(0, 18, 0, 0)),
        axis.title.x = element_text(size = 25, margin = margin(15, 0, 0, 0)),
        axis.ticks.length = unit(1.25, 'mm'),
        strip.text = element_text(size = 13),
        strip.background = element_blank()) +
  facet_rep_wrap(~week, dir = "v", ncol = 2)

ggsave("figures/apis_larval_selectivity_alpha_weekly.png", dpi = 300, width = 10, height = 10)

## Plot Electivity  
ggplot(larval.selectivity.week, aes(x = species, y = mean.E, group = species)) +
  geom_bar(stat = "identity", color = "black", fill = "gray80", width = 1) +
  geom_errorbar(data = filter(larval.selectivity.week, se.E != 0), 
                aes(ymin = mean.E + se.E, ymax = mean.E - se.E), width = 0.3) +
  geom_hline(yintercept = 0) +
  geom_hline(data = larval.selectivity.threshold, aes(yintercept = E.threshold), linetype = 'dashed') +
  geom_text(data = larval.selectivity.week.zero, aes(x = species, y = 0.115), label = "nf", size = 3) +
  scale_y_continuous(limits = c(-1, 1), expand = c(0, 0))+
  labs(x = "Prey Taxa", y = expression(paste("Electivity Index (", E["i"]^"*", ")", sep = ""))) +
  theme(panel.grid = element_blank(), panel.background = element_blank(),
        panel.spacing = unit(1, "lines"), axis.line = element_line(),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 25, margin = margin(0, 15, 0, 0)),
        axis.title.x = element_text(size = 25, margin = margin(15, 0, 0, 0)),
        axis.ticks.length = unit(1.25, 'mm'),
        strip.text = element_text(size = 13),
        strip.background = element_blank()) +
  facet_rep_wrap(~week, dir = "v", ncol = 2)

ggsave("figures/apis_larval_selectivity_E_weekly.png", dpi = 300, width = 10, height = 10)

