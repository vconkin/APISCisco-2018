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
library(cowplot)       # for facet_rep_wrap()


## LOAD DATA ====================================================

diet.cont <- read_excel("data/APIS_Coregonus_2018.xlsx", sheet = "Larval_Diet") %>%
  mutate(trawl = as.numeric(ifelse(trawl == "37.1", "37", trawl)))
effort <- read_excel("data/APIS_Coregonus_2018.xlsx", sheet = "Neuston Effort") %>% 
  select(trawl, week) %>%
  mutate(trawl = as.numeric(ifelse(trawl == "37.1", "37", trawl)))


## DIET DATA PREP ===============================================

## Calculate sample sizes (no. of larvae with food) by week - save for plotting
diet.sample.size <- left_join(diet.cont, effort) %>% 
  group_by(week) %>% 
  filter(diet.count > 0) %>%
  summarize(n.fish = sum(n.diet))

## Restrict DF to the selected variables and rename variables
diet.cont.species <- diet.cont %>% select(trawl, Nauplii:Chironomid_pupae) %>% 
  rename("Acanthocyclops" = "Acanthocyclops_sp.", "Eucyclops" = "Eucyclops_sp.",
         "Holopedium" = "Holopedium_gibberum", "Daphnia" = "Daphnia_sp.", 
         "Bosmina" = "Bosmina_sp.")

## Make all NAs (blanks) zero
diet.cont.species[is.na(diet.cont.species)] <- 0

## Condense all food items down into types
diet.comp <- diet.cont.species %>% mutate("Calanoid copepodid" = Cal_Copepodite,
                                          "Cyclopoid copepodid" = Cyc_Copepodite,
                                          "Calanoidae" = L.minutus + L.sicilis + Limnocalanus_macrurus + 
                                            E.lacustrus + Senecella_calanoides + Unknown_Fragment_Cyclopoid,
                                          "Cyclopoidae" = Acanthocyclops + Diacyclops_thomasi + 
                                            Eucyclops + Unknown_Fragment_Cyclopoid,
                                          "Other" = Daphnia + Bosmina + Invertebrate_eggs + Chironomid_pupae + 
                                            Rotifera + Bythotrephes + Diaphanosoma + Leptodora_kindi) %>%
  select(-Acanthocyclops, -Diacyclops_thomasi, -Eucyclops, -Cyc_Copepodite,
         -L.minutus, -L.sicilis, -Limnocalanus_macrurus, -E.lacustrus, 
         -Senecella_calanoides, -Cal_Copepodite, -Unknown_Fragment_Calanoid, 
         -Unknown_Fragment_Cyclopoid, -Invertebrate_eggs, -Chironomid_pupae, 
         -Rotifera, -Daphnia, -Bosmina, -Bythotrephes, -Diaphanosoma, -Leptodora_kindi) %>%
  gather(species, diet.count, Nauplii:Other) %>% droplevels() %>% 
  filter(diet.count != 0)


## DIET COMPOSITION =============================================

## Group by weeks and summarize
diet.comp.week <- left_join(diet.comp, effort, by = "trawl") %>%
  left_join(diet.sample.size) %>% 
  filter(!is.na(n.fish)) %>% 
  group_by(week, species) %>%
  summarize(n.fish = unique(n.fish),
            diet.count = sum(diet.count)) %>% 
  mutate(diet.count.indiv = diet.count/n.fish) %>% 
  group_by(week) %>%
  mutate(diet.total = sum(diet.count.indiv)) %>% 
  mutate(diet.perc = round((diet.count.indiv/diet.total)*100, 2)) %>% ungroup() %>% 
  mutate(label = paste0(week,'\n(', n.fish, ")"),
         label = factor(label, ordered = TRUE))


## ABBREVIATE TAXA NAMES ========================================

diet.comp.week$species <- gsub('Cyclopoidae', "CY", diet.comp.week$species)
diet.comp.week$species <- gsub('Cyclopoid copepodid', "CY*", diet.comp.week$species)
diet.comp.week$species <- gsub('Calanoidae', "CA", diet.comp.week$species)
diet.comp.week$species <- gsub('Calanoid copepodid', "CA*", diet.comp.week$species)
diet.comp.week$species <- gsub('Holopedium', "HO", diet.comp.week$species)
diet.comp.week$species <- gsub('Nauplii', "NA", diet.comp.week$species)
diet.comp.week$species <- gsub('Other', "OT", diet.comp.week$species)


## VISUALIZATION ================================================

## Define a colorblind safe(ish) palette for 7-classes
color <- c("gray30", "#e69f00", "#56b4e9", "#009e73", "#f0e442", "#0072b2", "#d55e00", "#cc79a7")

## Plot Number per Fish  
diet.count.plot <- ggplot(diet.comp.week, aes(x = label, y = diet.count.indiv, fill = species )) +
  geom_bar(stat = "identity", color = "black", width = 1) +
  scale_x_discrete(expand = c(0, 0))+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0,0)) +
  labs(y = "Avg. # of Diet Items per Individual") +
  scale_fill_manual(values = color) +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        strip.text = element_blank(), 
        axis.ticks.length = unit(2, 'mm'),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20, colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 25, margin = margin(0, 20, 0, 0)),
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.key.size = unit(0.75, 'cm'),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        plot.margin = unit(c(8, 5, 15, 5), "mm"))

## Plot Percent per Fish  
diet.comp.plot <- ggplot(diet.comp.week, aes(x = label, y = diet.perc, fill = species ))+
  geom_bar(stat = "identity", color = "black", width = 1) +
  scale_x_discrete(expand = c(0, 0))+
  scale_y_continuous(limits = c(0, 100.1), breaks = seq(0, 100, 20), expand = c(0,0)) +
  labs(y = "Avg. Diet Composition (%)") +
  scale_fill_manual(values = color) +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        strip.text = element_blank(),
        legend.title =  element_blank(),
        legend.text = element_text(size = 15),
        legend.key.size = unit(0.75, 'cm'),
        legend.position = "none",
        axis.text.y = element_text(size = 20, colour = "black"),
        axis.text.x = element_text(size = 20, colour = "black"),
        axis.title.y = element_text(size = 25, margin = margin(0, 20, 0, 0)),
        axis.title.x = element_blank(),
        panel.spacing = unit(2, "lines"),
        plot.margin = unit(c(0, 5, 10, 5), "mm"))


## PANELED VISUALIZATION ========================================

## Create common legend
legend <- get_legend(diet.comp.plot + theme(legend.position = "right"))

# arrange the three plots in a single row
diet.grid <- plot_grid(diet.count.plot,
                       diet.comp.plot,
                       nrow = 2)

## add legend to grid
diet.grid.legend <- plot_grid(diet.grid, legend, ncol = 2, rel_widths = c(2, 0.2))

## add common x-axis label
ggdraw(add_sub(diet.grid.legend, "Week", vpadding = grid::unit(0,"lines"), y = 0.75, x = 0.5, size = 25))

ggsave("figures/apis_diet_weekly_gridded.png", width = 12, height = 12, dpi = 300)
