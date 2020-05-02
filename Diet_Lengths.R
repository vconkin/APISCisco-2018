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
effort <- read_excel("data/APIS_Coregonus_2018.xlsx", sheet = "Neuston_Effort") %>% 
  select(trawl, week) %>%
  mutate(trawl = as.numeric(ifelse(trawl == "37.1", "37", trawl)))


## DIET DATA PREP ===============================================

## Calculate sample sizes (no. of larvae with food) by week - save for plotting
diet.sample.size <- left_join(diet.cont, effort) %>% 
  mutate(week.bin = factor(ifelse(week < 25, "first", "last"))) %>% 
  group_by(week.bin, tl.bin) %>% 
  summarize(n.fish = sum(n.diet))

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
                                          "Calanoidae" = L.minutus + L.sicilis + Limnocalanus_macrurus + 
                                            E.lacustrus + Senecella_calanoides + Unknown_Fragment_Cyclopoid,
                                          "Cyclopoidae" = Acanthocyclops + Diacyclops_thomasi + 
                                            Eucyclops + Unknown_Fragment_Cyclopoid,
                                          "Other" = Bosmina + Invertebrate_eggs + Chironomid_pupae + 
                                            Rotifera + Diaphanosoma + Leptodora_kindi) %>%
  select(-Acanthocyclops, -Diacyclops_thomasi, -Eucyclops, -Cyc_Copepodite,
         -L.minutus, -L.sicilis, -Limnocalanus_macrurus, -E.lacustrus, 
         -Senecella_calanoides, -Cal_Copepodite, -Unknown_Fragment_Calanoid, 
         -Unknown_Fragment_Cyclopoid, -Invertebrate_eggs, -Chironomid_pupae, 
         -Rotifera, -Bosmina, -Diaphanosoma, -Leptodora_kindi) %>%
  gather(species, diet.count, Nauplii:Other) %>% droplevels() %>% 
  filter(diet.count != 0)


## DIET COMPOSITION =============================================

## Summarize by trawl, week, length bin, and species
diet.comp.length <- left_join(diet.comp, effort, by = "trawl") %>%
  mutate(week.bin = factor(ifelse(week < 25, "first", "last"))) %>% 
  group_by(week.bin, tl.bin, species) %>%
  summarize(diet.count = sum(diet.count)) %>% ungroup() %>% 
  complete(species = unique(.$species), tl.bin = 6:26, week.bin = unique(.$week.bin), fill = list(diet.count = 0)) %>% 
  group_by(week.bin, tl.bin) %>% 
  mutate(diet.total = sum(diet.count),
         diet.perc = round((diet.count/diet.total)*100, 2)) %>% ungroup() %>% 
  left_join(diet.sample.size) 


## ABBREVIATE TAXA NAMES ========================================

diet.comp.length$species <- gsub('Cyclopoidae', "CY", diet.comp.length$species)
diet.comp.length$species <- gsub('Cyclopoid copepodid', "CY*", diet.comp.length$species)
diet.comp.length$species <- gsub('Bythotrephes', "BY", diet.comp.length$species)
diet.comp.length$species <- gsub('Daphnia', "DA", diet.comp.length$species)
diet.comp.length$species <- gsub('Holopedium', "HO", diet.comp.length$species)
diet.comp.length$species <- gsub('Calanoidae', "CA", diet.comp.length$species)
diet.comp.length$species <- gsub('Calanoid copepodid', "CA*", diet.comp.length$species)
diet.comp.length$species <- gsub('Nauplii', "NA", diet.comp.length$species)
diet.comp.length$species <- gsub('Other', "OT", diet.comp.length$species)


## CREATE SAMPLE SIZE LABELS ====================================

diet.comp.length.first <- diet.comp.length %>% filter(week.bin == "first") %>% 
  mutate(diet.perc = replace_na(diet.perc, 0),
         n.fish = replace_na(n.fish, 0),
         label = paste0(tl.bin,'\n(', n.fish, ")"),
         label = factor(label, ordered = TRUE, levels = c("6\n(0)","7\n(0)", "8\n(0)", "9\n(0)", "10\n(6)", "11\n(23)", 
                                                          "12\n(42)","13\n(73)", "14\n(50)", "15\n(33)", "16\n(15)", 
                                                          "17\n(6)", "18\n(3)", "19\n(0)","20\n(0)", "21\n(1)", 
                                                          "22\n(0)", "23\n(0)", "24\n(0)", "25\n(0)", "26\n(0)")))

diet.comp.length.last <- diet.comp.length %>% filter(week.bin == "last") %>% 
  mutate(diet.perc = replace_na(diet.perc, 0),
         n.fish = replace_na(n.fish, 0),
         label = paste0(tl.bin,'\n(', n.fish, ")"),
         label = factor(label, ordered = TRUE, levels = c("6\n(1)","7\n(0)", "8\n(3)", "9\n(4)", "10\n(10)", "11\n(12)", 
                                                          "12\n(14)","13\n(19)", "14\n(28)", "15\n(22)", "16\n(22)", 
                                                          "17\n(33)", "18\n(19)", "19\n(18)","20\n(4)", "21\n(11)", 
                                                          "22\n(2)", "23\n(2)", "24\n(0)", "25\n(1)", "26\n(1)")))

## VISUALIZATION ================================================
## Define a colorblind safe(ish) palette for 7-classes
#color <- c("gray30", "#e69f00", "#56b4e9", "#009e73", "#f0e442", "#0072b2", "#d55e00")
color <- c("#000000", "#F0E442", "#56B4E9", "#CC79A7", "#009E73", "#D55E00", "#E69F00", "#0072B2", "gray40")


diet.comp.pre <- ggplot(diet.comp.length.first, aes(x = label, y = diet.perc, fill = species))+
  geom_bar(stat = "identity", color = "black", width = 1) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100.1), breaks = seq(0, 100, 20), expand = c(0,0)) +
  labs(x = "Length Class (mm)", y = "Avg. Diet Composition (%)") +
  scale_fill_manual(values = color) +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        strip.text = element_blank(), 
        axis.ticks.length = unit(2, 'mm'),
        axis.text = element_text(size = 19, colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 25, margin = margin(0, 20, 0, 0)),
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.key.size = unit(0.75, 'cm'),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        plot.margin = unit(c(5, 5, 8, 5), "mm"))

diet.comp.post <- ggplot(diet.comp.length.last, aes(x = label, y = diet.perc, fill = species))+
  geom_bar(stat = "identity", color = "black", width = 1) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100.1), breaks = seq(0, 100, 20), expand = c(0,0)) +
  labs(x = "Length Class (mm)", y = "Avg. Diet Composition (%)") +
  scale_fill_manual(values = color) +
  theme_bw() +
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        strip.text = element_blank(), 
        axis.ticks.length = unit(2, 'mm'),
        axis.text = element_text(size = 19, colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 25, margin = margin(0, 20, 0, 0)),
        legend.text = element_text(size = 15),
        legend.title = element_blank(),
        legend.key.size = unit(0.75, 'cm'),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        plot.margin = unit(c(5, 5, 8, 5), "mm"))


## PANELED VISUALIZATION ========================================

## Create common legend
legend <- get_legend(diet.comp.post + theme(legend.position = "right"))

# arrange the three plots in a single row
diet.grid <- plot_grid(diet.comp.pre,
                       diet.comp.post,
                       nrow = 2)

## add legend to grid
diet.grid.legend <- plot_grid(diet.grid, legend, ncol = 2, rel_widths = c(2, 0.2))

## add common x-axis label
ggdraw(add_sub(diet.grid.legend, "Length Classes (mm)", vpadding = grid::unit(0,"lines"), y = 0.85, x = 0.5, size = 25))

ggsave("figures/Fig_7_diet_length.tiff", width = 14, height = 10, dpi = 300)

