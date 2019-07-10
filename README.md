APIS Coregonine
==========

This repository contains data files, analysis scripts, and manuscript figures for *manuscript title* by ...authors... that has been accepted for publication in ...journal name...


# Data files (not in the repo)
* `data/APIS_Coregonus_2018.xlsx` -- Excel file containing the coregonine biological and effort data specific to this project.
* `data/APIS_Zoolankton_2018.xlsx` -- Excel file containing the zooplankton biological and effort data specific to this project.
* `data/Superior_Files` -- Folder containing the raw zooplankton files created from miscroscope software, length-weight regression coefficients, and `All_Samples.csv`, `Bytho_Samples.csv`, and `APIS_Zoop_Summary.csv` created in `data_prep/Zems_1_Upload.R`, `data_prep/Zems_2_Upload_Bytho.R`, `data_prep/Zems_3_Wrangle.R`.


# Scripts
## Length Frequency Analysis
* `Length_Freq.R` -- Processes the larval length/yolk data to produce **Figure XX** in the manuscript.

## Yolk-sac Condition - Diet Analysis
* `YolkSac_Diet.R` -- Processes the larval length/yolk and diet data to produce **Figure XX** in the manuscript.

## Zooplankton Density and Biomass Visualization
* `Zoop_DensityBiomass_Plots.R` -- Produces the zooplankton density, biomass, relative density, and relative biomass figures compiled as **Figure XX** in the manuscript.

## Prey Selectivity Analysis
* `Prey_Selectivity.R` -- Produces the larval diet and zooplankton data to calculate Chesson's alpha index and produce **Figure XX** in the manuscript.


# Other Files
* `APISCisco-2018.Rproj` -- RStudio project file.
* `.gitignore` -- things to ignore when committing or pushing to GitHub.
* `README.md` -- This readme file.
