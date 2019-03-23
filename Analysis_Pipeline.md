### Apostle Islands Cisco workflow

The overall goal of the workflow is to synthesize the data from the field (effort information including trawl, date/time, distances, volume filtered, etc.), lab processing (yolk condition, lengths, stomach contents), and zems analysis (zooplankton counts and lengths). These form two branches, fish and zooplankton, that ultimately are combined to calculate selectivity values.  Each piece is laid out here as I worked through them:

1. Fish Data Flow:  [Field Data](#Field Data) —**> **[Stomach Data](#Stomach Data)

2. Zems Data Flow:  [Bytho_Upload](#Bythotrephes Data)—**>** [Zems_1_Upload](#Zems Data) —**>** [Zems_2_Wrangle](#Format Zems Data) —**>** [Zems_3_Visualize](#Visualize)

3. Diet Selection Analysis:  [Selection](#Selection)

All scripts were run in R using R version 3.5.2 (2018-12-20). Files in the path are included in each file description. 

------

###### <u>Field Data</u>

File name: AI_Coregonus_Copy 

Files in path: APIS_Coregonus_2018.xlsx

Description: This file takes compiles and visualizes all the data from the field sampling. Original was written by Mark Vinson, I expanded and tweaked the plots. The script primarily explores length trends using Ashland lengths, as well as abundance and distribution across weeks, islands, and island groups.

 

###### <u>Stomach Data</u>

File name: Larval_Coregonus_processing

Files in path: Larval_Coregonus_processing.xlsx, NC.csv, Selgeby_FullStomachs.csv,

Description: This script takes the stomach data to make preliminary diet figures, and combines some of the data across sheets in the excel file (yolk condition and number of prey items consumed).  It’s useful in visualizing the length and diet data together, but still needs some tweaks in creating n labels for length data to add to plots. The comparison to Selgeby is currently under construction to make a comparable density plot using geometric mean. The script includes extracted data from the Selgeby plots for the percent of stomachs with food, and presence of nauplii and copepodites. The Selgeby data plots are found in each of these sections immediately after the same plot made with the 2018 data.

 

###### <u>Bythotrephes Data</u> 

File name: Bytho_Upload 

Files in path: All files in Bytho folder: serials 950, 980, 982, 990, 1002, 1004, 1006, 1008, 1010

Outputs: Bytho.Samples.csv: compiled *Bythotrephes* measurements and counts

Description: Use the Bytho folder in the Zems directory to upload all the *Bythotrephes* data and calculate expansion coefficients (they should all be 1). Since they were counted and measured across the whole sample, the calculations are different and they need to be wrangled separately and then combined. We do that in Zems_2_Wrangle after we build our two data frames.

 

###### <u>Zems Data</u>

File name: Zems_1_Upload 

Files in path: All Superior*.csv files in main Zems folder

Description: Read in all the Zems files and calculate expansion coefficients. The outputs from each of the upload steps will be combined in Zems_2_Wrangle.

Outputs: All.Samples.csv: measurements and counts for all zooplankton except *Bythotrephes*



###### <u>Format Zems Data</u>

File name: Zems_2_Wrangle

Files in path: All.Samples.csv, Bytho.Samples.csv, APIS_Cut.csv, Zoop_Cut.csv, LWtable_forHannah.csv

Description: Formats the data into a range of more useable outputs. We first combine the *Bythotrephes* with the rest of the zooplankton data and output all the combined lengths (ZLength). Next, we expand the zooplankton data to add in zeros for species that were not observed in a trawl. Those are collapsed down and the *Bythotrephes* and zooplankton outputs are combined (ZTrawl). Density for each genus is then calculated, including zero counts. The density data is then exported (ZDensity). Biomass is calculated from length/weight regressions

Outputs: ZLength.csv: Basic transformation to add in volume filtered and effort information (Site, date, trawl number, etc.)

 ZTrawl.csv: Counts for all species with added zeros for trawls where individual species were not found. Includes volume filtered and effort information.

ZDensity.csv: Calculated number per sample and density (number/L) with Date, week, and station. Includes zeros for trawls where individual species were not found.

SumBySampleSpecies.csv: Biomass by genera observed, including mean individual dry weight and length, and sd for both parameters. Only effort information included is trawl. 

TotalZoopTable18.csv: Calculated total biomass and density (number/L) across taxa for each trawl.



###### <u>Visualize</u>

File name: Zems_3_Visualize

Files in path: ZLength.csv, ZTrawl.csv,  ZDensity.csv, SumBySampleSpecies.csv, APIS_Cut.csv

Description: Creates visuals for the Zems data we've compiled in the last 2 steps. We start out with density data across weeks and sites, then look at length distributions. We bin the zooplankton lengths and look at distributions in length and across space/time. Lastly, we take a look at biomass over time collectively and by individual taxa.



###### <u>Selection</u>

File name: Selection_Combine
Files in path: Larval_Coregonus_processing.xlsx, ZDensity.csv, APIS_Cut.csv
Description: combines the Zems data with length and diet data making it the last step to join the two branches of analysis. We start by calculating the indices we're interested in, and then we make a few additional plots that combine the diet and Zems data.
Outputs: Selection_Indices.csv: summarizes selection for each prey type occurring in the zooplankton tow and the fish stomachs from that trawl. 

