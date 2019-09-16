library(readxl)
library(dbplyr)
library(tidyverse)
library(purrr)
library(forcats)


### INITIALIZE VARIABLES ----

# Riparian table lookup
riparian.lookup <- tribble(
  ~variable, ~name,
  #--------/------
  "disturbance", "Disturbance index",
  "stability", "Stability index",
  "densio1L", "Shading over land: 1.0m from shore\n(%)",
  "densio05", "Shading over water: 0.5m depth\n(%)",
  "densio1W", "Shading over water: 1.0m depth\n(%)",
  "slope", "Slope\n(degrees)",
  "buffermean", "Mean buffer width\n(m)",
  "buffermax", "Maximum buffer width\n(m)",
  "buffermin", "Minimum buffer width\n(m)",
  "treecover", "Tree cover\n(%)",
  "hishrub", "Tall shrub cover\n(%)",
  "lowshrub", "Low shrub cover\n(%)",
  "groundcover", "Ground cover\n(%)",
  "composite", "Composite cover\n(0 to 12)",
  "regenerating", "Regenerating trees (DBH <10cm)\n(count)",
  "sapling", "Young trees (DBH 11-28cm)\n(count)",
  "bigtrees", "Mature trees (DBH >28cm)\n(count)",
  "FWD1","Fine woody structure: 1.0m\n(mean #/quadrat)",
  "MWD05","Medium woody structure: 0.5m\n(mean #/quadrat)",
  "MWD1","Medium woody structure: 1.0m\n(mean #/quadrat)",
  "CWD05","Coarse woody structure: 0.5m\n(mean #/quadrat)",
  "CWD1","Coarse woody structure: 1.0m\n(mean #/quadrat)",
  "leaflit1","Leaf litter: 1.0m\n(% transect)"
)

# Littoral table lookup
littoral.lookup <- tribble(
  ~variable, ~fullname, ~name, ~depth,
  #--------/------
  "embed05","Embeddedness: 0.5m\n(%)", "Embeddedness\n(%)", 0.5,
  "embed1","Embeddedness: 1.0m\n(%)", "Embeddedness\n(%)", 1.0,
  "FWD05","Fine woody structure: 0.5m\n(mean #/quadrat)", "Fine woody structure\n(mean #/quadrat)", 0.5,
  "FWD1","Fine woody structure: 1.0m\n(mean #/quadrat)", "Fine woody structure\n(mean #/quadrat)",  1.0,
  "MWD05","Medium woody structure: 0.5m\n(mean #/quadrat)", "Medium woody structure\n(mean #/quadrat)", 0.5,
  "MWD1","Medium woody structure: 1.0m\n(mean #/quadrat)", "Medium woody structure\n(mean #/quadrat)",  1.0,
  "CWD05","Coarse woody structure: 0.5m\n(mean #/quadrat)", "Coarse woody structure\n(mean #/quadrat)", 0.5,
  "CWD1","Coarse woody structure: 1.0m\n(mean #/quadrat)",  "Coarse woody structure\n(mean #/quadrat)", 1.0,
  "leaflit05","Leaf litter: 0.5m\n(% transect)", "Leaf litter\n(% transect)", 0.5,
  "leaflit1","Leaf litter: 1.0m\n(% transect)", "Leaf litter\n(% transect)", 1.0,
  "aufcov05", "Aufwuchs cover: 0.5m\n(rating 1-10)", "Aufwuchs cover\n(rating 1-10)", 0.5,
  "aufcov1", "Aufwuchs cover: 1.0m\n(rating 1-10)", "Aufwuchs cover\n(rating 1-10)", 1.0,
  "aufden05","Aufwuchs density: 0.5m\n(rating 1-3)", "Aufwuchs density\n(rating 1-3)", 0.5,
  "aufden1","Aufwuchs density: 1.0m\n(rating 1-3)", "Aufwuchs density\n(rating 1-3)", 1.0,
  "boulders05","Boulder cover: 0.5m\n(%)", "Boulder cover\n(%)", 0.5,
  "boulder1","Boulder cover: 1.0m\n(%)", "Boulder cover\n(%)", 1.0,
  "cobble05","Cobble cover: 0.5m\n(%)", "Cobble cover\n(%)", 0.5,
  "cobble1","Cobble cover: 1.0m\n(%)", "Cobble cover\n(%)", 1.0,
  "grain05","Grain cover: 0.5m\n(%)", "Grain cover\n(%)", 0.5,
  "grain1","Grain cover: 1.0m\n(%)", "Grain cover\n(%)", 1.0,
  "gravel05","Gravel cover: 0.5m\n(%)", "Gravel cover\n(%)", 0.5,
  "gravel1","Gravel cover: 1.0m\n(%)", "Gravel cover\n(%)", 1.0,
  "sand05","Sand cover: 0.5m\n(%)", "Sand cover\n(%)", 0.5,
  "sand1","Sand cover: 1.0m\n(%)", "Sand cover\n(%)", 1.0,
  "silt05","Silt cover: 0.5m\n(%)", "Silt cover\n(%)", 0.5,
  "silt1","Silt cover: 1.0m\n(%)", "Silt cover\n(%)", 1.0,
)

# Macrorock table lookup
macrorock.lookup <- tribble(
  ~variable, ~name,
  #--------/------
  "oforganisms", "Total no. of organisms",
  "shannon", "Shannon Index",
  "meanrichness", "Mean richness",
  "eptrichness", "EPT Richness",
  "fbi", "FBI",
  "percentcote", "Proportion COTE",
  "percentcrustmol", "Proportion Crustacea and Mollusca",
  "percentchironomidae", "Proportion Chironomidae",
  "percentoligochaeta", "Proportion Oligochaeta"
)

# Macrotran table lookup
macrotran.lookup <- tribble(
  ~variable, ~name,
  #--------/------
  "shannon","Shannon Index",
  "oforganisms","Total no. of organisms",
  "cotesum","cotesum",
  "sumoligochaeta","sumoligochaeta",
  "sumcrustmol","sumcrustmol",
  "sumchironomidae","sumchironomidae",
  "percentcote","Proportion COTE",
  "percentoligochaeta","Proportion Oligochaeta",
  "percentcrustmol","Proportion Crustacea and Mollusca",
  "percentchironomidae","Proportion Chironomidae",
  "fbi","FBI",
  "eptrichness","EPT Richness",
  "countcote","countcote",
  "meanrichness","Mean richness",
  "dominantorder","dominantorder",
  "dominant3order","dominant3order",
  "sumeptsumeptsumchiro","sumeptsumeptsumchiro",
  "sumcotesumcotesumchirosumoli","sumcotesumcotesumchirosumoli",
  "coleoptera","coleoptera",
  "diptera","diptera",
  "ephemeroptera","ephemeroptera",
  "trichoptera","trichoptera"
)

### READ DATA FILES ----
riparian <- read_excel("../data_raw/Riparian Data.xls")
littoral <- read_xls("../data_raw/Littoral Data.xls")
macrorock <- read_xls("../data_raw/Macroinvert Rock trap data.xls")
macrotran <- read_xls("../data_raw/Macroinvert Transect data.xls")

### PREP DATA TABLES ----

# Create long form riparian dataset (Slope column has dots that needed to be replaced with NA) ----
riparian_lg <- gather(riparian, key="variable", value="value", -Lake, -treatment) %>% 
  left_join(riparian.lookup, "variable") %>% 
  mutate(value = as.numeric(na_if(value,".")),
         Lake = fct_relevel(Lake, "EP", "NP", "GP"),
         name = fct_relevel(name, riparian.lookup$name),
         treatment = factor(treatment, levels=c("REF", "BD", "UD"), ordered = TRUE))

# Create long form littoral dataset  ----
littoral_lg <- gather(littoral, key="variable", value="value", -Lake, -treatment) %>% 
  left_join(littoral.lookup, "variable") %>% 
  mutate(Lake = fct_relevel(Lake, "EP", "NP", "GP"),
         name = fct_relevel(name, unique(littoral.lookup$name)),
         treatment = factor(treatment, levels=c("REF", "BD", "UD"), ordered = TRUE))

# Create long form macroinvertebrate rock trap table ----
macrorock_lg <- macrorock %>%  select(-name, -depth) %>% 
  gather(key="variable", value="value", -pond, -treatment) %>% 
  left_join(macrorock.lookup, "variable") %>% 
  mutate(treatment = fct_recode(treatment, UD = "U", BD = "B", REF = "R"),
         treatment = factor(treatment, levels=c("REF", "BD", "UD"), ordered = TRUE),
         Lake = fct_recode(pond, GP = "Great", NP = "North", EP = "East"),
         Lake = fct_relevel(Lake, "EP", "NP", "GP")) %>% 
  select(-pond)

# Create long form macroinvertebrate transect table ----
macrotran_lg <- macrotran %>%  select(-name) %>% 
  gather(key="variable", value="value", -pond, -treatment, -depth) %>% 
  left_join(macrotran.lookup, "variable") %>% 
  mutate(treatment = fct_recode(treatment, UD = "U", BD = "B", REF = "R"),
         treatment = factor(treatment, levels=c("REF", "BD", "UD"), ordered = TRUE),
         Lake = fct_recode(pond, GP = "Great", NP = "North", EP = "East"),
         Lake = fct_relevel(Lake, "EP", "NP", "GP"),
         depth = fct_relevel(depth, "T0.5", "T1.0", "RT"),
        # variable = paste0(variable,": ",depth),
         name = paste0(name,": ",depth)) %>% 
  select(-pond)

# Save data tables to file
save(file="../data_derived/data_tables.rdata", macrotran_lg, macrorock_lg, littoral_lg, riparian_lg, 
     macrotran.lookup, macrorock.lookup,riparian.lookup, littoral.lookup)
