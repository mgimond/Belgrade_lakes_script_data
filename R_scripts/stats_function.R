library(dplyr)
library(tibble)
library(tidyr)
library(forcats)
library(purrr)


# Load data tables
load("../data_derived/data_tables.rdata")


#### CUSTOM FUNCTIONS -----

# Kruskal-Wallace  function
kw <- function(table, grp, val, cond, lookup) {
  grp <- substitute(grp)
  k.t <- substitute(kruskal.test(val ~ cond, data=.))
  table %>% na.omit() %>%
  split(.[[grp]])   %>%
  map( ~eval(k.t)[c("p.value", "statistic")]) %>% 
    map_df(unlist) %>% 
    t() %>% as.data.frame() %>% rownames_to_column() %>%
    magrittr::set_colnames(c("Variable", "P.orig", "H")) %>%
    mutate(P.value = p.adjust(P.orig, method="holm"),
           P.value = sprintf("%0.3f",P.value),
           H =  sprintf("%0.3f",H),
           H_p.adj = paste0(H, " (", if_else(P.value < 0.001, "<0.001", P.value ),")"),
           Variable = fct_relevel( Variable,lookup)) %>%
    select(-P.orig, -H, -P.value) %>%
    arrange(Variable)
} 

# Dunn function (requires FSA package)
# Note that this test will return a warning indicating that the categorical 
# variable is not a factor which is fine (the variable is in fact an "ordered 
# factor")
dunn <- function(table, grp, val, cond, lookup){
  grp <- substitute(grp)
  k.t <- substitute(FSA::dunnTest(val ~ cond, data=., method="holm"))
  table %>% na.omit() %>% 
  split(.[[grp]]) %>%  
  map( ~eval(k.t)$res) %>% 
  map(select, Comparison, P.adj) %>% 
  map(spread, Comparison, P.adj) %>%
  map_df(rbind, .id = "Variable") %>%
  mutate_if(is.numeric, funs(sprintf("%0.3f",.)) ) %>%
  mutate_all( ~ if_else(.x < 0.001, "<0.001", .x )) %>%
  mutate( Variable = factor( Variable,levels=lookup))
}

# Jonckheere-Terpstra function (requires the clinfun package)
jonck <- function(table, grp, val, cond, lookup){  
  val <- substitute(val)
  cond <- substitute(cond)
  grp <- substitute(grp)
  table %>% na.omit() %>%
  split(.[[grp]]) %>% 
  map( ~ with(., clinfun::jonckheere.test(eval(val), eval(cond), 
                         alternative = "decreasing",nperm=30000)[c("p.value", "statistic")]) ) %>%
  map_df(unlist) %>%
  t() %>% as.data.frame() %>%
  rownames_to_column() %>%
  magrittr::set_colnames(c("Variable", "P.orig", "JT")) %>%
  mutate(P.value = p.adjust(as.numeric(P.orig), method="holm"),
         P.value = sprintf("%0.3f",P.value),
         JT =  sprintf("%0.3f",JT),
         JT_p.adj = paste0(JT, " (", if_else(P.value <= 0.001, "<0.001", P.value ),")"),
         Variable = factor( Variable,levels=lookup)) %>% 
         select(-P.orig,-JT, -P.value) %>%
  arrange(Variable)
}
#### END of CUSTOM FUNCTIONS

# Riparian data analysis ----
# Across treatments
riparian.kw <- kw(riparian_lg, variable, val=value, cond=treatment,riparian.lookup$variable)
riparian.dunn <- dunn(riparian_lg, variable, val=value, cond=treatment,riparian.lookup$variable)
riparian.jonck <-  jonck(riparian_lg, variable, val=value, cond=treatment,riparian.lookup$variable)
riparian.stats <- full_join(riparian.kw, full_join(riparian.dunn,riparian.jonck, by='Variable' ), by='Variable')

write.csv(riparian.stats, file="../data_derived/riparian_treatment_stats.csv")

# Across lakes
riparian.lake.kw <- kw(riparian_lg, variable, val=value, cond=Lake, riparian.lookup$variable)
riparian.lake.dunn <- dunn(riparian_lg, variable, val=value, cond=Lake, riparian.lookup$variable)
riparian.lake.stats <- full_join(riparian.lake.kw, riparian.lake.dunn, by='Variable' )

write.csv(riparian.lake.stats, file="../data_derived/riparian_lake_stats.csv")

# littoral data analysis ----
# Across treatments
littoral.kw <- kw(littoral_lg, variable, val=value, cond=treatment,littoral.lookup$variable)
littoral.dunn <- dunn(littoral_lg, variable, val=value, cond=treatment,littoral.lookup$variable)
littoral.jonck <-  jonck(littoral_lg, variable, val=value, cond=treatment,littoral.lookup$variable)
littoral.stats <- full_join(littoral.kw, full_join(littoral.dunn,littoral.jonck, by='Variable' ), by='Variable')

write.csv(littoral.stats, file="../data_derived/littoral_treatment_stats.csv")

# Across lakes
littoral.lake.kw <- kw(littoral_lg, variable, val=value, cond=Lake,littoral.lookup$variable)
littoral.lake.dunn <- dunn(littoral_lg, variable, val=value, cond=Lake,littoral.lookup$variable)
littoral.lake.stats <- full_join(littoral.lake.kw, littoral.lake.dunn, by='Variable' )

write.csv(littoral.lake.stats, file="../data_derived/littoral_lake_stats.csv")

# Macrotran data analysis ----

# 0.5 depth
macrotran_lg_0.5 <- filter(macrotran_lg, depth == "T0.5")

# Across treatments
macrotran05.kw <- kw(macrotran_lg_0.5, variable, val=value, cond=treatment,macrotran.lookup$variable)
macrotran05.dunn <- dunn(macrotran_lg_0.5, variable, val=value, cond=treatment,macrotran.lookup$variable)
macrotran05.jonck <-  jonck(macrotran_lg_0.5, variable, val=value, cond=treatment, macrotran.lookup$variable)
macrotran05.stats <- full_join(macrotran05.kw, full_join(macrotran05.dunn,macrotran05.jonck, by='Variable' ), by='Variable')

write.csv(macrotran05.stats, file="../data_derived/macrotran_0.5_treatment_stats.csv")

# Across lakes
macrotran05.lake.kw <- kw(macrotran_lg_0.5, variable, val=value, cond=Lake, macrotran.lookup$variable)
macrotran05.lake.dunn <- dunn(macrotran_lg_0.5, variable, val=value, cond=Lake,macrotran.lookup$variable)
macrotran05.lake.stats <- full_join(macrotran05.lake.kw, macrotran05.lake.dunn, by='Variable' )

write.csv(macrotran05.lake.stats, file="../data_derived/macrotran_0.5_lake_stats.csv")

# 1.0 depth
macrotran_lg_1.0 <- filter(macrotran_lg, depth == "T1.0")

# Across treatments
macrotran1.kw <- kw(macrotran_lg_1.0, variable, val=value, cond=treatment,macrotran.lookup$variable)
macrotran1.dunn <- dunn(macrotran_lg_1.0, variable, val=value, cond=treatment,macrotran.lookup$variable)
macrotran1.jonck <-  jonck(macrotran_lg_1.0, variable, val=value, cond=treatment, macrotran.lookup$variable)
macrotran1.stats <- full_join(macrotran1.kw, full_join(macrotran1.dunn,macrotran1.jonck, by='Variable' ), by='Variable')

write.csv(macrotran1.stats, file="../data_derived/macrotran_1.0_treatment_stats.csv")

# Across lakes
macrotran1.lake.kw <- kw(macrotran_lg_1.0, variable, val=value, cond=Lake, macrotran.lookup$variable)
macrotran1.lake.dunn <- dunn(macrotran_lg_1.0, variable, val=value, cond=Lake,macrotran.lookup$variable)
macrotran1.lake.stats <- full_join(macrotran1.lake.kw, macrotran1.lake.dunn, by='Variable' )

write.csv(macrotran1.lake.stats, file="../data_derived/macrotran_1.0_lake_stats.csv")

# RT

macrotran_lg_RT <- filter(macrotran_lg, depth == "RT")

# Across treatments
macrotranRT.kw <- kw(macrotran_lg_RT, variable, val=value, cond=treatment,macrotran.lookup$variable)
macrotranRT.dunn <- dunn(macrotran_lg_RT, variable, val=value, cond=treatment,macrotran.lookup$variable)
macrotranRT.jonck <-  jonck(macrotran_lg_RT, variable, val=value, cond=treatment, macrotran.lookup$variable)
macrotranRT.stats <- full_join(macrotranRT.kw, full_join(macrotranRT.dunn,macrotranRT.jonck, by='Variable' ), by='Variable')

write.csv(macrotranRT.stats, file="../data_derived/macrotran_RT_treatment_stats.csv")

# Across lakes
macrotranRT.lake.kw <- kw(macrotran_lg_RT, variable, val=value, cond=Lake, macrotran.lookup$variable)
macrotranRT.lake.dunn <- dunn(macrotran_lg_RT, variable, val=value, cond=Lake,macrotran.lookup$variable)
macrotranRT.lake.stats <- full_join(macrotranRT.lake.kw, macrotranRT.lake.dunn, by='Variable' )

write.csv(macrotranRT.lake.stats, file="../data_derived/macrotran_RT_lake_stats.csv")