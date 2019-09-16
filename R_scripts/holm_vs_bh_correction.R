library(dplyr)
library(purrr)
library(tibble)
library(forcats)
library(ggplot2)
library(gridExtra)

load("../data_derived/data_tables.rdata")

gg.riparian <- riparian_lg %>% na.omit() %>% 
  split(.$variable) %>% 
  map( ~kruskal.test(value ~ treatment, data=.)[c("p.value", "statistic")]) %>%
  map_df(unlist) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% 
  magrittr::set_colnames(c("Variable", "P.orig", "H")) %>% 
  mutate(holm = p.adjust(P.orig, method="holm"),
         BH = p.adjust(P.orig, method="BH"),
         case = case_when(holm < 0.05 & BH < 0.05 ~ "a",
                          holm >= 0.05 & BH < 0.05 ~ "b",
                          TRUE ~ "c")) %>% 
  select(-P.orig, -H) %>% 
  arrange(Variable)%>% 
  ggplot(.) + aes(holm,BH,col= case) + geom_point(alpha=0.5,show.legend = FALSE) + coord_equal(xlim=c(0,1), ylim=c(0,1)) +
  geom_abline(intercept=0, slope=1, col="grey") + 
  scale_color_manual(values=c("a" = "orange", "b" = "red", "c"="black")) + ggtitle("Riparian")


gg.littoral <- littoral_lg %>% na.omit() %>% 
  split(.$variable) %>% 
  map( ~kruskal.test(value ~ treatment, data=.)[c("p.value", "statistic")]) %>%
  map_df(unlist) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% 
  magrittr::set_colnames(c("Variable", "P.orig", "H")) %>% 
  mutate(holm = p.adjust(P.orig, method="holm"),
         BH = p.adjust(P.orig, method="BH"),
         case = case_when(holm < 0.05 & BH < 0.05 ~ "a",
                          holm >= 0.05 & BH < 0.05 ~ "b",
                          TRUE ~ "c")) %>% 
  select(-P.orig, -H) %>% 
  arrange(Variable)%>% 
  ggplot(.) + aes(holm,BH,col= case) + geom_point(alpha=0.5,show.legend = FALSE) + coord_equal(xlim=c(0,1), ylim=c(0,1)) +
  geom_abline(intercept=0, slope=1, col="grey") + 
  scale_color_manual(values=c("a" = "orange", "b" = "red", "c"="black")) + ggtitle("Littoral")


gg.macrotran <- macrotran_lg %>% na.omit() %>% 
  split(.$variable) %>% 
  map( ~kruskal.test(value ~ treatment, data=.)[c("p.value", "statistic")]) %>%
  map_df(unlist) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% 
  magrittr::set_colnames(c("Variable", "P.orig", "H")) %>% 
  mutate(holm = p.adjust(P.orig, method="holm"),
         BH = p.adjust(P.orig, method="BH"),
         case = case_when(holm < 0.05 & BH < 0.05 ~ "a",
                          holm >= 0.05 & BH < 0.05 ~ "b",
                          TRUE ~ "c")) %>% 
  select(-P.orig, -H) %>% 
  arrange(Variable)%>% 
  ggplot(.) + aes(holm,BH,col= case) + geom_point(alpha=0.5,show.legend = FALSE) + coord_equal(xlim=c(0,1), ylim=c(0,1)) +
  geom_abline(intercept=0, slope=1, col="grey") + 
  scale_color_manual(values=c("a" = "orange", "b" = "red", "c"="black")) + ggtitle("Macrotran")

gg.macrorock <- macrorock_lg %>% na.omit() %>% 
  split(.$variable) %>% 
  map( ~kruskal.test(value ~ treatment, data=.)[c("p.value", "statistic")]) %>%
  map_df(unlist) %>% 
  t() %>% as.data.frame() %>% rownames_to_column() %>% 
  magrittr::set_colnames(c("Variable", "P.orig", "H")) %>% 
  mutate(holm = p.adjust(P.orig, method="holm"),
         BH = p.adjust(P.orig, method="BH"),
         case = case_when(holm < 0.05 & BH < 0.05 ~ "a",
                          holm >= 0.05 & BH < 0.05 ~ "b",
                          TRUE ~ "c")) %>% 
  select(-P.orig, -H) %>% 
  arrange(Variable)%>% 
  ggplot(.) + aes(holm,BH,col= case) + geom_point(alpha=0.5,show.legend = FALSE) + coord_equal(xlim=c(0,1), ylim=c(0,1)) +
  geom_abline(intercept=0, slope=1, col="grey") + 
  scale_color_manual(values=c("a" = "orange", "b" = "red", "c"="black")) + ggtitle("Macrorock")


grid.arrange(gg.riparian, gg.littoral, gg.macrorock, gg.macrotran, nrow=2, name="p-values")
