library(tidyverse)
library(purrr)
library(forcats)
library(stringr)
library(svglite)
library(tiff)
library(ggthemes)
library(grid)

# Load data tables
load("../data_derived/data_tables.rdata")

# Color palettes used in subsequent plots
lakes.col <- RColorBrewer::brewer.pal(8, "Set2")[c(3,4,7)]  # Across lajes
treat.col <- c("#43CD80", "#fec44f", "#d95f0e")  # Across treatments


# Box-plot function
plot1 <- function(data, x, y, cond, sub.var,title, col=c("grey80", "grey60", "grey40") , depth.sub=NULL,
                  label.rm = FALSE, name = name) {
  x <- enquo(x)
  y <- enquo(y)
  cond <- enquo(cond)
  cname <- enquo(name)
  data2 <- data %>% group_by(!!x) %>% na.omit() %>% 
    filter(!!cond %in% sub.var )
  if( !is.null(depth.sub) ) {
    data2 <- filter(data2, depth %in% depth.sub)
  }
  P1 <- ggplot(data2) + aes(x=!!x, y=!!y, fill=!!x, col=!!x) +
    geom_boxplot(col=NA, coef=0, outlier.size = 0 ) +
    geom_boxplot(aes(ymin=..lower.., ymax=..upper..), fatten=3,coef=0, outlier.shape = NA, show.legend=FALSE) +
    geom_point(size=0.5, alpha=.3, col="black", show.legend=FALSE, shape=16) +
    #stat_summary(fun.y = "mean", geom="point", cex=2.0, pch=21, bg="white", show.legend=FALSE) +
    stat_summary(fun.y = "median", geom="point", cex=2.0, pch=21, bg="white", show.legend=FALSE, col="black") +
    scale_fill_manual(values=col) +  
    scale_colour_manual(values=col) +
    coord_flip() +
    theme_minimal() +
    facet_wrap(  vars(!!cname) , scales="free", ncol=1, strip.position = "left") +
    guides(fill = guide_legend(reverse = TRUE, title=title)) +
    theme(axis.title.x=element_blank(),
          axis.text.x= element_text(size=7,vjust=3),
          axis.text.y = element_blank(),
          axis.title.y=element_blank(),
          strip.text.y=element_text(angle=180),
          legend.position = "right",
          legend.title = NULL,
          panel.spacing = unit(0, "lines"),
          strip.text = element_text(hjust=1))  
  
  if (label.rm == FALSE){
    return(P1 )
  } else {
    return(P1 + theme(strip.text.y=element_blank()))
  }
}


### RIPARIAN  SUMMARIES----

# Boxplots across lakes ----
plot1(riparian_lg, x=Lake, y=value, cond=variable, sub.var=riparian.lookup$variable, title="Lake",
#      col=c("grey80", "grey60", "grey40"))
      col = lakes.col)

# Print plot to file
ggsave("../plots/riparian_lakes.svg", width=6, height=9, units="in", pointsize=12)
ggsave("../plots/riparian_lakes.tif", width=6, height=9, units="in", device="tiff", dpi=300)

# Boxplots across treatments ----
plot1(riparian_lg, x=treatment, y=value, cond=variable, sub.var=riparian.lookup$variable, title="Treatment",
#      col=c("#F2EA96", "#fec44f", "#d95f0e") )
       col = treat.col)

# Print plot to file
ggsave("../plots/riparian_treatment.svg", dpi=400, width=6, height=9, units="in",pointsize=12)
ggsave("../plots/riparian_treatment.tif", width=6, height=9, units="in", device="tiff", dpi=300)

### LITTORAL SUMMARIES----

# Boxplots across lakes ----
# plot1(littoral_lg, x=Lake, y=value, cond=variable, sub.var=littoral.lookup$variable, title="Lake",
#       col = lakes.col, name = fullname)

p1 <- plot1(subset(littoral_lg, depth == 0.5), x=Lake, y=value, cond=variable, sub.var=littoral.lookup$variable, title="Lake",
      col = lakes.col, name = name) + ggtitle("0.5 meter depth")
p2 <- plot1(subset(littoral_lg, depth == 1), x=Lake, y=value, cond=variable, sub.var=littoral.lookup$variable, title="Lake",
            col = lakes.col, name = name, label.rm = TRUE) + ggtitle("1.0 meter depth")

ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend = "bottom", widths = c(1.6,1))


# Print plot to file
ggsave("../plots/Littoral_lakes.svg", width=6, height=10, units="in")
ggsave("../plots/Littoral_lakes.tif",dpi=300, width=6, height=10, units="in", device="tiff")

# Boxplots across treatments ----
# plot1(littoral_lg, x=treatment, y=value, cond=variable, sub.var=littoral.lookup$variable, title="Treatment",
#       col = treat.col, name = fullname)

p1 <- plot1(subset(littoral_lg, depth == 0.5), x=treatment, y=value, cond=variable, sub.var=littoral.lookup$variable, title="Lake",
            col = treat.col, name = name) + ggtitle("0.5 meter depth")
p2 <- plot1(subset(littoral_lg, depth == 1), x=treatment, y=value, cond=variable, sub.var=littoral.lookup$variable, title="Lake",
            col = treat.col, name = name, label.rm = TRUE) + ggtitle("1.0 meter depth")

ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = TRUE, legend = "bottom", widths = c(1.6,1))


# Print plot to file (or save plot as png 900 x 1400)
ggsave("../plots/Littoral_treatment.svg", width=6, height=10, units="in")
ggsave("../plots/Littoral_treatment.tif",dpi=300, width=6, height=10, units="in", device="tiff")

### MACROINVERTEBRATE ROCK TRAP SUMMARIES----

# Boxplots across lakes ----
plot1(macrorock_lg, x=Lake, y=value, cond=variable, sub.var=macrorock.lookup$variable, title="Lake",
      col = lakes.col)

# Print plot to file
ggsave("../plots/macrorock_Lakes.svg", width=6, height=5, units="in")
ggsave("../plots/macrorock_Lakes.tif", width=6, height=5, units="in", device = "tiff")

# Boxplots across treatments ----
plot1(macrorock_lg, x=treatment, y=value, cond=variable, sub.var=macrorock.lookup$variable, title="Treatment",
      col = treat.col )

# Print plot to file
ggsave("../plots/macrorock_treatment.svg", width=6, height=5, units="in")
ggsave("../plots/macrorock_treatment.tif", width=6, height=5, units="in", device = "tiff")

### MACROINVERTEBRATE TRANSECT SUMMARIES----

# Boxplots across lakes ----
sub.var <- c("oforganisms", "meanrichness", "eptrichness", "fbi", "cotesum", "percentchironomidae", "percentcrustmol")
depth <- "T0.5"

plot1(macrotran_lg, x=Lake, y=value, cond=variable, sub.var = sub.var, title="Lake",
      col = lakes.col, depth.sub=depth)

# Print plot to file
ggsave("../plots/macrotran_Lakes.svg", width=6, height=4, units="in")
ggsave("../plots/macrotran_lakes.tif", width=6, height=4, units="in", device="tiff", dpi=300)

# Boxplots across treatments ----
plot1(macrotran_lg, x=treatment, y=value, cond=variable, sub.var=sub.var, title="Treatment",
      col = treat.col, depth.sub=depth )

# Print plot to file
ggsave("../plots/macrotran_treatment.svg", width=6, height=4, units="in")
ggsave("../plots/macrotran_treatment.tif", width=6, height=4, units="in", device="tiff", dpi=300)


