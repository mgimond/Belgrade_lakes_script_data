library(dplyr)
library(readxl)
library(ggplot2)
library(tukeyedar)

# Load the data
dat <- read_xls("../data_raw/Organics data.xls", na = ".")

# Convert Round to factor
dat$Round <- as.factor(dat$Round)

# Plot the data
ggplot(dat) + aes(y=`Percent Organics`, x=Lake) + geom_boxplot() +
  facet_wrap(~Round)

# The data are strongly skewed. They also exhibit strong heteroskedasticity. 
# Use the following implementation of the spread-level plot to extract a
# transformation power (see https://mgimond.github.io/ES218/Week07b.html#variations_of_the_s-l_plot)

sl <- dat %>%
  group_by(Lake, Round)  %>%
  summarise (level  = log(median(`Percent Organics`, na.rm=TRUE)),
             IQR = IQR(`Percent Organics`, na.rm=TRUE),  # Computes the interquartile range
             spread = log(IQR))

ggplot(sl, aes(x=level, y=spread)) +geom_point() + 
  stat_smooth(method="lm", se=FALSE) +
  xlab("Median (log)") + ylab("Spread (log)") 

lm(spread ~ level, sl) # The slope is 2.08 giving us a power of (1 - slope) = -1


# Apply an inverse transformation using Box-Cox method (this preserves rank)
dat$org_inverse <- eda_re(dat$`Percent Organics`, p=-1, tukey=FALSE)

# Plot the transformed data
ggplot(dat) + aes(y=org_inverse, x=Lake) + geom_boxplot() +
  facet_wrap(~Round) +ylab("Inverse percent")

# Run an anova analysis. Include interactive terms.
M.inv.int <- lm(org_inverse ~ Lake + Round + Lake:Round, dat) # Diagnostic plots look good
## car::Anova(M.inv.int, type=2) # If Type II is sought, use car's Anova function
anova(M.inv.int) # Interaction terms are not significant


# Since there is no interaction:
anova(lm(org_inverse ~ Lake, dat))
with( dat, pairwise.t.test( org_inverse, Lake, p.adjust.method = "bonferroni") )

# Limit to Round 1
anova(lm(org_inverse ~ Lake, dat[dat$Round == 1,]))  # Significant
with(dat[dat$Round == 1,], pairwise.t.test( org_inverse, Lake, p.adjust.method = "bonferroni"))

# Limit to Round 2
anova(lm(org_inverse ~ Lake, dat[dat$Round == 2,])) # Not significant


