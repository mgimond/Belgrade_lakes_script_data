# This R script creates median polish tables from the organics.xls
# dataset

library(readxl)
library(tidyverse)


####  FUNCTION: Medium polish plot ----
t.plot <- function(df.in, x, y, z, title=NULL, effect=FALSE){
  # Create data frame
  x.lab <- as.character(substitute(x))
  y.lab <- as.character(substitute(y))
  z.lab <- as.character(substitute(z))
  xx <- eval(substitute(x), df.in)
  yy <- eval(substitute(y), df.in)
  zz <- eval(substitute(z), df.in)
  df <- data.frame(xx,yy,zz)
  
  # Plot the data
  P <-  ggplot(df, aes(x=xx, y=yy, fill=zz)) +geom_tile() +
    geom_text(aes(label = round(df$zz, 2))) +
    scale_fill_gradient(low = "grey90", high = "grey40") +
    theme(plot.title = element_text(size = rel(2)),
          axis.ticks = element_blank(),
          legend.position="none",
          panel.background = element_rect(fill = "white")) +
    scale_x_discrete(position = "top") +
    xlab(x.lab) + ylab(y.lab) +
    ggtitle(title)
  
  if(effect == FALSE){
    return(P)
  } else {
    top.l <- length(unique(df$yy))
    return(P + geom_vline(xintercept = 1.5) +
             geom_hline(yintercept = top.l -.5)
    )
  }
}

####  End of Tile plot function

####  FUNCTION: Median polish table ####
med2tbl <- function(M.in,cnames,rnames){
  cnames2 <- append("Global", cnames)
  rnames2 <- append("Global", rnames )
  dfa <- rbind( t(as.data.frame(M.in[3]))  , as.data.frame(M.in$residuals) )
  dfb <- cbind( `Row effect` = factor(rnames2,levels=rnames2), 
                Global=c(M.in$overall, M.in$row) , dfa)
  
  # Create long form of dfb matrix
  df.out <- dfb  %>%
    gather(key="Column effect", value = "Value", -`Row effect`) %>%
    mutate( `Row effect` = factor(`Row effect`, levels=rev(rnames2)),
            `Column effect` = factor(`Column effect`, levels=cnames2))
  return(df.out)
}

####  End of tile plot function

# Load data file ----
infile <- read_excel("../data_raw/Organics data.xls", na=".")

# Tidy data table
dat <- infile %>% 
  mutate(Treatment = fct_relevel(Treatment, "R", "BD", "UD"),
         Summer = case_when(Round == 1~ "Early", Round ==2 ~ "Late")) %>% 
         na.omit() %>% 
         select(-Round)
     

#### Summarize by Lake and Treatment ----
dat.sum <- group_by(dat, Lake, Treatment) %>% summarize(orgs = median(`Percent Organics`, na.rm=TRUE))

# Plot raw table
t.plot(df.in=dat.sum, x=Lake, y=Treatment, z=orgs)

#### 1st iteration of a median polish
dat.sum.w <- spread(dat.sum,key = Lake, value = orgs) %>% as.data.frame
#colnames(dat.sum.w) <- dat.sum.w[1,]
rownames(dat.sum.w) <- t(dat.sum.w[,1])

M1 <- medpolish(dat.sum.w[,-1], maxiter = 3)

cnames <- colnames(dat.sum.w)
#rnames <- c("Treament",rownames(dat.sum.w))
rnames <- rownames(dat.sum.w)

# Plot polished table
df.M1 <- med2tbl(M1, cnames, rnames)
colnames(df.M1) <- c("Treatment effect", "Lake effect",  "Value")
t.plot(df.in=df.M1, x=`Lake effect`, y=`Treatment effect`, z=Value, effect=TRUE, title="All summer")

# Check for non-additivity
plot(M1) # Looks good

#### Summarize by Treatment and Time of summer----
dat.sum <- group_by(dat, Summer, Treatment) %>% summarize(orgs = median(`Percent Organics`, na.rm=TRUE))

# Plot raw table
t.plot(df.in=dat.sum, x=Summer, y=Treatment, z=orgs)

#### 1st iteration of a median polish
dat.sum.w <- spread(dat.sum,key = Summer, value = orgs) %>% as.data.frame
#colnames(dat.sum.w) <- dat.sum.w[1,]
rownames(dat.sum.w) <- t(dat.sum.w[,1])

M1 <- medpolish(dat.sum.w[,-1], maxiter = 3)

cnames <- colnames(dat.sum.w)
#rnames <- c("Treament",rownames(dat.sum.w))
rnames <- rownames(dat.sum.w)

# Plot polished table
df.M1 <- med2tbl(M1, cnames, rnames)
colnames(df.M1) <- c("Treatment effect", "Summer effect",  "Value")
t.plot(df.in=df.M1, x=`Summer effect`, y=`Treatment effect`, z=Value, effect=TRUE)

# Check for non-additivity 
plot(M1)  # (note: trend may be present)


#### Summarize by Lake and Time of summer----
dat.sum <- group_by(dat, Summer, Lake) %>% summarize(orgs = median(`Percent Organics`, na.rm=TRUE))

# Plot raw table
t.plot(df.in=dat.sum, x=Summer, y=Lake, z=orgs)

#### 1st iteration of a median polish
dat.sum.w <- spread(dat.sum,key = Summer, value = orgs) %>% as.data.frame
#colnames(dat.sum.w) <- dat.sum.w[1,]
rownames(dat.sum.w) <- t(dat.sum.w[,1])

M1 <- medpolish(dat.sum.w[,-1], maxiter = 3)

cnames <- colnames(dat.sum.w)
#rnames <- c("Treament",rownames(dat.sum.w))
rnames <- rownames(dat.sum.w)

# Plot polished table
df.M1 <- med2tbl(M1, cnames, rnames)
colnames(df.M1) <- c("Lake effect", "Summer effect",  "Value")
t.plot(df.in=df.M1, x=`Summer effect`, y=`Lake effect`, z=Value, effect=TRUE)

# Check for non-additivity (note: trend may be present)
plot(M1)

#### Summarize by Lake and Treatment for early summer only----
dat.sum <- dat %>% 
  filter(Summer == "Early") %>% 
  group_by(Lake, Treatment) %>% 
  summarize(orgs = median(`Percent Organics`, na.rm=TRUE))

# Plot raw table
t.plot(df.in=dat.sum, x=Lake, y=Treatment, z=orgs)

#### 1st iteration of a median polish
dat.sum.w <- spread(dat.sum,key = Lake, value = orgs) %>% as.data.frame
#colnames(dat.sum.w) <- dat.sum.w[1,]
rownames(dat.sum.w) <- t(dat.sum.w[,1])

M1 <- medpolish(dat.sum.w[,-1], maxiter = 3)

cnames <- colnames(dat.sum.w)
#rnames <- c("Treament",rownames(dat.sum.w))
rnames <- rownames(dat.sum.w)

# Plot polished table
df.M1 <- med2tbl(M1, cnames, rnames)
colnames(df.M1) <- c("Treatment effect", "Lake effect",  "Value")
t.plot(df.in=df.M1, x=`Lake effect`, y=`Treatment effect`, z=Value, effect=TRUE, title="Early summer") 

# Check for non-additivity
plot(M1)


#### Summarize by Lake and Treatment for late summer only----
dat.sum <- dat %>% 
  filter(Summer == "Late") %>% 
  group_by(Lake, Treatment) %>% 
  summarize(orgs = median(`Percent Organics`, na.rm=TRUE))

# Plot raw table
t.plot(df.in=dat.sum, x=Lake, y=Treatment, z=orgs)

#### 1st iteration of a median polish
dat.sum.w <- spread(dat.sum,key = Lake, value = orgs) %>% as.data.frame
#colnames(dat.sum.w) <- dat.sum.w[1,]
rownames(dat.sum.w) <- t(dat.sum.w[,1])

M1 <- medpolish(dat.sum.w[,-1], maxiter = 3)

cnames <- colnames(dat.sum.w)
#rnames <- c("Treament",rownames(dat.sum.w))
rnames <- rownames(dat.sum.w)

# Plot polished table
df.M1 <- med2tbl(M1, cnames, rnames)
colnames(df.M1) <- c("Treatment effect", "Lake effect",  "Value")
t.plot(df.in=df.M1, x=`Lake effect`, y=`Treatment effect`, z=Value, effect=TRUE, title="Late summer") 


# Check for non-additivity
plot(M1)

