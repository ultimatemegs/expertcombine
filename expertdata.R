# Title: Combining expert estimates
# Author: Megan Barnes 
# Date Created: 30 July 2018
# Date last modified: 31 July 2018

# Objective: 
# Combine expert estimates to create a joint estimates using multiple methods
# Create visualisations to support delphi process and publication 

library(mc2d)
library(ggplot2)
library(tidyverse)
library(magrittr)
#library(expert)  # you can elicit quantiles with this!! -> possibly use for micro? 

# Read in data 
trial.dat=read.csv("./transmissionexpert.csv") # some example data
head(trial.dat)

# Create distance datasets for each distance we estimated
dist.1km=trial.dat[which(trial.dat$dist == 1),]
dist.1km

# Plotting medians and intervals is straightforward using the pointinterval geom for individual experts
library(ggplot2)
library(ggstance)
library(magrittr)

# for 1km estimates... 
# Estimate a combined distribution using the averaging approach (mcbride, burgman, ??hemming et al) 
# check process with victoria to see if she does it differently... 
combined <- data.frame(ex = "Combined", dist = 1, min = mean(dist.1km$min), best = mean(dist.1km$best), max = mean(dist.1km$max)) # create combined row 
dist1km_plus <- rbind(dist.1km, combined)

# Plot 
Est_Plot <- dist1km_plus %>%  # need to generalise and convert to function
  ggplot(aes(y = ex, x = best, xmin = min, xmax = max, width = 1)) + 
  geom_pointrangeh() +   # there is an equivalent bayes variation called geom_pointinterval in package tidybayes
  #https://www.rdocumentation.org/packages/tidybayes/versions/1.1.0/topics/geom_pointinterval
  theme_grey() + 
  xlab("Estimates for each expert") + ylab(NULL) + 
  ggtitle("Expert estimates for 1km distance")

Est_Plot

# TO DOs
# Make Est Plot for all distances
# Re-order (set level order with levels()) so that Combined is at the bottom 
# make dots blue or figure out what that nice blue and grey theme is 
# Make a multi-panel plot that combines them all using gg functionality so can have shared titles ect. 
# e.g. cowplot, ggmap etc.


# Ok now we did it the easy way, let's do it a better way. 
# We want to: 
# 1. estimate a pert distribution for each expert
# 2. take a bunch of samples from each expert
# 3. "pool" (this means make into one vector of values)
# 4. Sample from the pooled data 

# Using mcd2 
# shape defaults to 4 - operations research literature recommends varying a bit, bit i think we can keep for now - investigate later if we want
# I got the density extimation working - you have to give it a range.
# i think we maybe want to use the ppart first though, so then we can randomly sample the distribution (maybe with rpart? maybe with just sample)
# THE DOCUMENTATION IS CRAPOLA, BUT HAVE ASKED ABOUT IT

library(mc2d)
# # test with example values 
# curve(dpert(x, min=3, mode=5, max=10, shape=6), from = 2, to = 11, lty=3)
# curve(dpert(x, min=3, mode=5, max=10), from = 2, to = 11, add=TRUE)
# curve(dpert(x, min=3, mode=5, max=10, shape=2), from = 2, to = 11, add=TRUE, lty=2)
# legend(x = 8, y = 2, c("Default", "shape:2", "shape:6"), lty=1:3)

# test with an individual set of values
dpert(x, min=10, mode=50, max=70, shape = 4) # doesn't work :(
x <- c(0.025, 0.5, 0.975)
dpert(x=x, min=10, mode=50, max=70, shape = 4) # returns all zeros... :/ 
curve(dpert(x, min=10, mode=50, max=70, shape = 4), from = 9, to = 100, lty=3) # works if you specify plotting range! 

# try with vectors of all the experts
x=c(0.025, 0.5, 0.975)  # vector of quantiles, set at 2.5% 50% and 97.5%
mins = dist.1km$min # vector of minima
maxs = dist.1km$max # vector of maxima
modes = dist.1km$best # vector (modes -> these are the most plausible estimates

dpert(x, min=mins, mode=modes, max=maxs, shape = 4) # # returns all zeros... :/ 
dpert(x=x, min=mins, mode=modes, max=maxs, shape = 4) # returns all zeros... :/ 
curve(dpert(x, min=mins, mode=modes, max=maxs, shape = 4), from = 9, to = 100, lty=3) # looks like need to do each one

# referenced and stacked?
curve(dpert(x, min=mins[1], mode=modes[1], max=maxs[1], shape = 4), from = 0, to = 100, lty=3) 
curve(dpert(x, min=mins[2], mode=modes[2], max=maxs[2], shape = 4), from = 0, to = 100, lty=3, add= TRUE) 
curve(dpert(x, min=mins[3], mode=modes[3], max=maxs[3], shape = 4), from = 0, to = 100, lty=3, add=TRUE) 
curve(dpert(x, min=mins[4], mode=modes[4], max=maxs[4], shape = 4), from = 0, to = 100, lty=3, add=TRUE) 
curve(dpert(x, min=mins[5], mode=modes[5], max=maxs[5], shape = 4), from = 0, to = 100, lty=3, add=TRUE)
# works, need to extend y axis though, and only shows density, still haven't got the distributions to estimate... 

# generate an empirical distribution for each expert 
emp_dist_1 <- as.data.frame(rpert(1000, min=mins[1], mode=modes[1], max=maxs[1], shape=4))
emp_dist_2 <- as.data.frame(rpert(1000, min=mins[2], mode=modes[2], max=maxs[2], shape=4))
emp_dist_3 <- as.data.frame(rpert(1000, min=mins[3], mode=modes[3], max=maxs[3], shape=4))
emp_dist_4 <- as.data.frame(rpert(1000, min=mins[4], mode=modes[4], max=maxs[4], shape=4))

# combine the data samples - "POOL"
emp_dist_1$expert <- "ONE"
names(emp_dist_1)[1] <- "sample"
emp_dist_2$expert <- "TWO"
names(emp_dist_2)[1] <- "sample"
emp_dist_3$expert <- "THREE"
names(emp_dist_3)[1] <- "sample"
emp_dist_4$expert <- "FOUR"
names(emp_dist_4)[1] <- "sample"
df_all <- data.frame(rbind(emp_dist_1, emp_dist_2, emp_dist_3, emp_dist_4))

# Double the dataset to make joint + individal dataset
df_all_joint <- df_all
df_all_joint$expert <- "JOINT"
df_all_2 <- rbind(df_all, df_all_joint)
table(df_all_2$expert)

### Make Pretty Plots ###

# Histogram with density plot 
df <- emp_dist_1 # Just one - can make multiple and facet with sample style if we like it
p <- ggplot(df, aes(x=emp_dist_1)) +
  geom_histogram(aes(y = ..density..), color="darkgrey", fill="grey", binwidth=2) +
  geom_density(alpha=0.2, fill = "antiquewhite", colour="lightgrey") +
  geom_vline(aes(xintercept=mean(emp_dist_1)), linetype="dashed", col="darkgoldenrod2") + 
  labs(title="Estimated likelihood distribution for transmission over 1km (1000 samples)", x="Estimated proportion transmitted", y = "Count") +  # tweak formatting
  theme_classic() 
p

# Overlaid density plots

# select which dataset
df <- df_all
df<- df_all_2

t <- ggplot(df, aes(x = sample, fill = expert)) +
  geom_density(alpha = .5)
t

#Overlaid histograms
t <- ggplot(df, aes(x = sample, fill = expert)) +
  geom_histogram(binwidth = 2, alpha = .5, position = "identity")
t

#Facet Histograms
# Need to first specify ordering with levels in the dataset
df_all_2$expert <- factor(df_all_2$expert, levels= c("ONE","TWO", "THREE", "FOUR", "JOINT")) # if numeric will likely self order except joint
df<- df_all_2

# PLot 
f <- ggplot(df, aes(x = sample)) + 
  #geom_histogram(color="darkgrey", fill="grey", binwidth=2) +
  geom_histogram(aes(fill = expert), colour="lightgrey", lty=1,binwidth=2) +  # Changes fill w/ grey edges, to make a better match need to specify wuith scalefillmanual
  facet_grid(expert ~ ., scales = "free") + 
  labs(x="Estimated proportion transmitted", y = "Count") + 
  ggtitle("Empirical pert-distributions for each expert")# + 
  #geom_text(element_text(family = "Georgia"))
f 

# some fun labelling ideas: http://bradleyboehmke.github.io/tutorials/histograms

########################################################  NOTES ######################################################## 

## ALTERNATELY CAN TRY WITH THIS OTHER PACKAGE
# av.val=betaPERT(a=dist.1km$min, m=dist.1km$best, b=dist.1km$max, k = 4, method = c("classic"))
# av.val=as.data.frame(av.val)

## POSSIBLY TRY THIS ONE WITH WEIGHTING 
## https://blog.revolutionanalytics.com/2014/01/forecasting-by-combining-expert-opinion.html
