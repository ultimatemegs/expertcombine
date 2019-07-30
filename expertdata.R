library(prevalence) # doesn't work because jags is done
#library(rjags)
#library(expert)  # you can elicit quantiles with this!! 
library(mc2d)
library(ggplot2)
library(tidyverse)
library(magrittr)

# Read in data 
trial.dat=read.csv("./transmissionexpert.csv") 
head(trial.dat)

# Create distance datasets for each distance we estimated
dist.1km=trial.dat[which(trial.dat$dist == 1),]
dist.1km

# Plotting medians and intervals is straightforward using the pointinterval geom for individual experts
library(ggplot2)
library(ggstance)

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

# try using mcd2 
# shape defaults to 4 - operations research literature recommends varying a bit, bit i think we can keep for now
# I got the density extimation working - you have to give it a range... 
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

# Stefano says: 
# I don't think you need to make the pdf...
# just use rpert () with the min, mode and max to generate an empirical distribution and sample from it

## ALTERNATELY CAN TRY WITH THIS OTHER PACKAGE JEREMY WAS PLAYING WITH 
# av.val=betaPERT(a=dist.1km$min, m=dist.1km$best, b=dist.1km$max, k = 4, method = c("classic"))
# av.val=as.data.frame(av.val)

## AND WE COULD TRY THIS ONE WITH WEIGHTING 
https://blog.revolutionanalytics.com/2014/01/forecasting-by-combining-expert-opinion.html

