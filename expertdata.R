library(prevalence)
library(rjags)
library(expert)
library(mc2d)

ex.data=read.csv("C:/Users/Jeremy/Dropbox/SESYNC PLASTIC/PLASTICPOLICY/Data/transmissionexpert.csv") 
head(ex.data)

dist.1km=ex.data[which(ex.data$dist == 1),]

av.val=betaPERT(a=dist.1km$min, m=dist.1km$best, b=dist.1km$max, k = 4, method = c("classic"))
av.val=as.data.frame(av.val)

x=c(.025, .5, .975)
min=c()
curve(dpert(x, min=dist.1km$min, mode=dist.1km$best, max=dist.1km$max, shape=4, log=FALSE))


