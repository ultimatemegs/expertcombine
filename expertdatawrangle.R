# Title: Wrangle expert elicited data from seasketch 
# Author: Megan Barnes 
# Date Created: 30 July 2018
# Date last modified: 31 July 2018

# Goal: manipulate data to format that can be fed into expertdata for genberating distributions and visualisations on the fly. 

# Libraries
library(tidyverse)
# @jeremyringma to add spatial stuff

# grab csv
expert.test.seasketch <- read_csv("./Test_survey_coords.csv")

# we need these columns only
expert.test.seasketch$NAME  # these are places
expert.test.seasketch$MEDIAN
expert.test.seasketch$HIGHEST_VA
expert.test.seasketch$LOWEST_VAL
expert.test.seasketch$UNIT
expert.test.seasketch$FID

# for parameter X... 
# for each country... 
# for each unit type? or will we convert? 

