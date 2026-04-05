### 01_data.R ##################################################################
# By: Ryan Ng | Date: 5 Apr 2026 ###############################################

# This script reads in demographic data for Astragalus utahensis, published by 
# Baer & Maron (2018, 2019, 2020) 
################################################################################

library(dplyr)
library(popbio)
library(ggplot2)
library(rlang)
library(patchwork)
library(tidyr)

# read demographic data  
demodata <- read.csv("demodata.csv")             # 7621 rows 
# remove Odgen site (population was lost to wildfire in 2014)
demodata <- demodata[demodata$Site != "Ogden", ] # 7003 rows 

# some data exploration 
colnames(demodata)               # col names and type 
demodata %>% count(Range)        # obs in each range 
demodata %>% count(Range, Site)  # obs in each site 
demodata %>% group_by(Site) %>%  # unique census in each site 
  summarise(census_years = paste(sort(unique(TransYear)), collapse = ","))

# rename life stages 
demodata <- demodata %>%
  mutate(
    lifestage = factor(lifestage,
                       levels = c(1, 2, 3),
                       labels = c("seedling", "juvenile", "adult")),
    lifestageNext = factor(lifestageNext,
                           levels = c(1, 2, 3),
                           labels = c("seedling", "juvenile", "adult")))

#################### SUMMARY ##############################
# total of 7003 observations 
  # 3358 from CENTER 
    # Bountiful = 1002 (1314, 1415, 1516)
    # Providence = 1137 (1314, 1415, 1516)
    # Uinta = 1219 (1314, 1415, 1516)
  # 3645 from NORTH 
    # Nordic Center = 994 (1314, 1415, 1516)
    # Reservoir = 1030 (1314, 1415, 1516)
    # Swan = 512 (1415, 1516)
    # Wolverine = 1109 (1314, 1415, 1516)
###########################################################

