## This script performs data visualization for the 2009-2022 puget sound rockfish data

##Load libraries
library(tidyverse)
library(here)
library(vegan)
library(ggpubr)
library(cowplot)

##Load data
dat <- read_csv(here("data", "RF_100m.csv"))
dat <- dat[,1:24]

##Use "PugetSoundRockfishData.R" to clean data

#use the following code to ensure no fish have zero counts. Remove any columns with no counts, and remove non-adult rf before making data pivot longer
#sum(dat$Black) etc.
#no China, Tiger, Widow, Yellowtail, Canary
dat <- dat %>% select(., -c(China, Tiger, Widow, Yellowtail, Canary, Wolfeel, Cabezon, YOY, Lingcod, `Painted Greenling`, Greenling))

##SUMMARY DATA VIZ: LINE PLOT OF FISH COUNTS OVER TIME

#OVERALL
ggplot(data = long.dat, aes(x=Date, y=Count, group = Species, color = Species)) + 
  geom_line() + geom_point() + 
  theme_cowplot()

#BY SITE
ggplot(data = long.dat, aes(x=Date, y=Count, group = Species, color = Species)) + 
  geom_line() + geom_point() + 
  theme_cowplot() + 
  facet_wrap(~Site)

## LINE PLOTS OF SHANNON-WEINER DIVERSITY INDEX OVER TIME

ggplot(data = long.dat, aes(x=Date, y=Shannon)) + 
  geom_line() + geom_point() + 
  theme_cowplot() + 
  facet_wrap(~Site)
