################################################################################
#PUGET SOUND ROCKFISH DATA CLEANING AND SUMSTATS
################################################################################

## This script includes code used for data cleaning and summary statistics on the 2009-2023 puget sound rockfish data

################################################################################
#SET UP
################################################################################

## Load libraries
library(tidyverse)
library(vegan)
library(ggpubr)#graphing
library(cowplot)#graphing
library(FSA)#kruskal-wallace diagnostics
library(PMCMRplus)#kruskal-wallace diagnostics
library(changepoint)
library(zoo)
library(egg)
library(car)

## Clear workspace
rm(list = ls())

## set your paths in a project folder 
input <- "C:/Users/shelledyk/OneDrive - Seattle Aquarium/Documents/Rockfish_Data_Analysis/data_input"
output <- "C:/Users/shelledyk/OneDrive - Seattle Aquarium/Documents/Rockfish_Data_Analysis/data_output"
code <- "C:/Users/shelledyk/OneDrive - Seattle Aquarium/Documents/Rockfish_Data_Analysis/code" 
fig <- "C:/Users/shelledyk/OneDrive - Seattle Aquarium/Documents/Rockfish_Data_Analysis/figures"

## load raw fish count data
setwd(input)
dat <- read.csv("RF_100m.csv", header=TRUE)

#remove empty columns and rows
dat <- dat[,1:24]
dat <- na.omit(dat)

#format date column for easy plotting
dat$Date <- as.Date(dat$Date, format = "%m/%d/%Y")
dat$Year <- as.numeric(format(dat$Date, "%Y"))
dat$Month <- as.numeric(format(dat$Date, "%m"))

#add season
dat <- dat %>% mutate(Season =
                        case_when(Month == 1 ~ "Winter",
                                  Month == 2 ~ "Winter",
                                  Month == 3 ~ "Spring",
                                  Month == 4 ~ "Spring",
                                  Month == 5 ~ "Spring",
                                  Month == 6 ~ "Summer",
                                  Month == 7 ~ "Summer",
                                  Month == 8 ~ "Summer",
                                  Month == 9 ~ "Fall",
                                  Month == 10 ~ "Fall",
                                  Month == 11 ~ "Fall",
                                  Month == 12 ~ "Winter"))

#rearrange columns
dat <- dat[c(1,3,2,27,26,25,4:24)]

#Select data of interest:
dat <- dat %>% 
  filter(., Direction == "F", Transect == "T1", Site != "Alki Fishing Reef") %>%
  dplyr::select(., -c(Direction, Transect))

#wide to long format
long.dat <- dat %>%
  pivot_longer("Black":"Puget_Sound", names_to = "Species", values_to = "Count")

################################################################################
#SUMMARY STATISTICS
################################################################################

#Remove YOY to calculate Shannon-Weaver diversity index
sum.dat <- filter(long.dat, Species != "YOY") #removeYOY

#Add column with Shannon-Weaver diversity index for each date and site
sum.dat <- sum.dat %>% 
  group_by(Site, Date) %>%
  mutate(Shannon = diversity(Count, "shannon"),
         Richness = sum(Count > 0))

#Add column with adult rockfish count for each date and site
sum.dat <- sum.dat %>% 
  subset(., Species != "Cabezon" & Species != "Lingcod" & Species != "Greenling" & Species != "Painted Greenling" & Species != "Wolfeel") %>%
  group_by(Site, Date) %>%
  mutate(RF_Count = sum(Count))

################################################################################
##ENVIRONMENTAL DATA PREP
################################################################################

#Puget Sound data from here: https://ecology.wa.gov/Research-Data/Monitoring-assessment/Puget-Sound-and-marine-monitoring

#load data
env.dat <- read_csv("EIM_Env_Data.csv")

#clean data
env.dat$Date <- mdy_hms(env.dat$Field_Collection_Start_Date_Time)
env.dat$Date <- date(env.dat$Date)

env.dat <- env.dat %>% 
  dplyr::select(., c(Location_ID, Date, Field_Collection_Upper_Depth, Field_Collection_Lower_Depth,Base_Parameter_Name, Statistical_Basis, Reslt_Parameter_Name, Result_Value, Result_Unit, Sample_Size))

#match sites to basin
translate <- read_csv("Location_translation.csv")
translate <- translate[,1:4]
env.dat <- merge(env.dat, translate, by = 'Location_ID', all.x=TRUE)

#Select parameters of interest
env.dat <- env.dat %>% 
  filter(., Statistical_Basis == "Median") %>%
  filter(., Base_Parameter_Name %in% c("Dissolved Oxygen", "Temperature, water", "pH"))

env.dat <- env.dat %>%
  filter(., Location %in% c("Central Puget Sound", "Hood Canal", "South Puget Sound", "North Puget Sound"))

#Housekeeping
rm(translate)

################################################################################
##YOY TABLE: RECRUITMENT EVENTS
################################################################################

#Subset to include only YOY observations
YOY <- long.dat %>% subset(., Species == "YOY" & Count > 0)

#Create a table
YOY <- YOY %>% 
  select(., c(Location, Site, Date, Season, Count)) %>%
  print()

#Export table to csv


#export plot

################################################################################
################################################################################

#CODE ON HOLD:
## function to place the last column [, ncol] in the first column position i.e. [, 1]
front.ofthe.line <- function(dat){
  num.col <- ncol(dat)
  dat <- dat[c(num.col, 1:num.col-1)]
  return(dat)
}


## create unique identifier for each combination of site / transect 
## (as all sites have T1, T2, etc.)
create.key <- function(dat){
  dat$Key <- dat$Site
  dat$Key <- with(dat, paste0(Key, Transect))
  dat <- front.ofthe.line(dat)
  return(dat)
}


## remove the "19" and "20" from year dates for easier visualization, e.g., "1991" --> "91" 
short.date <- function(dat){
  dat$short.date <- gsub("20","", as.character(dat$Year))
  dat <- front.ofthe.line(dat)
  return(dat)
}


## create unique identifyer for data w/ multiple transects
dat <- create.key(dat)


## create a shorter year identifier, e.g., "10" instead of "2010"
dat <- short.date(dat)

## visualize correlation coefficients
graphics.off()
windows(10.5, 8, record=T)


setwd(output)
spp_scores <- read.csv("spp_scores_log.csv", header=T)
names(spp_scores)[1]<-"spp_name"


pt.type <- 21
pt.col <- "black"
pt.fill <- "red"
text.col <- "red"


plot.spp.correlations <- function(x){
  t1 <- ggplot(data=data, aes(NMDS1, NMDS2)) + my.theme + coord_fixed() +
    geom_point(data=data, aes(NMDS1, NMDS2), pch=pt.type, fill="gray", col="gray") +
    geom_segment(data=spp_scores, aes(x=spp_x, y=spp_y, xend=0, yend=0)) + 
    geom_point(data=spp_scores, aes(x=spp_x, y=spp_y)) +
    geom_text(data=spp_scores, aes(x=spp_x, y=spp_y, label=spp_name), color=text.col)
  return(t1)
}


p3 <- plot.spp.correlations(data)  
print(p3)
