################################################################################
##PUGET SOUND ROCKFISH DATA CLEANING AND SUMSTATS
################################################################################

## This script includes code used for data cleaning and summary statistics on the 2009-2023 puget sound rockfish data
## Inputs: "RF_100m.csv", "EIM_Env_Data.csv", and "Location_translation.csv"

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
#library(changepoint)
library(zoo) #time transformation
#library(egg)
library(car) #diagnostics/regression
library(DataCombine)

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
                                  Month == 9 ~ "Autumn",
                                  Month == 10 ~ "Autumn",
                                  Month == 11 ~ "Autumn",
                                  Month == 12 ~ "Winter"))

#rearrange columns
dat <- dat[c(1,3,2,27,26,25,4:24)]

#Select data of interest:
dat <- dat %>% 
  filter(., Direction == "F", Transect == "T1", Site != "Alki Fishing Reef") %>%
  dplyr::select(., -c(Direction, Transect))

#replace Location names with accurate basin names
translate <- data.frame(
  old = c("North Puget Sound"), 
  new = c( "Admiralty Inlet"))

dat <- FindReplace(data = dat, Var = "Location", replaceData = translate, from = "old", to = "new", exact = TRUE, vector = FALSE)

rm(translate)

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

#convert to Date format
env.dat$Date <- mdy_hms(env.dat$Field_Collection_Start_Date_Time)
env.dat$Date <- date(env.dat$Date)

#select columns of interest
env.dat <- env.dat %>% 
  dplyr::select(., c(Location_ID, Date, Field_Collection_Upper_Depth, Field_Collection_Lower_Depth,Base_Parameter_Name, Statistical_Basis, Reslt_Parameter_Name, Result_Value, Result_Unit, Sample_Size))

#add year column
env.dat <- env.dat %>% mutate(Year=year(Date))

#match sites to basin
translate <- read_csv("Location_translation.csv") #csv file created to convert water quality sites to corresponding Puget Sound basin
env.dat <- merge(env.dat, translate, by = 'Location_ID', all.x=TRUE) #merges dataframes

#Select parameters of interest
env.dat <- env.dat %>% 
  filter(., Statistical_Basis == "Median") %>%
  filter(., Field_Collection_Lower_Depth < 25) %>%
  filter(., Base_Parameter_Name %in% c("Dissolved Oxygen", "Temperature, water", "pH"))

#select basins of interest
env.dat <- env.dat %>%
  filter(., Location %in% c("Central Puget Sound", "Hood Canal", "Admiralty Inlet", "South Puget Sound"))

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

################################################################################
##METADATA CREATIONS
################################################################################

#create table with just sampling effort information
meta <- long.dat %>%
  select(-c(Species, Count)) %>%
  unique()

#graph of sampling effort - may use in supplementary
ggplot(meta, aes(x=Year, y=Site)) + 
  geom_point(alpha=0.25, size=5, shape=15) +
  theme_cowplot()

#graph of sampling effort, with season
ggplot(meta, aes(x=Year, y=Site, fill=Season)) + 
  geom_point(alpha=0.5, size=3, position="jitter", shape=22) +
  theme_cowplot()

#create datatable with sampling by season as well as with lumped seasonality
meta2 <- meta %>% 
  group_by(Year) %>%
  count(Season) %>%
  pivot_wider(names_from=Season, values_from=n) %>%
  mutate(low=sum(Winter,Spring),
         high=sum(Autumn, Summer))

#graph of sampling effort by results (even)
ggplot(meta2, aes(x=Year)) +
  geom_line(aes(y=high), color="red") +
  geom_line(aes(y=low), color="blue")

#Housekeeping
rm(meta, meta2)

################################################################################
##FIGURE 2: SITE TRENDS OVER TIME
################################################################################

#select data of interest for figure
data <- long.dat %>% 
  filter(Species %in% c("Black", "Brown", "Copper", "Puget_Sound", "Quillback", "Yellowtail"))

#specify manual aesthetic parameters
basin.shapes <- c("Admiralty Inlet" = 0, "Hood Canal" = 16, "Central Puget Sound" = 17, "South Puget Sound"=6)
basin.cols <- c("Admiralty Inlet" = "gray40", "Hood Canal" = "black", "Central Puget Sound" = "gray60", "South Puget Sound" = "gray20")
#species.cols <- c("Copper"="yellow4", "Black"="gray8", "Brown"="gray40", "Quillback"="gray60", "Yellowtail"="gray80")
species.cols <- c("Copper"="orange", "Black"="gray8", "Brown"="blue", "Quillback"="violetred", "Yellowtail"="skyblue")

#Figure 2: species observed by site over time
ggplot(data = data, aes(x=Date, y=Count, group = Species, color = Species)) + 
  geom_point(aes(shape=Location), size=2) + theme_cowplot() + 
  facet_wrap(~factor(Site, levels=c("Keystone", "Point Hudson", "Possession Point", "Edmonds",
                                    "Rockaway", "Saltwater", "Point Whitney", "Sund Rock", "Z's Reef"))) + 
  scale_shape_manual(values=basin.shapes) + scale_color_manual(values=species.cols) +
  theme(axis.title.x=element_blank())+
  scale_y_continuous(trans="log2")

#Housekeeping
rm(basin.shapes, basin.cols, species.cols, data)

################################################################################
##END OF CODE
################################################################################
