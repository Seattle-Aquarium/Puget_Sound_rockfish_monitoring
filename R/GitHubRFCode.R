## This script includes code used for data cleaning, analysis, and graphing on the 2009-2023 puget sound rockfish data


## set up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load libraries
library(tidyverse)
library(vegan)
library(ggpubr)#graphing
library(cowplot)#graphing
library(FSA)#kruskal-wallace diagnostics
library(PMCMRplus)#kruskal-wallace diagnostics
library(car)

#Upload raw data:
dat <- read_csv("data_input/RF_100m.csv")

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
dat <- dat[c(1,3:5,26,25,2,27,6:24)]

#wide to long format
long.dat <- dat %>%
  pivot_longer("Black":"Puget_Sound", names_to = "Species", values_to = "Count")

#Select data of interest:
long.dat <- long.dat %>% 
  filter(., Direction == "F", Transect == "T1", Site != "Alki Fishing Reef") %>%
  select(., -c(Direction, Transect))

#Make summary statistics~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sum.dat <- filter(long.dat, Species != "YOY") #removeYOY

sum.dat <- sum.dat %>% 
  group_by(Site, Date) %>%
  mutate(Shannon = diversity(Count, "shannon"),
         Richness = sum(Count > 0))

sum.dat <- sum.dat %>% 
  subset(., Species != "Cabezon" & Species != "Lingcod" & Species != "Greenling" & Species != "Painted Greenling" & Species != "Wolfeel") %>%
  group_by(Site, Date) %>%
  mutate(RF_Count = sum(Count)) #add adult RF count column

#Create a table describing where/when YOY were observed~~~~~~~~~~~~~~~~~~~~~~~~
YOY <- long.dat %>% subset(., Species == "YOY" & Count > 0)
YOY <- YOY %>% 
  select(., c(Location, Site, Date, Season, Count)) %>%
  print()

#Statistical analysis~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Rockfish Count, dependent on basin location and season:
sum.dat <- sum.dat %>%
  select(., -c(Species, Count)) %>%
  unique()

#statistical test: Kruskal Wallace (non-parametric equivalent of one-way Anova)
kruskal.test(RF_Count ~ Season, data = sum.dat)
kruskal.test(RF_Count ~ Location, data = sum.dat)

#diagnostics

#Dunn's Kruskal-Wallis post-hoc test for #Season OR #Location
posthocs1<-dunnTest(RF_Count ~ Season, data=sum.dat, method="holm")
print(posthocs1)

#Dwass, Steel, Critchlow, Fligner post-hoc test
posthocs2<-dscfAllPairsTest(as.factor(RF_Count) ~ as.factor(Season), data=sum.dat)
print(posthocs2)

#Shannon Diversity, dependent on Season and Basin:
  
#Data prep:
dat <- sum.dat %>% 
  select(.,c(Location, Site, Date, Season, Shannon)) %>%
  unique()

#2-way ANOVA set up to test independence btwn season & site, based on diversity
res.aov <- aov(Shannon ~ Location * Season, data = dat)
summary(res.aov)

TukeyHSD(res.aov, which = "Season")
TukeyHSD(res.aov, which = "Location")

#Diagnostics:

#test for assumptions
plot(res.aov3, 1)
plot(res.aov3, 2)

# Extract the residuals
aov_residuals <- residuals(object = res.aov3)

# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

# Run Levene Test
leveneTest(Shannon ~ Site*Season, data = dat)

#Environmental analyses~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Puget Sound data from here: https://ecology.wa.gov/Research-Data/Monitoring-assessment/Puget-Sound-and-marine-monitoring

#load data
env.dat <- read_csv("data_input/EIM_Env_Data.csv")

#clean data
env.dat$Date <- mdy_hms(env.dat$Field_Collection_Start_Date_Time)
env.dat$Date <- date(env.dat$Date)

env.dat <- env.dat %>% 
  select(., c(Location_ID, Date, Field_Collection_Upper_Depth, Field_Collection_Lower_Depth,Base_Parameter_Name, Statistical_Basis, Reslt_Parameter_Name, Result_Value, Result_Unit, Sample_Size))

#color points by basin
translate <- read_csv("data_input/Location_translation.csv")
translate <- translate[,1:4]
env.dat <- merge(env.dat, translate, by = 'Location_ID', all.x=TRUE)

#Select parameters of interest
env.dat <- env.dat %>% 
  filter(., Statistical_Basis == "Median") %>%
  filter(., Base_Parameter_Name %in% c("Dissolved Oxygen", "Temperature, water", "pH", "Conductivity", "Salinity", "Turbidity"))

env.dat <- env.dat %>%
  filter(., Location %in% c("Central Puget Sound", "Hood Canal", "South Puget Sound", "North Puget Sound"))


#Figure ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create graphics with trends in summary statistics over time:

sum.dat <- sum.dat %>% 
  pivot_longer(
    cols = c("Shannon", "Richness", "RF_Count"),
    names_to = "metric",
    values_to = "value"
  )

ggplot(data = sum.dat, 
       aes(x=Date, y=value, group=metric, color=metric)) +
  geom_point() + geom_smooth(method=glm, se=TRUE) +
  theme_cowplot() + facet_wrap(~Location) +
  scale_y_continuous(trans='log2')

#Figure ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Seasonal/Basin differences in RF_Count
ggboxplot(sum.dat, x = "Season", y = "RF_Count", 
          order = c("Winter", "Spring", "Summer", "Fall"),
          ylab = "Count", xlab = "Season")

ggline(sum.dat, x = "Season", y = "RF_Count", 
       add = c("mean_se", "jitter"), 
       order = c("Winter", "Spring", "Summer", "Fall"),
       ylab = "Count", xlab = "Season")

ggboxplot(sum.dat, x = "Location", y = "RF_Count", order = c("North Puget Sound", "Central Puget Sound", "South Puget Sound", "Hood Canal"), ylab = "RF Count", xlab = "Basin")

#Figure ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#visualize differences based on season
ggboxplot(dat, x="Season", y = "Shannon", color = "Season") + 
  facet_wrap(~Site) + theme(axis.text.x=element_blank()) #alt. group by Location
#OR
ggboxplot(dat, x="Season", y = "Shannon")

#visualize differences based on site
ggboxplot(dat, x="Location", y = "Shannon") + 
  theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust = 1))

#Figure ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Plot environmental data

ggplot(data = env.dat, aes(x=Date, y=Result_Value, color = Location, alpha = 0.1)) + 
  geom_point() + theme_cowplot() + facet_wrap(~Base_Parameter_Name)


