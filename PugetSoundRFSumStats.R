## This script performs summary statistics on the 2009-2023 puget sound rockfish data


## set up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#load libraries
library(tidyverse)
library(here)
library(vegan)
library(ggpubr)

#load and prep raw data

#import data
dat <- read_csv(here("data", "RF_100m.csv"))
dat <- dat[,1:24]

#select only T1 forward counts
dat <- filter(dat, Direction == "F", Transect == "T1")

str(dat)

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
dat <- dat[c(1,3,26,25,2,27,6:24)]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#2-way ANOVA set up to test independence btwn season & site, based on abundance

#create abundance databases for YOY and rockfish
YOYdat <- dat[,c(1:6,15)]
YOYdat <- YOYdat %>% 
  rename("Abundance" = "YOY")
ARFdat<-dat[,-c(10,14:17,21)]#remove non-adult non-rockfish
ARFdat <- ARFdat %>% mutate(Abundance = rowSums(.[,c(7:19)], na.rm = T))#add abundance col

#visualize differences
ggboxplot(ARFdat, x="Season", y = "Abundance") + 
  facet_wrap(~Site) #alt. group by Location
#OR

ggboxplot(ARFdat, x = "Site", y = "Abundance", color = "Season")

ggline(ARFdat, x = "Site", y = "Abundance", color = "Season",
       add = c("mean_se", "dotplot"))

#test for assumptions
plot(res.aov3, 1)
plot(res.aov3, 2)
# Extract the residuals
aov_residuals <- residuals(object = res.aov3)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )
library(car)
leveneTest(Abundance ~ Site*Season, data = ARFdat)

#run stats
res.aov2 <- aov(Abundance ~ Site + Season, data = ARFdat)
summary(res.aov2)

res.aov3 <- aov(Abundance ~ Site * Season, data = ARFdat)
summary(res.aov3)

sumstats <- group_by(ARFdat, Site, Season) %>%
  summarise(
    count = n(),
    mean = mean(Abundance, na.rm = TRUE),
    sd = sd(Abundance, na.rm = TRUE)
  )

TukeyHSD(res.aov3, which = "Season")
#there may be a difference between spring and fall (p < 0.05)

#didn't pass test for normalcy

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Kruskal Wallace (non-parametric equivalent of one-way Anova)

#sumstats
group_by(ARFdat, Season) %>%
  summarise(
    count = n(),
    mean = mean(Abundance, na.rm = TRUE),
    sd = sd(Abundance, na.rm = TRUE),
    median = median(Abundance, na.rm = TRUE),
    IQR = IQR(Abundance, na.rm = TRUE)
  )

group_by(ARFdat, Site) %>%
  summarise(
    count = n(),
    mean = mean(Abundance, na.rm = TRUE),
    sd = sd(Abundance, na.rm = TRUE),
    median = median(Abundance, na.rm = TRUE),
    IQR = IQR(Abundance, na.rm = TRUE)
  )

#visualize
ggboxplot(ARFdat, x = "Season", y = "Abundance", 
          order = c("Winter", "Spring", "Summer", "Fall"),
          ylab = "Abundance", xlab = "Season")

ggline(ARFdat, x = "Season", y = "Abundance", 
       add = c("mean_se", "jitter"), 
       order = c("Winter", "Spring", "Summer", "Fall"),
       ylab = "Abundance", xlab = "Season")

#kw test
kruskal.test(Abundance ~ Season, data = ARFdat)


