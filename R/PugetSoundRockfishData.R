## This script performs data cleaning on the 2009-2022 puget sound rockfish data


## set up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#load libraries
library(tidyverse)
library(here)
library(vegan)


#check to see that here() leads to root directory or top-level folder
#should be Seattle_Aquarium_Hawaii_coral_monitoring
here()

#if incorrect, enter `set_here(path = "...")`
#check here() again to make sure you're in the right top-level folder

#import data
dat <- read_csv(here("data", "puget_sound_rockfish.csv"))

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
                                  
#wide to long format
long.dat <- dat %>%
  pivot_longer("Black":"PugetSound", names_to = "Species", values_to = "Count")


#plot of sum of all counts across sites by species
plot1 <- ggplot(long.dat, aes(x = Year, y = Count, fill = Species)) +
  geom_col() +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1600)) +
  ggtitle("Sum of Counts Across All Sites by Species")


ggsave(filename = here("output", "Sum Counts-All Sites By Species.png"), width = 10, height = 7)

#plot of sum of all species by sites
plot2 <- ggplot(long.dat, aes(x = Year, y = Count, fill = Site)) +
  geom_col() +
  theme_bw() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1600)) +
  ggtitle("Sum of Counts Across All Species By Site")


ggsave(filename = here("output", "Sum Counts-All Species By Site.png"), width = 10, height = 7)

#plot of counts by sites
plot3 <- ggplot(long.dat, aes(x = Year, y = Count, fill = Species)) +
  geom_col() +
  theme_bw() +
  facet_wrap(~Site, nrow = 3, labeller = label_wrap_gen(width = 10)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 800)) 

ggsave(filename = here("output", "Counts - By Sites.png"), width = 10, height = 7)

#plot of counts by species
plot4 <- ggplot(long.dat, aes(x = Year, y = Count, fill = Site)) +
  geom_col() +
  theme_bw() +
  facet_wrap(~Species, nrow = 3, labeller = label_wrap_gen(width = 10)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 700)) 

ggsave(filename = here("output", "Counts - By Species.png"), width = 10, height = 7)

#plot of counts by season
plot5 <- ggplot(long.dat, aes(x = Year, y = Count, fill = Species)) +
  geom_col() +
  theme_bw() +
  facet_wrap(~Season, nrow = 3, labeller = label_wrap_gen(width = 10)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1000)) 

ggsave(filename = here("output", "Counts - Season by Species.png"), width = 10, height = 7)

#plot of counts by season by site
plot6 <- ggplot(long.dat, aes(x = Year, y = Count, fill = Site)) +
  geom_col() +
  theme_bw() +
  facet_wrap(~Season, nrow = 3, labeller = label_wrap_gen(width = 10)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1000)) 

ggsave(filename = here("output", "Counts - Season By Site.png"), width = 10, height = 7)

#sum of all fish per site
long.dat %>%
  drop_na() %>%
  group_by(Site) %>%
  summarize(sum = sum(Count))

#effort, how many surveys had 0 counts?
zerocountsurveys <- dat[rowSums(dat[7:25])==0,]
#six surveys










