################################################################################
##PUGET SOUND ROCKFISH SEASONAL VARIATION
################################################################################

## This script includes code used for analysis of seasonal variation for the 2017-2023 puget sound rockfish data
#sites: Keystone, Point Hudson, Point Whitney, Rockaway, Sund Rock, Edmonds
#years: 2017+

################################################################################
##TWO-WAY ANOVA: SHANNON DIVERSITY INDEPENDENT OF SEASON AND BASIN?
################################################################################

#filter data for sites and years that have multiple seasons represented
data <- sum.dat %>% 
  filter(., Year >= 2017 & Site %in% c("Keystone", "Point Hudson", "Point Whitney", "Rockaway", "Sund Rock", "Edmonds"))

#Select parameters of interest
data <- data %>% 
  select(.,c(Location, Site, Date, Season, Shannon)) %>%
  unique()

#2-way ANOVA set up to test independence btwn season & site, based on diversity
res.aov <- aov(Shannon ~ Location * Season, data = data)
summary(res.aov)

TukeyHSD(res.aov, which = "Season")

#test for assumptions
plot(res.aov, 1)
plot(res.aov, 2)

# Extract the residuals
aov_residuals <- residuals(object = res.aov)

# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals ) 

# Run Levene Test
leveneTest(Shannon ~ Location*Season, data = data)

##RESULT: According to a 2-way ANOVA, Shannon diversity significantly differs by season (p<0.05). Spring and winter both differ significantly from fall.

#Housekeeping
rm(res.aov)
rm(aov_residuals)

################################################################################
##KRUSKAL-WALLACE TEST: ADULT ROCKFISH COUNT INDEPENDENT OF SEASON?
################################################################################

#filter data for sites and years that have multiple seasons represented
data <- sum.dat %>% 
  filter(., Year >= 2017 & Site %in% c("Keystone", "Point Hudson", "Point Whitney", "Rockaway", "Sund Rock", "Edmonds"))

#Select parameters of interest
data <- data %>% 
  select(.,c(Location, Site, Date, Season, RF_Count)) %>%
  unique()

#statistical test: Kruskal Wallace (non-parametric equivalent of one-way Anova)
kruskal.test(RF_Count ~ Season, data = data)

#diagnostics

#Dunn's Kruskal-Wallis post-hoc test for #Season OR #Location
posthocs1<-dunnTest(RF_Count ~ Season, data=data, method="holm")
print(posthocs1)

#Dwass, Steel, Critchlow, Fligner post-hoc test
posthocs2<-dscfAllPairsTest(as.factor(RF_Count) ~ as.factor(Season), data=data)
print(posthocs2)

#Results: According to Dunn's and DSCF post-hoc tests, Fall and Winter differ significantly, with marginally significant difference in means between fall and spring.

#Housekeeping
rm(posthocs1)
rm(posthocs2)

################################################################################
#VISUALIZING DIFFERENCE
################################################################################

#filter data for sites and years that have multiple seasons represented
data <- sum.dat %>% 
  filter(., Year >= 2017 & Site %in% c("Keystone", "Point Hudson", "Point Whitney", "Rockaway", "Sund Rock", "Edmonds"))

#Select parameters of interest
data <- data %>% 
  dplyr::select(.,c(Location, Site, Date, Season, RF_Count, Shannon)) %>%
  unique()

#Make individual plots
Shannon <- ggboxplot(data, x="Season", y = "Shannon")
RF_Count <- ggboxplot(data, x="Season", y="RF_Count") + scale_y_continuous(trans='log2')

#Plot together
ggarrange(Shannon, RF_Count)

################################################################################
#HOUSEKEEPING
################################################################################
rm(data)
rm(RF_Count)
rm(Shannon)


