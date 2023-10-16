################################################################################
##PUGET SOUND ROCKFISH DATA SUPPLEMENTAL COMPARISON
################################################################################

## This script includes code used to run meta-analysis with our data and the data used for Tolimieri et al. 2017 (https://doi.org/10.1002/ece3.2901) 

################################################################################
#SET UP
################################################################################

#Run "PugetSoundRF_datacleaning.R" script

#load additional library for renaming
library(DataCombine)

#load data
datTol <- read_csv("TolimieriDat.csv")

#prep Tolimieri data for analysis
data1 <- datTol %>%
  select(-c(X1955:X1997)) %>%
  filter(Survey.1=="REEF") %>%
  filter(MCA %in% c(9,10,11,12,13))

#rename year columns to remove "X"
names(data1) <- sub('^X', '', names(data1))

#make data tidy
data1 <- data1 %>%
  pivot_longer(7:23, names_to="Year", values_to="RF_Count")

#rename MCA values for merging
data1 <- as.data.frame(data1)

translate <- data.frame(
  code = c("12","13"), 
  name = c( "Hood Canal", "South Puget Sound"))

data1 <- FindReplace(data = data1, Var = "MCA", replaceData = translate, from = "code", to = "name", exact = TRUE, vector = FALSE)

#rename MCA title and Survey.1 for merging
data1 <- data1 %>%
  select(MCA, Survey.1, Year, RF_Count) %>%
  rename(Location=MCA,
         Method=Survey.1)

#prep my data for analysis
data2 <- sum.dat %>% 
  mutate(Method="Aquarium")%>%
  select(Location, Method, Year, RF_Count) %>%
  unique()

data2$Year <- as.character(data2$Year)

#merge datasets
data <- full_join(data1,data2)

#HOUSEKEEPING
rm(data1,data2,env.dat,translate,YOY)

#prelim plotting
data$Year <- as.numeric(data$Year)

#750x500
ggplot(data, aes(x=Year,y=RF_Count)) +
  geom_point(size=2, aes(shape=Method, alpha=Method))+
  geom_smooth(method="lm", se=FALSE, color="black")+
  theme_cowplot()+
  ylab("Adult rockfish count")+
  scale_shape_manual(values=c(17,6))+
  scale_alpha_manual(values=c(0.5,1))+
  scale_y_continuous(trans="log2")

subdata <- data %>%
  filter(Location %in% c("Hood Canal","South Puget Sound"))

#
ggplot(subdata, aes(x=Year,y=RF_Count)) +
  geom_point(size=2, aes(shape=Method, alpha=Method))+
  geom_smooth(method="lm", se=FALSE, color="black")+
  theme_cowplot()+
  ylab("Adult rockfish count")+
  scale_shape_manual(values=c(17,6))+
  scale_alpha_manual(values=c(0.5,1))+
  facet_wrap(~Location) +
  scale_y_continuous(trans="log2")

#Correlation testing
cor.test(data$Year, data$RF_Count, method="kendall") #no significant relationship as in no gains or losses

#Hood Only
Hood <- data %>%
  filter(Location=="Hood Canal")
cor.test(Hood$Year, Hood$RF_Count, method="kendall") #still very negative p=0.0002959, z=-3.61, tau=0.307

#South Only
South <- data %>%
  filter(Location=="South Puget Sound")
cor.test(South$Year, South$RF_Count, method="kendall") #significant gains, p=0.0365, tau=0.316, z=2.09