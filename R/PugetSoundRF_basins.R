################################################################################
##PUGET SOUND ROCKFISH BASIN VARIATION
################################################################################

## This script includes code used for analyzing basin differences for the 2009-2023 puget sound rockfish data
## Use with outputs from "PugetSoundRF_datacleaning.R" file

################################################################################
#OVERALL TREND IN ADULT ROCKFISH OBSERVATIONS
################################################################################

#species by basin

#set manual aesthetics
basin.shapes <- c("Admiralty Inlet" = 0, "Hood Canal" = 16, "Central Puget Sound" = 17, "South Puget Sound" = 6)
basin.cols<- c("Admiralty Inlet" = "gray40", "Hood Canal" = "black", "Central Puget Sound" = "gray60", "South Puget Sound" = "gray20")

#remove YOY for graphing data
data <- long.dat %>%
  filter(Species != "YOY")

#create community structure graph for Figure #
comm <- ggplot(data, aes(x=Species, y=Count, shape=Location, color=Location)) + geom_point(alpha=0.75, size=2.5) + 
  facet_wrap(~Location, ncol=4) + theme_cowplot() + coord_flip() + 
  theme(axis.title.y=element_blank(), legend.position="none") +
  scale_shape_manual(values=basin.shapes) + scale_color_manual(values=basin.cols) +
  scale_y_continuous(trans='log2')

#calculate correlation
data<-sum.dat %>% 
  select(., -c(Count, Species)) %>%
  unique()

cor.test(data$Year, data$RF_Count, method="kendall")

#plot overall rockfish trend with stat results
overall <- ggplot(data = data, aes(x=Date, y=RF_Count)) + 
  geom_point(color="gray30", size=2, shape=1) + geom_smooth(method=glm, se=FALSE, aes(color="Overall")) + theme_cowplot() + 
  annotate("text", x=as.Date("2012-05-02"), y=256, label="Tau = -0.138") +
  annotate("text", x=as.Date("2012-05-02"), y=150, label="p = 0.016") +
  theme(axis.title.x=element_blank()) + ylab("Count (total)") +
  scale_color_manual(name="Legend", values="black") +
  scale_y_continuous(trans='log2')  

#plot ARF boxplots
ggplot() + 
  geom_boxplot(data=sum.dat, aes(x=Year, y=RF_Count, group=Year)) + 
  theme_cowplot() + 
  facet_wrap(~Location) + 
  scale_y_continuous(trans="log2") +
  ylab("Adult rockfish count") + theme(axis.title.x=element_blank())
#800x400

################################################################################
##COUNT OVER TIME BY BASIN
################################################################################

#run correlation test with each basin
#1. filter to create database with info from one basin
#2. run test
#3. view results from test

Admiralty <- data %>% filter(Location == "Admiralty Inlet")
cor.Admiralty <- cor.test(Admiralty$Year, Admiralty$RF_Count, method="kendall")
print(cor.Admiralty) 

Central <- data %>% filter(Location == "Central Puget Sound")
cor.Central <- cor.test(Central$Year, Central$RF_Count, method="kendall")
print(cor.Central) 

Hood <- data %>% filter(Location == "Hood Canal")
cor.Hood <- cor.test(Hood$Year, Hood$RF_Count, method="kendall")
print(cor.Hood) 

South <- data %>% filter(Location == "South Puget Sound")
cor.South <- cor.test(South$Year, South$RF_Count, method="kendall")
print(cor.South)

#plot 

#set aesthetics
basin.cols<- c("Admiralty Inlet" = "gray40", "Hood Canal" = "black", "Central Puget Sound" = "gray60", "South Puget Sound" = "gray20")
basin.shapes <- c("Admiralty Inlet" = 0, "Hood Canal" = 16, "Central Puget Sound" = 17, "South Puget Sound"=6)

#create basin trends over time for Figure # b.
basin <- ggplot(data = data, aes(x=Date, y=RF_Count, group=Location, shape=Location, color=Location)) +
  geom_point(size=2) + geom_smooth(method=glm, se=FALSE) + theme_cowplot() +
  annotate("text", x=as.Date("2013-10-09"), y=400, label="Tau=-0.238;", color="gray40") + 
  annotate("text", x=as.Date("2013-10-09"), y=225, label="p=0.053", color="gray40") +
  annotate("text", x=as.Date("2012-05-02"), y=60, label="Tau=-0.414;") +
  annotate("text", x=as.Date("2012-05-02"), y=35, label="p<0.001") +
  annotate("text", x=as.Date("2010-05-19"), y=7, label = "Tau=0.019", color="gray60") +
  annotate("text", x=as.Date("2010-05-19"), y=4, label = "p=0.842", color = "gray60") +
  annotate("text", x=as.Date("2012-11-02"), y=5, label ="Tau=0.566", color="gray20") +
  annotate("text", x=as.Date("2012-11-02"), y=2.5, label = "p=0.058", color="gray20") +
  scale_color_manual(values=basin.cols) + scale_shape_manual(values=basin.shapes) +
  theme(axis.title.x=element_blank()) + ylab("Count (total)") +
  scale_y_continuous(trans='log2')

#Filter data to include most frequently observed species
fig.dat <- filter(sum.dat, Species %in% c("Black", "Brown", "Copper", "Quillback", "Yellowtail", "Puget_Sound"))
#fig.dat <- filter(sum.dat, Species %in% c("Black", "Puget_Sound", "Yellowtail", "Copper"))#top 4 most observed

#Correlations for species within basin
Hood <- fig.dat %>% filter(Location == "Hood Canal" & Species == "Yellowtail")
cor <- cor.test(Hood$Year, Hood$Count, method = "kendall")
print(cor)

Admiralty <- fig.dat %>% filter(Location == "Admiralty Inlet" & Species == "Yellowtail")
cor <- cor.test(Admiralty$Year, Admiralty$Count, method = "kendall")
print(cor)

Central <- fig.dat %>% filter(Location == "Central Puget Sound" & Species == "Yellowtail")
cor <- cor.test(Central$Year, Central$Count, method = "kendall")
print(cor)

South <- fig.dat %>% filter(Location == "South Puget Sound" & Species == "Copper")
cor <- cor.test(South$Year, South$Count, method = "kendall")
print(cor)

#Create database for Hood canal specifically
fig.dat <- fig.dat %>% filter(Location == "Hood Canal" & Species != "Puget_Sound")

#set plot aesthetics
#species.cols <- c("Copper"="yellow4", "Black"="gray8", "Brown"="gray40", "Quillback"="gray60", "Yellowtail"="gray80")
species.cols <- c("Copper"="orange", "Black"="gray8", "Brown"="blue", "Quillback"="violetred", "Yellowtail"="skyblue")
#species.shapes <- c("Copper"=15, "Black"=16, "Brown"=17, "Quillback"=18, "Yellowtail"=19)

#create figure # c.
HC <- ggplot(data = fig.dat, aes(x=Date, y=Count, color=Species)) + geom_point(shape=16, size=2) + 
  geom_smooth(method=glm, se=FALSE, size=0.5) + theme_cowplot() + 
  annotate("text", x=as.Date("2020-11-14"), y=105, label="Copper:", color="orange")+
  annotate("text", x=as.Date("2020-11-14"), y=60, label="Tau=-0.461;", color="orange") +
  annotate("text", x=as.Date("2020-11-14"), y=35, label="p<0.001", color="orange") +
  scale_color_manual(values=species.cols) +
  theme(axis.title.x=element_blank()) + ylab("Count (by species)") +
  scale_x_date(limits=c(as.Date("2009-12-23"), as.Date("2023-02-28"))) +
  scale_y_continuous(trans='log2') 

#assemble component parts into Figure #
ggarrange(overall, basin, HC, ncol=1, label.x=0.1, align="v", labels = c("A", "B", "C"))
#800wx700h

#Correlation with envmtl data

#load environmental data for DO and Hood Canal
env <- env.dat %>% 
  filter(Base_Parameter_Name=="Dissolved Oxygen", Location=="Hood Canal") %>%
  group_by(Year) %>%
  mutate(Avg_DO = mean(Result_Value)) %>%
  select(Year, Avg_DO) %>%
  unique()

#load fish data for Coppers in Hood Canal
fish <- long.dat %>%
  filter(Location=="Hood Canal", Species=="Copper") %>%
  group_by(Year) %>%
  mutate(Avg_Count = mean(Count)) %>%
  select(Year, Avg_Count)%>%
  unique()

#combine fish and environmental data
test <- merge(env, fish, by="Year")

#run test
cor <- cor.test(test$Avg_Count, test$Avg_DO, method="kendall")
print(cor)

#Housekeeping
rm(fig.dat)
rm(Hood)
rm(Admiralty)
rm(Central)
rm(cor)
rm(cor.Hood)
rm(cor.Central)
rm(cor.Admiralty)
rm(overall)
rm(basin)
rm(test, env, fish)

################################################################################
## NMDS 
################################################################################

## remove YOY
data <- dat %>% 
  subset(., select=-c(YOY))

## remove sampling days with 0 observations
data <- data[rowSums(data[c(7:24)])>0,]

## create separate metadata and spp dataframes 
ncol_metadata <- 6
info <- data[,1:ncol_metadata]
spp <- data[,-(1:ncol_metadata)]

##Remove any species with 1 or fewer observations
spp <- spp %>% select_if(negate(function(col) is.numeric(col) && sum(col) <= 1))

## perform log_transform, if desired
log_transform <- function(x){
  out <- log10(x+1)
  return(out)
}

## run, if desired
spp <- log_transform(spp)

## perform multivariate analysis 
ord <- metaMDS(comm = spp, distance="bray", k=2, min = 1000, trymax=2000, 
               autotransform = F, wascores = TRUE)

## save a new ordination 
setwd(output)
save(ord, file = "ord.rda")

## work with ordination: stress, NMDS coords 
setwd(output)
load("ord.rda")

## visualize stress, check ordination, xy coordinates 
## open graphics window
graphics.off()
windows(6,6,record=T)

## plot
plot(ord)
stressplot(ord)

## overlay correlation with log species  
dist <- ord$dist
ord.points <- postMDS(ord$points, dist)
spp_scores <- as.data.frame(wascores(ord.points, spp))     
names(spp_scores)[1] <- "spp_x"
names(spp_scores)[2] <- "spp_y"
write.csv(spp_scores, "spp_scores_log.csv")

## NMDS ordination coordinates saved as data frame
save.coords <- function(ord, info, spp){
  t1 <- as.data.frame(scores(ord)[[1]])
  t2 <- cbind(t1, info, spp)
  return(t2)
}

## bind nmds coordinates to dataframe with site info and spp counts
NMDS_coords <- save.coords(ord, info, spp)

## save final output as CSV files for further analysis / visualization  ~~~~~~~~
setwd(output)
write.csv(NMDS_coords,'NMDS_coords_basin.csv')

#Housekeeping:
rm(info)
rm(NMDS_coords)
rm(ord)
rm(ord.points)
rm(spp_scores)
rm(dist)
rm(ncol_metadata)
rm(save.coords)

################################################################################
## NMDS ANALYSES AND VISUALIZATIONS
################################################################################

#load data
data <- read.csv("NMDS_coords_basin.csv")
data <- data[,-1]

## filter by season to calculate season ellipses 
filter.basin <- function(x){filter(data, Location %in% c(x))}

Admiralty <- filter.basin("Admiralty Inlet")
Central <- filter.basin("Central Puget Sound")
Hood <- filter.basin("Hood Canal")
South <- filter.basin("South Puget Sound")

## analysis of dispersion
## function to calculate homogeneity of dispersion 
disper.f <- function(grouping, spp){
  group_location <- grouping
  dist <- vegdist(spp, method="bray")
  disp <- betadisper(dist, group=grouping, type="centroid")
  t1 <- anova(disp)
  return(t1)
}

## calculate homogeneity of dispersion

basin_1 <- arrange(filter.basin(c("Admiralty Inlet","Central Puget Sound")), Location)
basin_2 <- arrange(filter.basin(c("Admiralty Inlet","Hood Canal")), Location)
basin_3 <- arrange(filter.basin(c("Central Puget Sound","Hood Canal")), Location)
basin_4 <- arrange(filter.basin(c("Admiralty Inlet", "South Puget Sound")), Location)
basin_5 <- arrange(filter.basin(c("Central Puget Sound", "South Puget Sound")), Location)
basin_6 <- arrange(filter.basin(c("Hood Canal", "South Puget Sound")), Location)

d1 <- disper.f(basin_1$Location, basin_1[, 9:19])
d2 <- disper.f(basin_2$Location, basin_2[, 9:19])
d3 <- disper.f(basin_3$Location, basin_3[, 9:19])
d4 <- disper.f(basin_4$Location, basin_4[, 9:19])
d5 <- disper.f(basin_5$Location, basin_5[, 9:19])
d6 <- disper.f(basin_6$Location, basin_6[, 9:19])

d1 #AI vs. CPS: p=0.75
d2 #AI vs. HC: p<0.01
d3 #CPS vs. HC: p<0.01
d4 #AI vs. SPS: p<0.001
d5 #CPS vs. SPS: p<0.001
d6 #HC vs. SPS: p<0.001

#HOUSEKEEPING
rm(d1,d2,d3,d4,d5,d6)

## data conversion / filtering
## define different cols for convert.factor function 
col.all <- c("Date", "Site", "Location", "Season", "Month", "Year")

## function to convert columns to factors
convert.factor <- function(data, col_X){
  data[col_X] <- lapply(data[col_X], as.factor)
  return(data)
}

## make the conversion 
data <- convert.factor(data, col.all)

## create dataframe with spp only 
data_spp <- data[, -c(1:8)]  

#Graphing parameters
my.theme = theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(), 
                 axis.line = element_line(colour = "black"),
                 plot.title = element_text(size=14), 
                 axis.text=element_text(size=12),
                 axis.title=element_text(size=14))

legend.theme = theme(legend.title = element_text(size=14), 
                     legend.text = element_text(size=14))

no.legend = theme(legend.position = "none")

basin.cols<- c("Admiralty Inlet" = "gray40", "Central Puget Sound" = "gray60", "Hood Canal" = "black", "South Puget Sound"="gray20")
basin.shapes <- c("Admiralty Inlet" = 0, "Hood Canal" = 16, "Central Puget Sound" = 17, "South Puget Sound"=6)
#basin.cols <- c(
# "gray30",  #Admiralty
#"gray60",  #Central
#"black")  #Hood 


#Create graph
graphics.off()
windows(10.5, 7, record=T)
pt.size <- 2

plot.ord <- function(data, Admiralty, Central, Hood, South){
  t2 <- ggplot(data, aes(x = NMDS1, y = NMDS2)) + my.theme + coord_fixed() + 
    scale_color_manual(values=basin.cols) + scale_shape_manual(values=basin.shapes) +
    geom_point(aes(color=Location, shape=Location), size=pt.size) + legend.theme +
    ylab("NMDS Axis-2") + xlab("NMDS Axis-1") + theme(legend.position="none") +
    stat_ellipse(data=Admiralty, aes(x=NMDS1, y=NMDS2), col=basin.cols[1]) +
    stat_ellipse(data=Central, aes(x=NMDS1, y=NMDS2), col=basin.cols[2]) +
    stat_ellipse(data=Hood, aes(x=NMDS1, y=NMDS2), col=basin.cols[3]) +
    stat_ellipse(data=South, aes(x=NMDS1, y=NMDS2), col=basin.cols[4]) +
    annotate("text", x=1.5, y=0.75, label="Pr(>F)<0.001") +
    annotate("segment",x=1.45,xend=1.45,y=0.4,yend=0.65,linetype="dashed") +
    annotate("segment",x=0.82,xend=1.4,y=1,yend=0.82,linetype="dashed")
  return(t2)
}

p2 <- plot.ord(data, Admiralty, Central, Hood, South)
print(p2)

ggarrange(comm, p2, nrow=1, labels=c("A", "B"), widths=c(1.75,1), label.x=0.1)
#1175x450

#Statistical analysis: 
perm.all <- function(data, spp){
  t1 <- how(nperm=10000)
  setBlocks(t1) <- with(data, Location)
  permanova.1 <- adonis2(spp ~ Location, 
                         by="terms", data=data, methods="bray", perm=10000)
  return(permanova.1)
}

p1 <- perm.all(data, data_spp)
print(p1)

##Determine if Admiralty Inlet and Central Puget Sound (aka only 2 that don't have significantly different dispersions) have significantly different centroids

onlyNS <- data %>%
  filter(Location %in% c("Admiralty Inlet", "Central Puget Sound"))

which(data$Location=="Admiralty Inlet")
which(data$Location=="Central Puget Sound")

NS_spp <- spp[c(1:10, 12, 14:16, 19, 24:26, 30:32, 35, 36, 38, 39, 41:43, 49:57, 61:63,
            67, 68, 69:72, 76, 77, 81:86, 87, 88, 92, 94, 95, 97:99, 101, 103, 104,
            107:110, 112:114, 119, 120, 123:127, 131, 132, 135, 138, 139, 140:147),]

p2 <- perm.all(onlyNS, NS_spp)
print(p2)

#Housekeeping
rm(Admiralty, basin_1, basin_2, basin_3, Central, comm, data_spp, Hood, perm.all)
rm(legend.theme, my.theme, no.legend, p1, p2, spp, code, col.all, fig, input)
rm(output, pt.size, convert.factor, disper.f, filter.basin, log_transform, plot.ord)
rm(onlyNS, NS_spp)


################################################################################
##ENVIRONMENTAL PARAMETERS BY BASIN
################################################################################

#Plot environmental data

#ggplot(data = env.dat, aes(x=Date, y=Result_Value, color = Location, alpha = 0.01)) + 
 # geom_point() + theme_cowplot() + facet_wrap(~Base_Parameter_Name)

#Create names for figure
names <- list("Dissolved Oxygen"="DO (mg/L)", "pH"="pH", "Temperature, water" = "Temp deg C")
name_labeller <- function(variable, value) {
  return(names[value])
}

#Specify significant comparisons
my_comparisons <- list(c("Admiralty Inlet", "Central Puget Sound"), 
                       c("Admiralty Inlet", "Hood Canal"),
                       c("Central Puget Sound", "Hood Canal"),
                       c("Admiralty Inlet", "South Puget Sound"),
                       c("Central Puget Sound", "South Puget Sound"),
                       c("Hood Canal", "South Puget Sound"))

#Create figure #
ggplot(data = env.dat, aes(x=Location, y=Result_Value)) + 
  geom_boxplot() + theme_cowplot() + 
  facet_wrap(~Base_Parameter_Name, scales="free", labeller=name_labeller) +
  theme(axis.text.x=element_text(angle=45, hjust=1), axis.title=element_blank()) +
  stat_compare_means(comparisons=my_comparisons)

#statistical test: Kruskal Wallace (non-parametric equivalent of one-way Anova)
data <- env.dat %>% filter(Base_Parameter_Name == "Temperature, water")
kruskal.test(Result_Value ~ Location, data = data)

#diagnostics

#Dunn's Kruskal-Wallis post-hoc test for #Season OR #Location
posthocs1<-dunnTest(Result_Value ~ Location, data=data, method="holm")
print(posthocs1)

#Dwass, Steel, Critchlow, Fligner post-hoc test
posthocs2<-dscfAllPairsTest(as.factor(Result_Value) ~ as.factor(Location), data=data)
print(posthocs2)

#Housekeeping
rm(my_comparisons, names, posthocs1, posthocs2, value.labs, name_labeller)

################################################################################
##END OF CODE
################################################################################
