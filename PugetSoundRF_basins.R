## This script includes code used for analyzing basin differences for the 2009-2023 puget sound rockfish data
## Inputs:
## Outputs:

################################################################################
#OVERALL TREND IN ADULT ROCKFISH OBSERVATIONS
################################################################################

#calculate correlation
data<-sum.dat %>% 
  select(., -c(Count, Species)) %>%
  unique()

cor.test(data$Year, data$RF_Count, method="kendall")

#plot
overall <- ggplot(data = data, aes(x=Date, y=RF_Count)) + 
  geom_point() + geom_smooth(method=glm, se=TRUE) + theme_cowplot() + 
  ggtitle("Adult Rockfish Count over time") +
  scale_y_continuous(trans='log2')

################################################################################
##COUNT OVER TIME BY BASIN
################################################################################

North <- data %>% filter(Location == "North Puget Sound")
cor.North <- cor.test(North$Year, North$RF_Count, method="kendall")
print(cor.North) 

Central <- data %>% filter(Location == "Central Puget Sound")
cor.Central <- cor.test(Central$Year, Central$RF_Count, method="kendall")
print(cor.Central) 

South <- data %>% filter(Location == "South Puget Sound")
cor.South <- cor.test(South$Year, South$RF_Count, method="kendall")
print(cor.South) 

Hood <- data %>% filter(Location == "Hood Canal")
cor.Hood <- cor.test(Hood$Year, Hood$RF_Count, method="kendall")
print(cor.Hood) 

#plot
basin <- ggplot(data = data, aes(x=Date, y=RF_Count, color=Location, group=Location)) +
  geom_point() + geom_smooth(method=glm, se=FALSE) + theme_cowplot() +
  ggtitle("Adult Rockfish Count over time by basin") +
  scale_y_continuous(trans='log2')

#Filter data to include most frequently observed species
fig.dat <- filter(sum.dat, Species %in% c("Black", "Brown", "Copper", "Quillback", "Yellowtail", "Puget_Sound"))
#fig.dat <- filter(sum.dat, Species %in% c("Black", "Puget_Sound", "Yellowtail", "Copper"))#top 4 most observed

#Correlations
Hood <- fig.dat %>% filter(Location == "Hood Canal" & Species == "Yellowtail")
cor <- cor.test(Hood$Year, Hood$Count, method = "kendall")
print(cor)

North <- fig.dat %>% filter(Location == "North Puget Sound" & Species == "Black")
cor <- cor.test(North$Year, North$Count, method = "kendall")
print(cor)

South <- fig.dat %>% filter(Location == "South Puget Sound" & Species == "Black")
cor <- cor.test(South$Year, South$Count, method = "kendall")
print(cor)

#Graph
fig.dat <- fig.dat %>% filter(Location == "Hood Canal" & Species != "Puget_Sound")

HC <- ggplot(data = fig.dat, aes(x=Date, y=Count, color=Species)) + geom_point() + 
  geom_smooth(method=glm, se=FALSE) + theme_cowplot() + 
  ggtitle("Species Counts over time in Hood Canal") +
  scale_y_continuous(trans='log2') 

ggarrange(overall, basin, HC)

#Housekeeping
rm(fig.dat)
rm(South)
rm(Hood)
rm(North)
rm(Central)
rm(cor)
rm(cor.Hood)
rm(cor.Central)
rm(cor.South)
rm(cor.North)
rm(overall)
rm(basin)
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

North <- filter.basin("North Puget Sound")
Central <- filter.basin("Central Puget Sound")
South <- filter.basin("South Puget Sound")
Hood <- filter.basin("Hood Canal")

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

basin_1 <- arrange(filter.basin(c("North Puget Sound","Central Puget Sound")), Location)
basin_2 <- arrange(filter.basin(c("North Puget Sound","South Puget Sound")), Location)
basin_3 <- arrange(filter.basin(c("North Puget Sound","Hood Canal")), Location)
basin_4 <- arrange(filter.basin(c("Central Puget Sound","South Puget Sound")), Location)
basin_5 <- arrange(filter.basin(c("Central Puget Sound","Hood Canal")), Location)
basin_6 <- arrange(filter.basin(c("South Puget Sound","Hood Canal")), Location)

d1 <- disper.f(basin_1$Location, basin_1[, 9:19])
d2 <- disper.f(basin_2$Location, basin_2[, 9:19])
d3 <- disper.f(basin_3$Location, basin_3[, 9:19])
d4 <- disper.f(basin_4$Location, basin_4[, 9:19])
d5 <- disper.f(basin_5$Location, basin_5[, 9:19])
d6 <- disper.f(basin_6$Location, basin_6[, 9:19])

d1
d2
d3
d4
d5
d6

#HOUSEKEEPING
rm(d1)
rm(d2)
rm(d3)
rm(d4)
rm(d5)
rm(d6)

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

basin.cols <- c(
  "#FFA824",  #Central 
  "#87CEEB",  #Hood
  "#32CC99",  #North 
  "#DE85B1")  #South

#Create graph
graphics.off()
windows(10.5, 7, record=T)
pt.size <- 2

plot.ord <- function(data, North, Central, South, Hood){
  t2 <- ggplot(data, aes(x = NMDS1, y = NMDS2)) + my.theme + coord_fixed() + 
    scale_color_manual(values=basin.cols) + geom_point(aes(color=Location), size=pt.size) + legend.theme +
    ylab("NMDS Axis-2") + xlab("NMDS Axis-1") +
    stat_ellipse(data=North, aes(x=NMDS1, y=NMDS2), col=basin.cols[3]) +
    stat_ellipse(data=Central, aes(x=NMDS1, y=NMDS2), col=basin.cols[1]) +
    stat_ellipse(data=South, aes(x=NMDS1, y=NMDS2), col=basin.cols[4]) + 
    stat_ellipse(data=Hood, aes(x=NMDS1, y=NMDS2), col=basin.cols[2])  
  return(t2)
}

p2 <- plot.ord(data, North, Central, South, Hood)
print(p2)

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

################################################################################
##ENVIRONMENTAL PARAMETERS BY BASIN
################################################################################

#Plot environmental data

ggplot(data = env.dat, aes(x=Date, y=Result_Value, color = Location, alpha = 0.01)) + 
  geom_point() + theme_cowplot() + facet_wrap(~Base_Parameter_Name)

ggplot(data = env.dat, aes(x=Location, y=Result_Value)) + 
  geom_boxplot() + theme_cowplot() + facet_wrap(~Base_Parameter_Name) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

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
