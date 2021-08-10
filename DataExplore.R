## Exploring abundance and presence data for other potential species
# Goal to select others to focus on

library(dplyr)
library(ggplot2)

setwd("/Users/evebeaury/Desktop/OneDrive - University of Massachusetts/Abundance modeling/Bethany datasets")
list.files()

alldata = read.csv("MergedDatasets_5July2021_EB.csv")
glimpse(alldata)

# Candidate species
codes = c("LYMI", "IMCY", "LYJA", "LOMA6", "PUMO", "PUMOL", "LISI", "LECU")
focal = alldata %>% filter(SpCode %in% codes)
focal$SpCode[focal$SpCode=="PUMO"] <- "PUMOL"
table(focal$SpCode)
table(focal$SpCode, focal$CoverType)
# kudzu has way fewer cover estimates, all the rest have similar numbers

## Which have climate relevance?
# LOMA6 - temp limited
# LISI - very diff top variables

lisi = focal %>% filter(SpCode=="LISI")
pct = lisi %>% filter(CoverType=="PercentCover" | CoverType=="AvgCoverClass") %>% mutate(Cover = as.numeric(Cover))
hist(pct$Cover)
quantile(pct$Cover)
pct %>% mutate(bins = cut(Cover, breaks = c(0,10,20,30,50,100))) %>%
  group_by(bins) %>% summarise(nplot = n())
# definitely fewer points.. but could be ok?

loma = focal %>% filter(SpCode=="LOMA6")
pct = loma %>% filter(CoverType=="PercentCover" | CoverType=="AvgCoverClass") %>% mutate(Cover = as.numeric(Cover))
hist(pct$Cover)
quantile(pct$Cover)
pct %>% mutate(bins = cut(Cover, breaks = c(0,10,20,30,50,100))) %>%
  group_by(bins) %>% summarise(nplot = n())
# also definitely fewer points.. but could be ok?

# maybe circle back
test = focal %>% filter(CoverType=="PercentCover" | CoverType=="AvgCoverClass") %>% 
  mutate(Cover = as.numeric(Cover), 
         bins = cut(Cover, breaks = c(0,10,20,30,50,100))) %>%
  group_by(SpCode, bins) %>% summarise(nplot = n())

# I think we definitely go with LISI, and maybe LECU
glimpse(lisi)
setwd("/Users/evebeaury/Desktop/OneDrive - University of Massachusetts/Abundance modeling/Ligustrum")
#write.csv(lisi, "LISI_presence.csv", row.names = F)
pct = lisi %>% filter(CoverType=="PercentCover" | CoverType=="AvgCoverClass") %>% mutate(Cover = as.numeric(Cover))
#write.csv(pct, "LISI_abundance.csv", row.names=F)
abund5 = pct %>% filter(Cover > 5)
range(abund5$Cover)
#write.csv(abund5, "LISI_abund5.csv", row.names=F)
abund10 = pct %>% filter(Cover > 10)
range(abund10$Cover)
#write.csv(abund10, "LISI_abund10.csv", row.names=F)
abund20 = pct %>% filter(Cover > 20)
range(abund20$Cover)
#write.csv(abund20, "LISI_abund20.csv", row.names=F)
abund30 = pct %>% filter(Cover > 30)
range(abund30$Cover)
#write.csv(abund30, "LISI_abund30.csv", row.names=F)
abund50 = pct %>% filter(Cover > 50)
range(abund50$Cover)
#write.csv(abund50, "LISI_abund50.csv", row.names=F)

# LECU
lecu = focal %>% filter(SpCode=="LECU")
pct = lecu %>% filter(CoverType=="PercentCover" | CoverType=="AvgCoverClass") %>% mutate(Cover = as.numeric(Cover))
setwd("/Users/evebeaury/Desktop/OneDrive - University of Massachusetts/Abundance modeling/Lespedeza")
#write.csv(lecu, "LECU_presence.csv", row.names = F)
pct = lecu %>% filter(CoverType=="PercentCover" | CoverType=="AvgCoverClass") %>% mutate(Cover = as.numeric(Cover))
#write.csv(pct, "LECU_abundance.csv", row.names=F)
abund5 = pct %>% filter(Cover > 5)
range(abund5$Cover)
#write.csv(abund5, "LECU_abund5.csv", row.names=F)
abund10 = pct %>% filter(Cover > 10)
range(abund10$Cover)
#write.csv(abund10, "LECU_abund10.csv", row.names=F)
abund20 = pct %>% filter(Cover > 20)
range(abund20$Cover)
#write.csv(abund20, "LECU_abund20.csv", row.names=F)
abund30 = pct %>% filter(Cover > 30)
range(abund30$Cover)
#write.csv(abund30, "LECU_abund30.csv", row.names=F)
abund50 = pct %>% filter(Cover > 50)
range(abund50$Cover)
#write.csv(abund50, "LECU_abund50.csv", row.names=F)

#
#
## Enviro explore for LISI
#
#
glimpse(lisi)
pct = lisi %>% filter(CoverType=="PercentCover" | CoverType=="AvgCoverClass") %>% mutate(Cover = as.numeric(Cover))

## Map
library(mapproj)
states <- map_data("state")
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = lisi %>% filter(!CoverType=="AbsenceOnly"), 
             aes(Long, Lat), pch=16, size=2.5, inherit.aes = FALSE, color="steelblue2") + 
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("All records")
# Def a southern skewed species - cool
# Some random points in west - are these legit?
lisi %>% filter(Long < -100) # CalFlora has it... Might be worth circling back to the scientific name they recorded

# just abundance records
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = pct, aes(Long, Lat), pch=16, size=2.5, 
             inherit.aes = FALSE, color="slateblue2") + 
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Abundance records")

# plot together
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = lisi, aes(Long, Lat), pch=16, size=2.5, 
             inherit.aes = FALSE, color="black") + 
  xlab("Longitude") + ylab("Latitude") +
  geom_point(data = pct, aes(Long, Lat), pch=16, size=2.5, inherit.aes = FALSE, 
             color="slateblue2") +
  ggtitle("Abundance (purple) and presence (black)")
# overlap in core of range - def missing points in a handful of places but overlap is pretty good

## Draw convex hull over range?
glimpse(lisi)
hull_all <- lisi %>% filter(!is.na(Long), !is.na(Lat)) %>%
  slice(chull(Long, Lat))
hull_pct = pct %>% filter(!is.na(Long), !is.na(Lat)) %>%
  slice(chull(Long, Lat))
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_polygon(data= hull_all, aes(Long, Lat), alpha=0.5, inherit.aes = FALSE, fill="darkgrey") + 
  geom_point(data = pct, aes(Long, Lat), pch=16, size=2.5, inherit.aes = FALSE, 
             color="slateblue2") +
  ggtitle("Abundance points within range of all records")  +xlab("Longitude") + ylab("Latitude")
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_polygon(data= hull_all, aes(Long, Lat), alpha=0.5, inherit.aes = FALSE, fill="darkgrey") + 
  geom_polygon(data= hull_pct, aes(Long, Lat), alpha=0.5, inherit.aes = FALSE,
               fill="slateblue2")+
  ggtitle("Geographic space of abundance vs. all points")

## Color by value of cover
pct = pct %>% mutate(CoverBins = cut(Cover, breaks = c(0,10,20,30,40,50,60,70,80,90,100))) %>%
  filter(!is.na(CoverBins))
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = pct %>% arrange(CoverBins), 
             aes(Long, Lat, color=CoverBins), pch=16, size=2.5, inherit.aes = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  theme(legend.position = "bottom")
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = pct %>% arrange(Cover), aes(Long, Lat, color=Cover), pch=16, size=2.5, inherit.aes = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  theme(legend.position = "bottom") +
  scale_colour_steps(n.breaks=5, low="blue", high="red") +
  ggtitle("Cover values")

# as hulls
hull_lower <- pct %>% filter(!is.na(Long), !is.na(Lat), Cover < 10) %>%
  slice(chull(Long, Lat))
hull_mid <- pct %>% filter(!is.na(Long), !is.na(Lat), Cover > 10) %>%
  slice(chull(Long, Lat))

ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_polygon(data= hull_all, aes(Long, Lat), alpha=0.7, inherit.aes = FALSE, fill="lightgrey") +
  geom_polygon(data= hull_lower, aes(Long, Lat), alpha=0.8, inherit.aes = FALSE,
               fill="#238b45") + 
  geom_polygon(data= hull_mid, aes(Long, Lat), alpha=0.8, inherit.aes = FALSE,
               fill="#005824") +
  ggtitle("Stacked hulls, all points, <10%, >10%")
# pretty good overlap with the abundance points

hull_high <- pct %>% filter(!is.na(Long), !is.na(Lat), Cover >50) %>%
  slice(chull(Long, Lat))
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_polygon(data= hull_all, aes(Long, Lat), alpha=0.7, inherit.aes = FALSE, fill="lightgrey") +
  geom_polygon(data= hull_high, aes(Long, Lat), alpha=0.8, inherit.aes = FALSE,
               fill="#238b45") +
  ggtitle("All points vs. cover > 50%")

## Presence vs absence
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = lisi, aes(Long, Lat, color=PA), pch=16, size=2.5, inherit.aes = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  facet_wrap(~PA) + theme(legend.position = "none")

## Pull in environmental data!
library(raster)
library(maptools)

# Make a new cover dataset
dat = rbind(lisi %>% filter(CoverType == "PresenceOnly"), pct %>% dplyr::select(-CoverBins)) %>%
  mutate(Point = ifelse(CoverType=="PresenceOnly", "Presence", "Abundance")) %>%
  filter(!is.na(Long))
head(dat)

# Make data spatial
coordinates(dat) <- ~ Long + Lat
proj4string(dat) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Pull rasters
setwd("/Users/evebeaury/Desktop/OneDrive - University of Massachusetts/Abundance modeling/Microstegium/PARC_awc_mean_0_5_integer")
out.wd = "/Users/evebeaury/Desktop/OneDrive - University of Massachusetts/Abundance modeling/Microstegium/PARC_awc_mean_0_5_integer"

list.files()
# pull a few
files = c("Americas_N_LCM_Cat100.tif", "awc_mean_0_5_integer.tif", "ETa_Apr_Oct_2003_2017_monthlyMean_integer.tif",
          "VCF_percent_treeCover_2000_2016_mean_integer.tif", "PPT_Oct_Sept_1981_2017.tif")
#files = list.files()[c(1:3,5:7)]
for(f in files){
  assign(paste0(f), raster(paste0(out.wd, "/", f)))
  a <- as.data.frame(extract(get(f), dat))
  colnames(a) <- f
  dat <- spCbind(dat, a)
}

head(dat)
pctj = as.data.frame(dat) %>% rename(humanfoot = Americas_N_LCM_Cat100.tif, watercontent = awc_mean_0_5_integer.tif,
                                     eta = ETa_Apr_Oct_2003_2017_monthlyMean_integer.tif, 
                                     treecover = VCF_percent_treeCover_2000_2016_mean_integer.tif,
                                     ppt = PPT_Oct_Sept_1981_2017.tif)
glimpse(pctj)
hist(pctj$humanfoot)
hist(pctj$eta)
hist(pctj$watercontent)
hist(pctj$ppt)
# cool!

## How do the distributions of enviro variables overlap for presence vs. abundance points?
pctj = pctj %>% mutate(Point = factor(Point, levels = c("Presence", "Abundance")))
pctj %>% ggplot(aes(humanfoot, fill=Point)) + geom_histogram(alpha=0.5) # wow!
pctj %>% ggplot(aes(watercontent, fill=Point)) + geom_histogram(alpha=0.5)
pctj %>% ggplot(aes(eta, fill=Point)) + geom_histogram(alpha=0.5)
pctj %>% ggplot(aes(treecover, fill=Point)) + geom_histogram(alpha=0.5)
pctj %>% ggplot(aes(ppt, fill=Point)) + geom_histogram(alpha=0.5)

## What if we set some abundance thresholds?
all = pctj %>% mutate(Value = "AllData")
allabun = pctj %>% filter(Point=="Abundance") %>% mutate(Value="AllAbund")
abun10 = pctj %>% filter(Cover > 10) %>% mutate(Value="Abund10")
abun20 = pctj %>% filter(Cover > 20) %>% mutate(Value="Abund20")
newpct = rbind(all, allabun, abun10, abun10, abun20)

newpct = newpct %>% mutate(Value = factor(Value, levels = c("AllData", "AllAbund", "Abund10", "Abund20")))
newpct %>% ggplot(aes(humanfoot, fill=Value)) + geom_histogram(alpha=0.7) +
  ggtitle("Data set at 10% and 20% thresholds")
newpct %>% ggplot(aes(watercontent, fill=Value)) + geom_histogram(alpha=0.7) +
  ggtitle("Data set at 10% and 20% thresholds")
newpct %>% ggplot(aes(eta, fill=Value)) + geom_histogram(alpha=0.7) +
  ggtitle("Data set at 10% and 20% thresholds")
newpct %>% ggplot(aes(treecover, fill=Value)) + geom_histogram(alpha=0.7) +
  ggtitle("Data set at 10% and 20% thresholds")
newpct %>% ggplot(aes(ppt, fill=Value)) + geom_histogram(alpha=0.7) +
  ggtitle("Data set at 10% and 20% thresholds")

## How do predictors relate to continuous cover?
pctj %>% filter(Point=="Abundance") %>% ggplot(aes(humanfoot, as.numeric(Cover))) + geom_point() + geom_smooth()
pctj %>% filter(Point=="Abundance") %>% ggplot(aes(eta, as.numeric(Cover))) + geom_point() + geom_smooth() # weird 0s?
pctj %>% filter(Point=="Abundance") %>% ggplot(aes(watercontent, as.numeric(Cover))) + geom_point() + geom_smooth() # tail on the high end of values
pctj %>% filter(Point=="Abundance") %>% ggplot(aes(treecover, as.numeric(Cover))) + geom_point() + geom_smooth() # tail on the high end of values
pctj %>% filter(Point=="Abundance") %>% ggplot(aes(ppt, as.numeric(Cover))) + geom_point() + geom_smooth() # tail on the high end of values
# terrible...

#
#
#
## Now for LECU
#
#
#
glimpse(lecu)
pct = lecu %>% filter(CoverType=="PercentCover" | CoverType=="AvgCoverClass") %>% mutate(Cover = as.numeric(Cover))

## Map
library(mapproj)
states <- map_data("state")
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = lecu %>% filter(!CoverType=="AbsenceOnly"), 
             aes(Long, Lat), pch=16, size=2.5, inherit.aes = FALSE, color="steelblue2") + 
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("All records")
# Similar to microstegium, more points in midwest?

# just abundance records
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = pct, aes(Long, Lat), pch=16, size=2.5, 
             inherit.aes = FALSE, color="slateblue2") + 
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Abundance records")
# definitely looks like the distribution shrinks

# plot together
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = lecu, aes(Long, Lat), pch=16, size=2.5, 
             inherit.aes = FALSE, color="black") + 
  xlab("Longitude") + ylab("Latitude") +
  geom_point(data = pct, aes(Long, Lat), pch=16, size=2.5, inherit.aes = FALSE, 
             color="slateblue2") +
  ggtitle("Abundance (purple) and presence (black)")
# oh never mind - really good overlap here

## Draw convex hull over range?
glimpse(lecu)
hull_all <- lecu %>% filter(!is.na(Long), !is.na(Lat)) %>%
  slice(chull(Long, Lat))
hull_pct = pct %>% filter(!is.na(Long), !is.na(Lat)) %>%
  slice(chull(Long, Lat))
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_polygon(data= hull_all, aes(Long, Lat), alpha=0.5, inherit.aes = FALSE, fill="darkgrey") + 
  geom_point(data = pct, aes(Long, Lat), pch=16, size=2.5, inherit.aes = FALSE, 
             color="slateblue2") +
  ggtitle("Abundance points within range of all records")  +xlab("Longitude") + ylab("Latitude")
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_polygon(data= hull_all, aes(Long, Lat), alpha=0.5, inherit.aes = FALSE, fill="darkgrey") + 
  geom_polygon(data= hull_pct, aes(Long, Lat), alpha=0.5, inherit.aes = FALSE,
               fill="slateblue2")+
  ggtitle("Geographic space of abundance vs. all points")

## Color by value of cover
pct = pct %>% mutate(CoverBins = cut(Cover, breaks = c(0,10,20,30,40,50,60,70,80,90,100))) %>%
  filter(!is.na(CoverBins))
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = pct %>% arrange(CoverBins), 
             aes(Long, Lat, color=CoverBins), pch=16, size=2.5, inherit.aes = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  theme(legend.position = "bottom")
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = pct %>% arrange(Cover), aes(Long, Lat, color=Cover), pch=16, size=2.5, inherit.aes = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  theme(legend.position = "bottom") +
  scale_colour_steps(n.breaks=5, low="blue", high="red") +
  ggtitle("Cover values")

# as hulls
hull_lower <- pct %>% filter(!is.na(Long), !is.na(Lat), Cover < 10) %>%
  slice(chull(Long, Lat))
hull_mid <- pct %>% filter(!is.na(Long), !is.na(Lat), Cover > 10) %>%
  slice(chull(Long, Lat))

ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_polygon(data= hull_all, aes(Long, Lat), alpha=0.7, inherit.aes = FALSE, fill="lightgrey") +
  geom_polygon(data= hull_lower, aes(Long, Lat), alpha=0.8, inherit.aes = FALSE,
               fill="#238b45") + 
  geom_polygon(data= hull_mid, aes(Long, Lat), alpha=0.8, inherit.aes = FALSE,
               fill="#005824") +
  ggtitle("Stacked hulls, all points, <10%, >10%")
# some non-overlap of the higher abundance points, missing that coastal range

hull_high <- pct %>% filter(!is.na(Long), !is.na(Lat), Cover >50) %>%
  slice(chull(Long, Lat))
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_polygon(data= hull_all, aes(Long, Lat), alpha=0.7, inherit.aes = FALSE, fill="lightgrey") +
  geom_polygon(data= hull_high, aes(Long, Lat), alpha=0.8, inherit.aes = FALSE,
               fill="#238b45") +
  ggtitle("All points vs. cover > 50%")

## Presence vs absence
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = lecu, aes(Long, Lat, color=PA), pch=16, size=2.5, inherit.aes = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  facet_wrap(~PA) + theme(legend.position = "none")

## Pull in environmental data!
library(raster)
library(maptools)

# Make a new cover dataset
dat = rbind(lecu %>% filter(CoverType == "PresenceOnly"), pct %>% dplyr::select(-CoverBins)) %>%
  mutate(Point = ifelse(CoverType=="PresenceOnly", "Presence", "Abundance")) %>%
  filter(!is.na(Long))
head(dat)

# Make data spatial
coordinates(dat) <- ~ Long + Lat
proj4string(dat) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Pull rasters
setwd("/Users/evebeaury/Desktop/OneDrive - University of Massachusetts/Abundance modeling/Microstegium/PARC_awc_mean_0_5_integer")
out.wd = "/Users/evebeaury/Desktop/OneDrive - University of Massachusetts/Abundance modeling/Microstegium/PARC_awc_mean_0_5_integer"

list.files()
# pull a few
files = c("Americas_N_LCM_Cat100.tif", "awc_mean_0_5_integer.tif", "ETa_Apr_Oct_2003_2017_monthlyMean_integer.tif",
          "VCF_percent_treeCover_2000_2016_mean_integer.tif", "PPT_Oct_Sept_1981_2017.tif")
#files = list.files()[c(1:3,5:7)]
for(f in files){
  assign(paste0(f), raster(paste0(out.wd, "/", f)))
  a <- as.data.frame(extract(get(f), dat))
  colnames(a) <- f
  dat <- spCbind(dat, a)
}

head(dat)
pctj = as.data.frame(dat) %>% rename(humanfoot = Americas_N_LCM_Cat100.tif, watercontent = awc_mean_0_5_integer.tif,
                                     eta = ETa_Apr_Oct_2003_2017_monthlyMean_integer.tif, 
                                     treecover = VCF_percent_treeCover_2000_2016_mean_integer.tif,
                                     ppt = PPT_Oct_Sept_1981_2017.tif)
glimpse(pctj)
hist(pctj$humanfoot)
hist(pctj$eta)
hist(pctj$watercontent)
hist(pctj$ppt)
# cool!

## How do the distributions of enviro variables overlap for presence vs. abundance points?
pctj = pctj %>% mutate(Point = factor(Point, levels = c("Presence", "Abundance")))
pctj %>% ggplot(aes(humanfoot, fill=Point)) + geom_histogram(alpha=0.5) # wow!
pctj %>% ggplot(aes(watercontent, fill=Point)) + geom_histogram(alpha=0.5)
pctj %>% ggplot(aes(eta, fill=Point)) + geom_histogram(alpha=0.5)
pctj %>% ggplot(aes(treecover, fill=Point)) + geom_histogram(alpha=0.5)
pctj %>% ggplot(aes(ppt, fill=Point)) + geom_histogram(alpha=0.5)

## What if we set some abundance thresholds?
all = pctj %>% mutate(Value = "AllData")
allabun = pctj %>% filter(Point=="Abundance") %>% mutate(Value="AllAbund")
abun10 = pctj %>% filter(Cover > 10) %>% mutate(Value="Abund10")
abun20 = pctj %>% filter(Cover > 20) %>% mutate(Value="Abund20")
newpct = rbind(all, allabun, abun10, abun10, abun20)

newpct = newpct %>% mutate(Value = factor(Value, levels = c("AllData", "AllAbund", "Abund10", "Abund20")))
newpct %>% ggplot(aes(humanfoot, fill=Value)) + geom_histogram(alpha=0.7) +
  ggtitle("Data set at 10% and 20% thresholds")
newpct %>% ggplot(aes(watercontent, fill=Value)) + geom_histogram(alpha=0.7) +
  ggtitle("Data set at 10% and 20% thresholds")
newpct %>% ggplot(aes(eta, fill=Value)) + geom_histogram(alpha=0.7) +
  ggtitle("Data set at 10% and 20% thresholds")
newpct %>% ggplot(aes(treecover, fill=Value)) + geom_histogram(alpha=0.7) +
  ggtitle("Data set at 10% and 20% thresholds")
newpct %>% ggplot(aes(ppt, fill=Value)) + geom_histogram(alpha=0.7) +
  ggtitle("Data set at 10% and 20% thresholds")

## How do predictors relate to continuous cover?
pctj %>% filter(Point=="Abundance") %>% ggplot(aes(humanfoot, as.numeric(Cover))) + geom_point() + geom_smooth()
pctj %>% filter(Point=="Abundance") %>% ggplot(aes(eta, as.numeric(Cover))) + geom_point() + geom_smooth() # weird 0s?
pctj %>% filter(Point=="Abundance") %>% ggplot(aes(watercontent, as.numeric(Cover))) + geom_point() + geom_smooth() # tail on the high end of values
pctj %>% filter(Point=="Abundance") %>% ggplot(aes(treecover, as.numeric(Cover))) + geom_point() + geom_smooth() # tail on the high end of values
pctj %>% filter(Point=="Abundance") %>% ggplot(aes(ppt, as.numeric(Cover))) + geom_point() + geom_smooth() # tail on the high end of values
# so terrible lol



