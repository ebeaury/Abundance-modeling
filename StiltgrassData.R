## Exploring abundance and presence data for microstegium (Eve's focal species)

library(dplyr)
library(ggplot2)

setwd("C:/Users/ebeaury/OneDrive - University of Massachusetts/Abundance modeling/Microstegium")
setwd("~/Desktop/OneDrive - University of Massachusetts/Abundance modeling/Microstegium")

theme_set(theme_bw())
theme_update(text = element_text(size=15))

## Load stiltgrass data
mv = read.csv("MV_allpoints.csv", stringsAsFactors = FALSE)
glimpse(mv)

## How many records of different types
table(mv$PA)
unique(mv$Dataset)
# huh - some real absensces
table(mv$CoverType)
mv %>% filter(!CoverType %in% c("PresenceOnly", "AbsenceOnly")) %>% nrow()
# most records are presence only, but a decent anount of percent cover
# probs ignore avg stem count
# but could consider incorporating cover classes or qualitative measures?
mv %>% filter(CoverType=="AvgCoverClass") %>% distinct(Cover)
mv %>% filter(CoverType=="Qualitative") %>% distinct(Cover)
# need to look up how the qualitative measures are determined (and how other studies lumped them in?)

## Look at continuous vals
pct = mv %>% filter(CoverType=="PercentCover" | CoverType=="AvgCoverClass") %>% mutate(Cover = as.numeric(Cover))
head(pct)
range(pct$Cover) # basically 0 to 100
pct %>% filter(Cover > 50) %>% nrow()
pct %>% ggplot(aes(Cover)) + geom_histogram(fill="steelblue2", color="black", binwidth=5) + 
  ylab("Number of records")
# most measures are low, but handful of high cover measures too!
quantile(pct$Cover)
# 25% of dataset is above 37% cover - cool!

## Map
library(mapproj)
states <- map_data("state") %>% filter(long > -100)
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = mv %>% filter(!CoverType=="AbsenceOnly"), 
             aes(Long, Lat), pch=16, size=2.5, inherit.aes = FALSE, color="steelblue2") +
  xlim(-101,-67) + ylim(25,50) + xlab("Longitude") + ylab("Latitude") +
  ggtitle("All records")
# wow!
# just abundance records
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = pct, aes(Long, Lat), pch=16, size=2.5, 
             inherit.aes = FALSE, color="slateblue2") +
  xlim(-101,-67) + ylim(25,50) + xlab("Longitude") + ylab("Latitude") +
  ggtitle("Abundance records")

# plot together
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = mv, aes(Long, Lat), pch=16, size=2.5, 
             inherit.aes = FALSE, color="black") +
  xlim(-101,-67) + ylim(25,50) + xlab("Longitude") + ylab("Latitude") +
  geom_point(data = pct, aes(Long, Lat), pch=16, size=2.5, inherit.aes = FALSE, 
             color="slateblue2")
# overlap in core of range - def missing points in a handful of places

## Draw convex hull over range?
glimpse(mv)
hull_all <- mv %>% filter(!is.na(Long), !is.na(Lat)) %>%
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
  ggtitle("Geographic space of abundance points vs. all points")

## Color by value of cover
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = pct, aes(Long, Lat, color=Cover), pch=16, size=2.5, inherit.aes = FALSE) +
  xlim(-101,-67) + ylim(25,50) + xlab("Longitude") + ylab("Latitude")
# so hard to see...
pct = pct %>% mutate(CoverBins = cut(Cover, breaks = c(0,10,20,30,40,50,60,70,80,90,100))) %>%
  filter(!is.na(CoverBins))
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = pct %>% arrange(CoverBins), 
             aes(Long, Lat, color=CoverBins), pch=16, size=2.5, inherit.aes = FALSE) +
  xlim(-101,-67) + ylim(25,50) + xlab("Longitude") + ylab("Latitude") +
  theme(legend.position = "bottom")
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = pct %>% arrange(Cover), aes(Long, Lat, color=Cover), pch=16, size=2.5, inherit.aes = FALSE) +
  xlim(-101,-67) + ylim(25,50) + xlab("Longitude") + ylab("Latitude") +
  theme(legend.position = "bottom") +
  scale_colour_steps(n.breaks=5, low="blue", high="red")

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
# high cover points definitely represent a subset of geographies within the presence range

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
  geom_point(data = mv, aes(Long, Lat, color=PA), pch=16, size=2.5, inherit.aes = FALSE) +
  xlim(-101,-67) + ylim(25,50) + xlab("Longitude") + ylab("Latitude") +
  facet_wrap(~PA) + theme(legend.position = "none")

## Pull in environmental data!
library(raster)
library(maptools)

# Make a new cover dataset
dat = rbind(mv %>% filter(CoverType == "PresenceOnly"), pct) %>%
  mutate(Point = ifelse(CoverType=="PresenceOnly", "Presence", "Abundance"))
head(dat)

# Make data spatial
coordinates(dat) <- ~ Long + Lat
proj4string(dat) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
dat

# Pull rasters
setwd("C:/Users/ebeaury/OneDrive - University of Massachusetts/Abundance modeling/Microstegium/EnviroData")
out.wd = "C:/Users/ebeaury/OneDrive - University of Massachusetts/Abundance modeling/Microstegium/EnviroData"

list.files()
# for now, only pull in datasets that are uploaded through drive
files = c("Americas_N_LCM_Cat100.tif", "awc_mean_0_5_integer.tif", "ETa_Apr_Oct_2003_2017_monthlyMean_integer.tif",
          "VCF_percent_treeCover_2000_2016_mean_integer.tif")
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
                                     treecover = VCF_percent_treeCover_2000_2016_mean_integer.tif)
glimpse(pctj)
hist(pctj$humanfoot)
hist(pctj$eta)
hist(pctj$watercontent)
# cool!

## How do the distributions of enviro variables overlap for presence vs. abundance points?
pctj = pctj %>% mutate(Point = factor(Point, levels = c("Presence", "Abundance")))
pctj %>% ggplot(aes(humanfoot, fill=Point)) + geom_histogram(alpha=0.5) # wow!
pctj %>% ggplot(aes(watercontent, fill=Point)) + geom_histogram(alpha=0.5)
pctj %>% ggplot(aes(eta, fill=Point)) + geom_histogram(alpha=0.5)
pctj %>% ggplot(aes(treecover, fill=Point)) + geom_histogram(alpha=0.5)

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

## How do predictors relate to continuous cover?
pctj %>% filter(Point=="Abundance") %>% ggplot(aes(humanfoot, as.numeric(Cover))) + geom_point() + geom_smooth()
pctj %>% filter(Point=="Abundance") %>% ggplot(aes(eta, as.numeric(Cover))) + geom_point() + geom_smooth() # weird 0s?
pctj %>% filter(Point=="Abundance") %>% ggplot(aes(watercontent, as.numeric(Cover))) + geom_point() + geom_smooth() # tail on the high end of values
pctj %>% filter(Point=="Abundance") %>% ggplot(aes(treecover, as.numeric(Cover))) + geom_point() + geom_smooth() # tail on the high end of values

