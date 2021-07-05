## Exploring abundance and presence data for microstegium (Eve's focal species)

library(dplyr)
library(ggplot2)

setwd("C:/Users/ebeaury/OneDrive - University of Massachusetts/Abundance modeling/Bethany datasets")
setwd("~/Desktop/OneDrive - University of Massachusetts/Abundance modeling/Bethany datasets")

## Load full abundance dataset
records = read.csv("MergedDatasets_5July2021_EB.csv")

## Get codes for microstegium
codes = c("MIVI", "ANVI11", "EUVI3", "EUVIV", "MIVII")
mv = records %>% filter(SpCode %in% codes)
head(mv)

## Do synonym codes actually pull?
unique(mv$SpCode) # nope lol

## How many records of different types
table(mv$PA)
# huh - some real absensces
table(mv$CoverType)
# most records are presence only, but a decent anount of percent cover
# probs ignore avg stem count
# but could consider incorporating cover classes or qualitative measures?
mv %>% filter(CoverType=="AvgCoverClass") %>% distinct(Cover)
mv %>% filter(CoverType=="Qualitative") %>% distinct(Cover)
# need to look up how the qualitative measures are determined (and how other studies lumped them in?)

## Look at continuous vals
pct = mv %>% filter(CoverType=="PercentCover") %>% mutate(Cover = as.numeric(Cover))
hist(pct$Cover)
# most measures are low, but handful of high cover measures too!
quantile(pct$Cover)
# 25% of dataset is above 37% cover - cool!

## Map
library(mapproj)
states <- map_data("state") %>% filter(long > -100)
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = mv, aes(Long, Lat), pch=16, size=2.5, alpha=0.2, inherit.aes = FALSE) +
  xlim(-105,-67) + ylim(24,51) + xlab("Longitude") + ylab("Latitude") +
  theme(axis.text=element_text(size=12), legend.text=element_text(size=12))
# wow!
# just continuous cover?
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = pct, aes(Long, Lat), pch=16, size=2.5, alpha=0.2, inherit.aes = FALSE) +
  xlim(-105,-67) + ylim(24,51) + xlab("Longitude") + ylab("Latitude") +
  theme(axis.text=element_text(size=12), legend.text=element_text(size=12))
# plot together
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = mv, aes(Long, Lat), pch=16, size=2.5, alpha=0.2, inherit.aes = FALSE) +
  xlim(-105,-67) + ylim(24,51) + xlab("Longitude") + ylab("Latitude") +
  theme(axis.text=element_text(size=12), legend.text=element_text(size=12)) +
  geom_point(data = pct, aes(Long, Lat), pch=16, size=2.5, alpha=0.2, inherit.aes = FALSE, 
             color="green")
# overlap in core of range - def missing points in a handful of places

## Draw convex hull over range?
glimpse(mv)
hull_all <- mv %>% filter(!is.na(Long), !is.na(Lat)) %>%
  slice(chull(Long, Lat))
hull_pct = pct %>% filter(!is.na(Long), !is.na(Lat)) %>%
  slice(chull(Long, Lat))
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_polygon(data= hull_all, aes(Long, Lat), alpha=0.3, inherit.aes = FALSE) + 
  geom_polygon(data= hull_pct, aes(Long, Lat), alpha=0.3, inherit.aes = FALSE,
               fill="green") + theme_bw() +
  ggtitle("Geographic space of percent cover points vs. all points")

## Color by value of cover
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = pct, aes(Long, Lat, color=Cover), pch=16, size=2.5, alpha=0.2, inherit.aes = FALSE) +
  xlim(-105,-67) + ylim(24,51) + xlab("Longitude") + ylab("Latitude") +
  theme(axis.text=element_text(size=12), legend.text=element_text(size=12))
# so hard to see...
pct = pct %>% mutate(CoverBins = cut(Cover, breaks = c(0,10,20,30,40,50,60,70,80,90,100))) %>%
  filter(!is.na(CoverBins))
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = pct, aes(Long, Lat, color=CoverBins), pch=16, size=2.5, alpha=0.2, inherit.aes = FALSE) +
  xlim(-105,-67) + ylim(24,51) + xlab("Longitude") + ylab("Latitude")
# maybe make a simpler cutoff
pct = pct %>% mutate(CoverBins = cut(Cover, breaks = c(0,50,100))) %>%
  filter(!is.na(CoverBins))
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = pct, aes(Long, Lat, color=CoverBins), pch=16, size=2.5, alpha=0.2, inherit.aes = FALSE) +
  xlim(-105,-67) + ylim(24,51) + xlab("Longitude") + ylab("Latitude")

# as hulls
hull_lower <- pct %>% filter(!is.na(Long), !is.na(Lat), Cover < 10) %>%
  slice(chull(Long, Lat))
hull_mid <- pct %>% filter(!is.na(Long), !is.na(Lat), Cover > 10 & Cover < 50) %>%
  slice(chull(Long, Lat))
hull_high <- pct %>% filter(!is.na(Long), !is.na(Lat), Cover >50) %>%
  slice(chull(Long, Lat))
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_polygon(data= hull_all, aes(Long, Lat), alpha=0.7, inherit.aes = FALSE) +
  geom_polygon(data= hull_lower, aes(Long, Lat), alpha=0.7, inherit.aes = FALSE,
               fill="red") + 
  geom_polygon(data= hull_mid, aes(Long, Lat), alpha=0.7, inherit.aes = FALSE,
               fill="green") + 
  geom_polygon(data= hull_high, aes(Long, Lat), alpha=0.7, inherit.aes = FALSE,
               fill="blue") + theme_bw()
# high cover points definitely represent a subset of geographies within the presence range

## Presence vs absence
ggplot(states, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", colour = "black") + 
  geom_point(data = mv, aes(Long, Lat, color=PA), pch=16, size=2.5, inherit.aes = FALSE) +
  xlim(-105,-67) + ylim(24,51) + xlab("Longitude") + ylab("Latitude") +
  facet_wrap(~PA)


## Export
setwd("~/Desktop/OneDrive - University of Massachusetts/Abundance modeling/Microstegium")
head(mv)
mv_ex = mv %>% filter(!is.na(Long), !is.na(Lat))

write.csv(mv_ex, "MV_allpoints.csv", row.names=F)

pct = mv %>% filter(!is.na(Long), !is.na(Lat), CoverType=="PercentCover")
write.csv(pct, "MV_pctcov.csv", row.names=F)  

presence = mv %>% filter(!is.na(Long), !is.na(Lat), PA=="Presence")
write.csv(presence, "MV_presenceonly.csv", row.names=F)
