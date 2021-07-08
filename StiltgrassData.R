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
