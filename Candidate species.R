## Identifying potential species to test abundande modeling method
library(dplyr)
library(ggplot2)

setwd("~/Desktop/Abundance modeling")

# Starting with eddmaps data (from Bethany)
edd <- read.csv("EDDMaps_pctcov_11_17_2020_cleaned (1).csv")
head(edd)
glimpse(edd)

# What units PctCov?
range(as.numeric(edd$PctCov)) # huh - so Pct cov way off?
hist(edd$PctCov)
edd %>% filter(PctCov > 100)
# 12 weird outliers > 100%
# drop for now
edd = edd %>% filter(PctCov < 101)
head(edd)

# How many species in dataset?
n_distinct(edd$SpCode) # 637

# How many records per species?
edd %>% group_by(SpCode) %>%
  summarise(n_obs = n()) %>%
  ggplot(aes(n_obs)) + geom_histogram()
# most species have really low numbers, others have higher
counts = edd %>% group_by(SpCode) %>%
  summarise(n_obs = n())
# how many species with more than 100 records/
counts %>% filter(n_obs > 100) %>% nrow()
# 181 species with > 100 records, narrows it down quite a bit

# Plot all occurrences?
library(tmap)
library(mapproj)
library(spdep)
library(sf)
data("us_states")
albers <- "+proj=aea +lat_1=29.83333333333334 +lat_2=45.83333333333334 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
us_pj = st_transform(us_states, albers)
border = st_union(us_pj, by_feature = FALSE)
# make eddmaps occurrences spatial
edd_sp =  st_as_sf(edd, coords = c("Long", "Lat"), crs= 4326)
tm_shape(border) +
  tm_polygons(border.col="black", col="white") +
  tm_shape(edd_sp) +
  tm_symbols()
# slow to plot cuz big points, but coverage really good in west, ok in east, not good in midwest
# so def want eastern invaders

# plot only well sampled species
counts = counts %>% filter(n_obs > 500)
edd_sub = edd %>% 
  filter(SpCode %in% counts$SpCode)
# okay so still majority of records even though way fewer species
# makes sense - most species are rare more often than abudnant

# species included in inhabit
inhab = read.csv("INHABIT_sp.csv")
# join code
codes = read.csv(file.choose())
head(codes)
inhab_join = left_join(inhab %>% 
                         rename(Scientific.Name = Scientific.name),
                       codes %>% select(Scientific.Name, Accepted.Symbol))
head(inhab_join)
# worked for the most part!
sum(is.na(inhab_join$Accepted.Symbol))
# 6 missing - whatever

# now subset eddmaps to inhabit sp
potential = edd_sub %>% filter(SpCode %in% inhab_join$Accepted.Symbol)
n_distinct(potential$SpCode)
# narrowed down to 56 sp with presence models + 500 occurrence records

# plot records for these??
pot_sp =  st_as_sf(potential, coords = c("Long", "Lat"), crs= 4326)
tm_shape(border) +
  tm_polygons(border.col="black", col="white") +
  tm_shape(pot_sp) +
  tm_symbols(size=0.1,shape=19)
# cool - so way fewer east coasty species
# subset to records in east of U.S.?
# east of 100 degrees w
range(edd$Long)
potential2 = potential %>% filter(Long > -100)
n_distinct(potential2$SpCode)
# narrows back down to 36
pot_sp =  st_as_sf(potential2, coords = c("Long", "Lat"), crs= 4326)
tm_shape(border) +
  tm_polygons(border.col="black", col="white") +
  tm_shape(pot_sp) +
  tm_symbols(size=0.1,shape=19, col="SpCode")
# recalc records per species
candidates = potential2 %>% group_by(SpCode) %>%
  summarise(n_obs = n()) %>% 
  filter(n_obs > 500)
# wow!!! now down to 17 species
# recognizable already (kudzu, microstegium, cogongrass, etc.)
candidates %>% left_join(codes_nondup)

# join accepted name
candidaes2 = edd_sub %>% filter(SpCode %in% candidates$SpCode)
candidaes2_sp =  st_as_sf(candidaes2, coords = c("Long", "Lat"), crs= 4326)
tm_shape(border) +
  tm_polygons(border.col="black", col="white") +
  tm_shape(candidaes2_sp) +
  tm_symbols(size=0.1,shape=19, col="SpCode")
# okay - so these species have lots of records on east coast, but not necessarily east coast invaders
drop = candidaes2 %>% filter(Long < -100) %>%
  distinct(SpCode)
# drop these...?
candidates3 = candidaes2 %>% filter(!(SpCode %in% drop$SpCode))
candidates3_sp =  st_as_sf(candidates3, coords = c("Long", "Lat"), crs= 4326)
tm_shape(border) +
  tm_polygons(border.col="black", col="white") +
  tm_shape(candidates3_sp) +
  tm_symbols(size=0.5,shape=19, col="SpCode")
# records for east coast specialist invaders!!!
head(candidates3)
# join accepted name
codes_nondup = codes %>% select(Accepted.Symbol, Scientific.Name) %>%
  rename(SpCode = Accepted.Symbol) %>% distinct()
# drop duplicated scientific names
codes_nondup = codes_nondup[!duplicated(codes_nondup$SpCode),]
candidates4 = candidates3 %>% left_join(codes_nondup)
candidates4 %>% filter(is.na(Scientific.Name)) # yay!
unique(candidates4$Scientific.Name)
candidates4_sp =  st_as_sf(candidates4, coords = c("Long", "Lat"), crs= 4326)
tm_shape(border) +
  tm_polygons(border.col="black", col="white") +
  tm_shape(candidates4_sp) +
  tm_symbols(size=0.3,shape=19, col="Scientific.Name") +
  tm_layout(legend.position = c("left", "center"),
            legend.title.size = 1.5,
            legend.text.size = 1,
            legend.bg.color = "white")
# also cool because it has a range of species that are widespread (honeysuckle and stiltgrass)
# vs species that are more habitat specific (lygopodium)
# but generally ranges of species seem to overlap

# now look at range of cover for each species
head(candidates4)
candidates4 %>% ggplot(aes(PctCov)) +
  geom_histogram() +
  facet_wrap(~Scientific.Name, scales="free") +
  theme_bw() + xlab("Percent cover") + ylab("Number of records")
# range of cover measurements for each species - that's good!