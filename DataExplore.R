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



