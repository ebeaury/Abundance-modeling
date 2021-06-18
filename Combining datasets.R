## Combining datasets found by Bethany
# EB
# 6/18/2021

library(dplyr)
library(ggplot2)

setwd("C:/Users/ebeaury/OneDrive - University of Massachusetts/Abundance modeling/Bethany datasets")

## Reading in datasets one at a time

veg = read.csv("All_VegBank_KPEACH_EB_reduced.csv")
edd_cov = read.csv("EDDMaps_pctcov_11_17_2020_cleaned.csv")
edd_class = read.csv("EDDMaps_covclass_11_17_2020_cleaned.csv")
edd_qual = read.csv("EDDMaps_qualitative_11_17_2020_cleaned.csv")
fl = read.csv("FLINV_cov_classes_2011.csv")
il = read.csv("IL_CTAP_pctcov_cleaned.csv")
imap_cov = read.csv("iMap_pctcov_4_30_2021.csv")
nwca = read.csv("NWCA_all_introduced_2011.csv")
tx = read.csv("TX_all_occurrences_5_22_2021.csv")
vahnp = read.csv("VANHP_reduced_06_16_21_EB.csv")

## Reformating to matching columns and different types of cover measurements
# vegbank
glimpse(veg)
veg_sub = veg %>% filter(ExoticStatus=="I", PctCov > 0) %>%
  select(UniqueID, Dataset, Lat, Long, SpCode, PctCov) %>%
  rename(Cover = PctCov) %>% mutate(CoverType = "PercentCover")
range(veg_sub$Cover)
# eddmaps
glimpse(edd_cov)
edd_cov_sub = edd_cov %>% distinct() %>% rename(Long = Longitude.Decimal, Lat = Latitude.Decimal, Cover = PctCov) %>%
  mutate(CoverType="PercentCover")
range(edd_cov_sub$Cover)
edd_cov_sub = edd_cov_sub %>% mutate(UniqueID = paste0("EDDCover",seq(1:nrow(edd_cov_sub))))
glimpse(edd_cov_sub)
glimpse(edd_class)
edd_class_sub = edd_class %>% distinct() %>% rename(Long = Longitude.Decimal, Lat = Latitude.Decimal, Cover = CovClass) %>%
  mutate(CoverType="CoverClass")
unique(edd_class_sub$Cover)
edd_class_sub = edd_class_sub %>% mutate(UniqueID = paste0("EDDCClass",seq(1:nrow(edd_class_sub))))
glimpse(edd_qual)
edd_qual_sub = edd_qual %>% rename(Long = Longitude.Decimal, Lat = Latitude.Decimal, Cover = Qualitative) %>%
  mutate(CoverType="Qualitative")
edd_qual_sub = edd_qual_sub %>% mutate(UniqueID = paste0("EDDCQual", seq(1:nrow(edd_qual_sub))))
unique(edd_qual_sub$Cover)
# florida
glimpse(fl)
fl_sub = fl %>% distinct() %>% rename(Cover = CovClass) %>% mutate(CoverType = "CoverClass")
unique(fl_sub$Cover)
# il
glimpse(il)
il_sub = il %>% distinct() %>% filter(!is.na(PctCov)) %>%
  rename(Cover = PctCov) %>% mutate(CoverType = "PercentCover")
# imap
glimpse(imap_cov)
imap_sub = imap_cov %>% distinct() %>%
  rename(Cover = PctCov) %>% mutate(CoverType = "PercentCover")
range(imap_sub$Cover)
# nwca
glimpse(nwca)
nwca_sub = nwca %>% distinct() %>% filter(!is.na(PctCov)) %>%
  rename(Cover = PctCov) %>% mutate(CoverType = "PercentCover")
range(nwca_sub$Cover)
# tx
glimpse(tx)
tx_sub = tx %>% distinct() %>% select(-Time_Spent, -Disturbance) %>% rename(Cover=Qualitative) %>%
  mutate(CoverType="Qualitative")
unique(tx_sub$Cover)
#vanhp
glimpse(vahnp)
vahnp_sub = vahnp %>% filter(ExoticStatus=="I") %>% select(UniqueID, Dataset, Long, Lat, SpCode, PctCov) %>%
  rename(Cover = PctCov) %>% mutate(CoverType = "PercentCover")
glimpse(vahnp_sub)
range(vahnp_sub$Cover)

# merge
all = rbind(edd_class_sub, edd_cov_sub, edd_qual_sub, fl_sub, il_sub, imap_sub, nwca_sub, tx_sub, vahnp_sub, veg_sub)
unique(all$Dataset)

# make 0% cover measures and 'none' qualitative measures absences
all$Cover[all$Cover==0] <- "Absent"
all$Cover[all$Cover=="None"] <- "Absent"
head(all)
table(all$CoverType) # cool!

# add a column distinguishing presence and absence
all = all %>% mutate(PA = ifelse(Cover=="Absent", "Absence", "Presence"))
table(all$PA)
