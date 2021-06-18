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
glimpse(fl)

il = read.csv("IL_CTAP_pctcov_cleaned.csv")
glimpse(il)

imap = read.csv("iMap_all_occurrences_4_30_2021.csv")
glimpse(imap)

nwca = read.csv("NWCA_all_introduced_2011.csv")
glimpse(nwca)

tx = read.csv("TX_all_occurrences_5_22_2021.csv")
glimpse(tx)

vahnp = read.csv("VANHP_reduced_06_16_21_EB.csv")
glimpse(vahnp)

## Reformating to matching columns and different types of cover measurements
# vegbank
glimpse(veg)
veg_sub = veg %>% filter(ExoticStatus=="I") %>%
  select(UniqueID, Dataset, Lat, Long, SpCode, PctCov) %>%
  rename(Cover = PctCov) %>% mutate(CoverType = "PercentCover")
# eddmaps
glimpse(edd_cov)
edd_cov_sub = edd_cov %>% distinct() %>% rename(Long = Longitude.Decimal, Lat = Latitude.Decimal, Cover = PctCov) %>%
  mutate(CoverType="PercentCover")
edd_cov_sub = edd_cov_sub %>% mutate(UniqueID = paste0("EDDCover",seq(1:nrow(edd_cov_sub))))
glimpse(edd_cov_sub)
glimpse(edd_class)
edd_class_sub = edd_class %>% distinct() %>% rename(Long = Longitude.Decimal, Lat = Latitude.Decimal, Cover = CovClass) %>%
  mutate(CoverType="CoverClass")
edd_class_sub = edd_class_sub %>% mutate(UniqueID = paste0("EDDCClass",seq(1:nrow(edd_class_sub))))
glimpse(edd_qual)
edd_qual_sub = edd_qual %>% rename(Long = Longitude.Decimal, Lat = Latitude.Decimal, Cover = Qualitative) %>%
  mutate(CoverType="Qualitative")
edd_qual_sub = edd_qual_sub %>% mutate(UniqueID = paste0("EDDCQual", seq(1:nrow(edd_qual_sub))))


