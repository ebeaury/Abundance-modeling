## Combining datasets found by Bethany
# EB
# Created 6/18/2021

library(dplyr)
library(ggplot2)

setwd("C:/Users/ebeaury/OneDrive - University of Massachusetts/Abundance modeling/Bethany datasets")
setwd("~/Desktop/OneDrive - University of Massachusetts/Abundance modeling/Bethany datasets")

## Reading in abundance datasets one at a time
#veg = read.csv("All_VegBank_KPEACH_EB_reduced.csv")
calclass = read.csv("Calflora_covclass_28June2021_cleaned.csv")
calqual = read.csv("Calflora_qualitative_28June2021_cleaned.csv")
calcov = read.csv("Calflora_pctcov_28June2021_cleaned.csv")
edd_cov = read.csv("EDDMaps_pctcov_11_17_2020_cleaned.csv")
edd_class = read.csv("EDDMaps_covclass_11_17_2020_cleaned.csv")
edd_qual = read.csv("EDDMaps_qualitative_11_17_2020_cleaned.csv")
fl = read.csv("FLINV_avg_cov_classes_2011.csv")
gl = read.csv("GLIFWC_stemcount_intro_cleaned.csv")
il = read.csv("IL_CTAP_pctcov_cleaned.csv")
imap_cov = read.csv("iMap_pctcov_4_30_2021.csv")
imap_class = read.csv("iMap_avg_covclass_4_30_2021.csv")
nceas = read.csv("Latest_BLM_NPS_NEON_FIA_introduced.csv")
misin = read.csv("MISIN_intro_qualitative.csv")
nas = read.csv("NAS_stemcount_intro_cleaned.csv")
ncvs_new = read.csv("NCVS_introduced.csv")
ncvs_old =read.csv("All_VegBank_KPEACH_EB_reduced.csv")

nwca = read.csv("NWCA_all_introduced_2011.csv")
tx = read.csv("TX_all_occurrences_5_22_2021.csv")
#vahnp = read.csv("VANHP_reduced_06_16_21_EB.csv")
#nps = read.csv("NPS.AllSpTraits_21April2021.csv")
#blm = read.csv("AIM_AllSpTax_24June2020.csv")
#fia = read.csv("FIA Veg data Latest Traits 8-4-20.csv")
#neon = read.csv("NEONdata_flatted20210225traits.csv")

## Reformating abundance data to matching columns and different types of cover measurements
# vegbank
glimpse(veg)
veg_sub = veg %>% filter(ExoticStatus=="I", PctCov > 0) %>%
  select(UniqueID, Dataset, Lat, Long, SpCode, PctCov) %>%
  rename(Cover = PctCov) %>% mutate(CoverType = "PercentCover")
range(veg_sub$Cover)
# cal flora
glimpse(calclass)
calclass_sub = calclass %>% distinct() %>% rename(Cover = CovClass) %>%
  mutate(CoverType = "CoverClass")
glimpse(calqual)
calqual_sub = calqual %>% distinct() %>% rename(Cover = Qualitative) %>%
  mutate(CoverType = "Qualitative")
glimpse(calcov)
calcov_sub = calcov %>% distinct() %>% rename(Cover = PctCov) %>%
  mutate(CoverType = "PercentCover")
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
fl_sub = fl %>% distinct() %>% rename(Cover = AvgCovClass) %>% mutate(CoverType = "AvgCoverClass")
unique(fl_sub$Cover)
# il
glimpse(il)
il_sub = il %>% distinct() %>% filter(!is.na(PctCov)) %>%
  rename(Cover = PctCov) %>% mutate(CoverType = "PercentCover")
# gl
glimpse(gl)
gl_sub = gl %>% select(-StemCount.Range) %>% rename(Cover = StemCount.Avg) %>%
  mutate(CoverType = "MeanStemCount")

# imap
glimpse(imap_cov)
imap_sub = imap_cov %>% distinct() %>%
  rename(Cover = PctCov) %>% mutate(CoverType = "PercentCover")
range(imap_sub$Cover)
glimpse(imap_class)
unique(imap_class$AvgCovClass)
imap_class_sub = imap_class %>% distinct() %>% select(-CovClass) %>%
  rename(Cover = AvgCovClass) %>% mutate(CoverType = "AvgCoverClass")
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
  rename(Cover = PctCov) %>% mutate(CoverType = "PercentCover") %>% distinct()
glimpse(vahnp_sub)
range(vahnp_sub$Cover)
# nps
glimpse(nps)
# fix absolute cover to be relative cover
nps_sub = nps %>% group_by(UniqueID) %>% summarise(TotalPlotCover = sum(Pct_Cov)) %>%
  left_join(nps) %>% mutate(RelCov = Pct_Cov / TotalPlotCover) %>%
  filter(Exotic=="I") %>%
  select(UniqueID, Dataset, Long, Lat, Species, RelCov) %>%
  rename(Cover=RelCov, SpCode = Species) %>% mutate(CoverType = "PercentCover", Cover = Cover*100) %>% 
  distinct()
glimpse(nps_sub)
range(nps_sub$Cover)
max(nps_sub$Cover)
# fia
glimpse(fia)
fia_sub = fia %>% mutate(Dataset="FIA", UniqueID = paste("FIA", PLOT, sep="_")) %>% filter(inv_L48=="I") %>%
  select(UniqueID, Dataset, LON, LAT, VEG_SPCD, PlotCover) %>%
  rename(Cover=PlotCover, Long = LON, Lat = LAT, SpCode =VEG_SPCD) %>% mutate(CoverType = "PercentCover") %>% distinct()
glimpse(fia_sub)
range(fia_sub$Cover)
# blm
glimpse(blm)
blm_sub = blm %>% mutate(Dataset = DataSet, UniqueID = PLOTKEY, Cover = prop.cover*100) %>%
  select(UniqueID, Dataset, NAD83.Y, NAD83.X, code, Cover) %>%
  rename(SpCode=code, Long = NAD83.X, Lat = NAD83.Y) %>% mutate(CoverType = "PercentCover") %>% distinct()
glimpse(blm_sub)
range(blm_sub$Cover)
# neon
glimpse(neon)
neon_sub = neon %>% filter(Native.Status=="I") %>%
  select(dataset,plotID, decimalLongitude,decimalLatitude, Accepted.Symbol,totalcover_sum,) %>%
  mutate(CoverType = "PercentCover") %>% 
  rename(Dataset=dataset, Long = decimalLongitude, Lat = decimalLatitude, SpCode=Accepted.Symbol, Cover=totalcover_sum, UniqueID = plotID)
glimpse(neon_sub)
range(neon_sub$Cover)
# misin
glimpse(misin)
misin_sub = misin %>% rename(Cover=Qualitative) %>% mutate(CoverType = "Qualitative")
unique(misin_sub$Cover)
# nas
glimpse(nas)
nas_sub = nas %>% select(-StemCount.Range) %>% rename(Cover = StemCount.Avg) %>%
  mutate(CoverType = "MeanStemCount")
# cal flora

# ncvs
glimpse(ncvs_new)
glimpse(ncvs_old)
unique(ncvs_old$Dataset)
# clean up NCVS data from Kristen's pull
ncvs_old2 = ncvs_old %>% filter(Dataset=="NCVS_WV") %>% 
  select(UniqueID, Long, Lat, SpCode, PctCov) %>% distinct() %>%
  mutate(Dataset="NCVS")
head(ncvs_old2)
# combine with NCVS data on google drive and drop duplicates
ncvs_sub = rbind(ncvs_new, ncvs_old2) %>% distinct() %>% rename(Cover=PctCov) %>%
  mutate(CoverType="PercentCover")
glimpse(ncvs_sub)
# export NCVS_WV to match format for other data
write.csv(ncvs_old2, "NCVS_WV_EB_5July2021.csv")

## Read in additional presence points
edd_pres = read.csv("EDDMaps_allpts_11_17_2020.csv")
impa_pres = read.csv("iMap_all_occurrences_4_30_2021.csv")
gl_pres = read.csv("GLIFWC_all_intro_cleaned.csv")
fl_pres = read.csv("FLINV_all_occurrences_2011.csv")
nas_pres = read.csv("NAS_all_intro_cleaned.csv")
misin_pres = read.csv("MISIN_all_intro.csv")
calflor = read.csv("all.points.calflora.selected_28June2021_cleaned.csv")

## Clean these up to match abundance points
glimpse(edd_pres)
edd_pres_sub = edd_pres %>% select(UniqueID, Lat, Long, SpCode, Dataset) %>%
  mutate(Cover = NA, CoverType = "PresenceOnly") %>% distinct()
glimpse(edd_pres_sub)
# imap
glimpse(impa_pres)
impa_pres_sub = impa_pres %>% filter(PctCov=="NULL" & CovClass == "NULL") %>%
  select(-PctCov, -CovClass) %>% mutate(Cover = NA, CoverType = "PresenceOnly") %>% distinct()
# gl
glimpse(gl_pres)
gl_pres_sub = gl_pres %>% select(-StemCount) %>% 
  mutate(Cover = NA, CoverType = "PresenceOnly")%>% distinct()
# fl
glimpse(fl_pres)
fl_pres_sub = fl_pres %>% select(-CovClass, -Year) %>% 
  mutate(Cover=NA, CoverType = "PresenceOnly")%>% distinct()
# nas
glimpse(nas_pres)
nas_pres_sub = nas_pres %>% select(UniqueID, Dataset, Long, Lat, SpCode) %>%
  mutate(Cover=NA, CoverType = "PresenceOnly") %>% distinct()
# misin
glimpse(misin_pres)
unique(misin_pres$Qualitative)
misin_pres_sub = misin_pres %>% filter(is.na(Qualitative)) %>% rename(Cover=Qualitative) %>%
  mutate(CoverType="PresenceOnly") %>% distinct()

## merge
all = rbind(edd_class_sub, edd_cov_sub, edd_qual_sub, fl_sub, il_sub, imap_sub, nwca_sub, tx_sub, vahnp_sub, veg_sub,
            nps_sub, fia_sub, blm_sub, neon_sub, misin_sub, nas_sub, gl_sub, edd_pres_sub, gl_pres_sub,
            fl_pres_sub, misin_pres_sub, nas_pres_sub, impa_pres_sub)
unique(all$Dataset)
table(all$CoverType)

# make 0% cover measures and 'none' qualitative measures absences
all$CoverType[all$Cover==0] <- "AbsenceOnly"
all$CoverType[all$Cover=="None"] <- "AbsenceOnly"
head(all)
table(all$CoverType) # cool!

all2 # add a column distinguishing presence and absence
all = all %>% mutate(PA = ifelse(CoverType=="AbsenceOnly", "Absence", "Presence"))
table(all$PA)

# are there cover measurements that exceed 100%?
all %>% filter(CoverType=="PercentCover") %>% mutate(Cover = as.numeric(Cover)) %>%
  filter(Cover > 100) %>% distinct(Dataset)
all %>% filter(CoverType=="PercentCover") %>% mutate(Cover = as.numeric(Cover)) %>%
  filter(Cover > 100) %>% filter(Dataset=="NEON") # only one plot in NEON
# drop weird NEON plot
all2 = all %>% filter(!(UniqueID == "OAES_008" & SpCode == "BOBL"))

# export a merged dataset
#write.csv(all2, "MergedDatasets_24June2021_EB.csv", row.names=F)

# add a column distinguishing presence, absence and abundance?
all2 = all2 %>% mutate(Type = ifelse(CoverType=="AbsenceOnly", "Absence",
                                     ifelse(CoverType == "PresenceOnly", "Presence", "Abundance")))
table(all2$Type)
unique(all2$CoverType)

# export an nceas merged datasets
nceas = rbind(nps_sub, blm_sub, fia_sub, neon_sub)
head(nceas)
# write.csv(nceas, "Latest_BLM_NPS_NEON_FIA_introduced.csv", row.names=F)
