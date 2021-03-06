## Combining datasets found by Bethany
# EB
# Created 6/18/2021

library(dplyr)
library(ggplot2)

setwd("C:/Users/ebeaury/OneDrive - University of Massachusetts/Abundance modeling/Bethany datasets")
setwd("~/Desktop/OneDrive - University of Massachusetts/Abundance modeling/Bethany datasets")

## Reading in abundance datasets one at a time
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
ncvs_all = read.csv("NCVS_introduced.csv")
ncvs_wv =read.csv("NCVS_WV_EB_5July2021.csv")
ncvs_va = read.csv("NCVS_VA_introduced.csv")
nwca = read.csv("NWCA_all_introduced_2011.csv")
tx = read.csv("TX_qualitative_5_22_2021.csv")
vegbank = read.csv("Vegbank_intro_cleaned.csv")

## Reformating abundance data to matching columns and different types of cover measurements
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
edd_cov_sub = edd_cov %>% distinct() %>% rename(Cover = PctCov) %>%
  mutate(CoverType="PercentCover")
range(edd_cov_sub$Cover)
glimpse(edd_cov_sub)
glimpse(edd_class)
edd_class_sub = edd_class %>% distinct() %>% rename(Cover = AvgCovClass) %>%
  mutate(CoverType="AvgCoverClass")
unique(edd_class_sub$Cover)
glimpse(edd_qual)
edd_qual_sub = edd_qual %>% rename(Cover = Qualitative) %>%
  mutate(CoverType="Qualitative")
unique(edd_qual_sub$Cover)
# florida
glimpse(fl)
fl_sub = fl %>% distinct() %>% select(-CovClass) %>%
  rename(Cover = AvgCovClass) %>% mutate(CoverType = "AvgCoverClass")
unique(fl_sub$Cover)
# gl
glimpse(gl)
gl_sub = gl %>% select(-StemCount.Range) %>% rename(Cover = StemCount.Avg) %>%
  mutate(CoverType = "AvgStemCount")
# il
glimpse(il)
il_sub = il %>% distinct() %>%
  rename(Cover = PctCov) %>% mutate(CoverType = "PercentCover")
il_sub$CoverType[is.na(il_sub$Cover)] <- "PresenceOnly"
# imap
glimpse(imap_cov)
imap_sub = imap_cov %>% distinct() %>%
  rename(Cover = PctCov) %>% mutate(CoverType = "PercentCover")
range(imap_sub$Cover)
glimpse(imap_class)
unique(imap_class$AvgCovClass)
imap_class_sub = imap_class %>% distinct() %>% select(-CovClass) %>%
  rename(Cover = AvgCovClass) %>% mutate(CoverType = "AvgCoverClass")
# nceas
glimpse(nceas) # good to go
# misin
glimpse(misin)
misin_sub = misin %>% rename(Cover=Qualitative) %>% mutate(CoverType = "Qualitative")
unique(misin_sub$Cover)
# nas
glimpse(nas)
nas_sub = nas %>% select(-StemCount.Range) %>% rename(Cover = StemCount.Avg) %>%
  mutate(CoverType = "AvgStemCount")
# ncvs
glimpse(ncvs_all)
glimpse(ncvs_wv)
glimpse(ncvs_va)
ncvs_sub = rbind(ncvs_all, ncvs_va,ncvs_wv %>% select(-X)) %>%
  rename(Cover=PctCov) %>% mutate(CoverType="PercentCover") %>% distinct()
glimpse(ncvs_sub)
# nwca
glimpse(nwca)
nwca_sub = nwca %>% distinct() %>%
  rename(Cover = PctCov) %>% mutate(CoverType = "PercentCover")
nwca_sub$CoverType[is.na(nwca_sub$Cover)] <- "PresenceOnly"
range(nwca_sub$Cover, na.rm=T)
# tx
glimpse(tx)
tx_sub = tx %>% distinct() %>% rename(Cover=Qualitative) %>%
  mutate(CoverType="Qualitative")
unique(tx_sub$Cover)
tx_sub %>% filter(is.na(Cover))
tx_sub$CoverType[is.na(tx_sub$Cover)] <- "PresenceOnly"
# vegbank
glimpse(vegbank)
veg_bank_sub = vegbank %>% rename(Cover=PctCov) %>% mutate(CoverType="PercentCover")
range(veg_bank_sub$Cover)

## Read in additional presence points
calflor = read.csv("all.points.calflora.selected_28June2021_cleaned.csv")
edd_pres = read.csv("EDDMaps_allpts_11_17_2020.csv")
fl_pres = read.csv("FLINV_all_occurrences_2011.csv")
gl_pres = read.csv("GLIFWC_all_intro_cleaned.csv")
imap_pres = read.csv("iMap_all_occurrences_4_30_2021.csv")
misin_pres = read.csv("MISIN_all_intro.csv")
nas_pres = read.csv("NAS_all_intro_cleaned.csv")
nwca_pres = read.csv("NWCA_all_introduced_2011.csv")
tx_pres = read.csv("TX_all_occurrences_5_22_2021.csv")

## Clean these up to match abundance points
# cal
glimpse(calflor)
cal_sub = calflor %>% filter(is.na(Qualitative) & is.na(PctCov) & is.na(CovClass)) %>%
  distinct() %>% select(UniqueID, Dataset, Long, Lat, SpCode) %>%
  mutate(Cover=NA, CoverType = "PresenceOnly") %>% distinct()
glimpse(cal_sub)
# edd
glimpse(edd_pres)
edd_pres_sub = edd_pres %>% filter(is.na(Qualitative) & is.na(PctCov) & is.na(AvgCovClass)) %>%
  select(UniqueID, Lat, Long, SpCode, Dataset) %>%
  mutate(Cover = NA, CoverType = "PresenceOnly") %>% distinct()
glimpse(edd_pres_sub)
# fl
glimpse(fl_pres)
unique(fl_pres$CovClass)
fl_pres_sub = fl_pres %>% filter(CovClass=="ZZ") %>%
  select(-CovClass, -Year) %>% 
  mutate(Cover=NA, CoverType = "PresenceOnly")%>% distinct()
# gl
glimpse(gl_pres)
unique(gl_pres$StemCount)
gl_pres_sub = gl_pres %>% filter(StemCount==" ") %>%
  select(-StemCount) %>% 
  mutate(Cover = NA, CoverType = "PresenceOnly")%>% distinct()
# imap
glimpse(impa_pres)
impa_pres_sub = impa_pres %>% filter(PctCov=="NULL" & CovClass == "NULL") %>%
  select(-PctCov, -CovClass) %>% mutate(Cover = NA, CoverType = "PresenceOnly") %>% distinct()
# misin
glimpse(misin_pres)
unique(misin_pres$Qualitative)
misin_pres_sub = misin_pres %>% filter(is.na(Qualitative)) %>% rename(Cover=Qualitative) %>%
  mutate(CoverType="PresenceOnly") %>% distinct()
# nas
glimpse(nas_pres)
unique(nas_pres$StemCount)
nas_pres_sub = nas_pres %>% filter(StemCount=="NULL" | StemCount=="") %>%
  select(UniqueID, Dataset, Long, Lat, SpCode) %>%
  mutate(Cover=NA, CoverType = "PresenceOnly") %>% distinct()
# nwca
glimpse(nwca_pres)
unique(nwca_pres$PctCov)
nwca_pres_sub = nwca_pres %>% filter(is.na(PctCov)) %>%
  rename(Cover = PctCov) %>% mutate(CoverType = "PresenceOnly") %>% distinct()
# tx
glimpse(tx_pres)
unique(tx_pres$Qualitative)
tx_pres_sub = tx_pres %>% filter(Qualitative=="" | Qualitative=="None") %>%
  select(UniqueID, Dataset, Long, Lat, SpCode, Qualitative) %>%
  rename(Cover = Qualitative) %>%
  mutate(CoverType = ifelse(Cover=="None", "AbsenceOnly", "PresenceOnly")) %>%
  distinct()
table(tx_pres_sub$CoverType)

## merge
all = rbind(cal_sub, calclass_sub, calcov_sub, calqual_sub, edd_class_sub, edd_cov_sub,edd_pres_sub,
            edd_qual_sub, fl_pres_sub, fl_sub, gl_pres_sub, gl_sub, il_sub, imap_class_sub, imap_sub,
            impa_pres_sub, misin_pres_sub, misin_sub, nas_pres_sub,nas_sub,nceas, ncvs_sub, nwca_pres_sub,
            nwca_sub, tx_pres_sub, tx_sub, veg_bank_sub)
unique(all$Dataset)
table(all$CoverType)

# make 0% cover measures and 'none' qualitative measures absences
all$CoverType[all$Cover==0] <- "AbsenceOnly"
all$CoverType[all$Cover=="None"] <- "AbsenceOnly"
head(all)
table(all$CoverType) # cool!

# add a column distinguishing presence and absence
all2 = all %>% mutate(PA = ifelse(CoverType=="AbsenceOnly", "Absence", "Presence"))
table(all2$PA)

# are there cover measurements that exceed 100%?
all2 %>% filter(CoverType=="PercentCover") %>% mutate(Cover = as.numeric(Cover)) %>%
  filter(Cover > 100) %>% distinct(Dataset)
all2 %>% filter(CoverType=="PercentCover") %>% mutate(Cover = as.numeric(Cover)) %>%
  filter(Cover > 100) # only one plot in NEON, but a bunch of NCVS plots
# leave for now

# export a merged dataset
#write.csv(all2, "MergedDatasets_5July2021_EB.csv", row.names=F)