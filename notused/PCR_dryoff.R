
# Maj Beldring, majbh@sund.ku.dk

# PCR_dryoff
# Check ALL pcr test results...
# Bar plot of all

# To do:

# Included in Data:
## Major and minor info
## PCR all
## DIM previous lacation
## all tests up to 150 days before calving
## all AB treatments up to 150 days before calving
## SCC post calving (2 first control + 5 DIM)
## Last milk pre
## calving outcome
## dates: PCR/BU test, treatment, dry-off, calving, test post calving


#-------------------------------------------------------
# Packages and settings:

library(tidyverse)
library(lubridate) # for date wrangling
Sys.setlocale("LC_ALL","English") # for date formats
memory.size()            # Checking your memory size
memory.limit()           # Checking the set limit
memory.limit(size=56000) # suggest for 64 bit

options(stringsAsFactors = FALSE) # prevent factorizing caracters

#-------------------------------------------------------
# 1: Loading data: ALL data eventually needed for final model, Regardless if not needed during testing

load("M:/PCR_data/PCR_prepare.RData")
load("M:/PCR_data/PCR_merge.RData") # use the pcr_curve and then do the 
rm(production, breed, calvings, dryoff, herd) 
# keep major, minor, pcr, treatments
gc()

#-----------------------------------------------------------------------------
# treatments

## df7: + teat seal
df7 <- left_join(df6, teat_treat, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
# keep only teat treatment in dryoff period
df7 <- df7 %>% 
  filter(TEAT_DATE < CALVING_DATE |is.na(TEAT_DATE)) %>%
  filter(TEAT_DATE >= DRYOFF_DATE |is.na(TEAT_DATE))
#convert all NA's in TEAT_TREAT to 0 (0= NOT PCR tested)
df7$TEAT_TREAT = factor(df7$TEAT_TREAT, levels=c(levels(df7$TEAT_TREAT), 0))
df7$TEAT_TREAT[is.na(df7$TEAT_TREAT)] = 0


## df8: + dryoff_treat
df8 <- left_join(df7, dryoff_treat, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
# keep only treatments in dry-off period
df8 <- df8 %>% 
  filter(DRYTREAT_DATE < CALVING_DATE |is.na(DRYTREAT_DATE)) %>%
  filter(DRYTREAT_DATE >= DRYOFF_DATE |is.na(DRYTREAT_DATE))
#convert all NA's in DRY_TEST to 0 (0= NOT PCR tested)
df8$DRY_TREAT = factor(df8$DRY_TREAT, levels=c(levels(df8$DRY_TREAT), 0))
df8$DRY_TREAT[is.na(df8$DRY_TREAT)] = 0


## df8: + other_treat: INCLUDING treatments 150 days prior..
df9 <- left_join(df8, other_treat, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
# keep only treatments close to and in dry-off period
df9 <- df9 %>% 
  filter(OTHER_AB_DATE < CALVING_DATE |is.na(OTHER_AB_DATE)) %>%
  filter(OTHER_AB_DATE + 150 >= DRYOFF_DATE |is.na(OTHER_AB_DATE))
#convert all NA's in OTHER_AB to 0 (0= NOT AB treated)
df9$OTHER_AB[is.na(df9$OTHER_AB)] = 0
# drop duplicates if multiple AB treatment
df10 <- df9 %>% 
  arrange(DYR_ID, KONTROLDATO, desc(OTHER_AB_DATE)) %>%
  distinct(DYR_ID, KONTROLDATO, .keep_all = TRUE)

rm(dryoff_treat, other_treat, teat_treat); gc()

#----------------------------------------------------------------
# PCR tests

## df11: +pcr
df11 <- left_join(df10, pcr, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
# keep only PCR pre calving and maximum 36 days before treatment
df11 <- df11 %>% 
  filter(PCR_DATE < CALVING_DATE |is.na(PCR_DATE)) %>%
  filter(DRYTREAT_DATE - 36 < PCR_DATE |is.na(PCR_DATE))
#convert all NA's in PCR_TEST to 0 (0= NOT AB treated)
df11$PCR_TEST = factor(df11$PCR_TEST, levels=c(levels(df11$PCR_TEST), 0))
df11$PCR_TEST[is.na(df11$PCR_TEST)] = 0

df_all_dates <- df11

rm(pcr, df10, df11); gc()

#-------------------------------------------------------------------------------------


#------------------------------------------------------------
# save cleaned data:

save.image("M:/PCR_data/PCR_dryoff.RData")
gc()



