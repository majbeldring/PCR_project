
# Maj Beldring, majbh@sund.ku.dk
# UCPH, 2020

# PCR_merge: Merge cleaned and prepared data
# Script #3 in PCR project

#-------------------------------------------------------
# Packages and settings:

library(tidyverse)
library(lubridate) # for date wrangling
memory.size()            # Checking your memory size
memory.limit()           # Checking the set limit
memory.limit(size=56000) # suggest for 64 bit

#-------------------------------------------------------
# Loading data: 

load("M:/PCR_data/PCR_prepare.RData")
rm(major, minor, pcr_full); gc()

#-----------------------------------------------------------------------
# Merging step by step to production data

## df1: Production + herd type
df1 <- left_join(production, herd, by = "BES_ID", sort="TRUE",allow.cartesian=TRUE)
df1 <- df1 %>% 
  filter(DATO_FRA <= KONTROLDATO) %>% 
  filter(DATO_TIL >= KONTROLDATO) %>%
  dplyr::select(-DATO_FRA, -DATO_TIL) %>%
  mutate(HERDTYPE = case_when(HERD_TYPE == 'con' ~ 1, HERD_TYPE == 'eco' ~ 0))

rm(herd, production); gc()

## df2: + breed
df2 <- full_join(df1, breed, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
df2 <- df2 %>% drop_na() # loosing 40.000 obs from df1

rm(df1, breed); gc()

## df3: + calvings (create DIM + parity + Calving_month/dryoff_month + calving outcome)
df3 <- full_join(df2, calvings, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
df3 <- df3 %>% 
  drop_na() %>% 
  filter(CALVING_DATE < KONTROLDATO) %>%
  filter(KONTROLDATO - 310 < CALVING_DATE)

rm(df2, calvings); gc()

## df4: + dryoff
df4 <- full_join(df3, dryoff, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
df4 <- df4 %>% 
  drop_na() %>% 
  filter(DRYOFF_DATE < CALVING_DATE) %>%
  filter(CALVING_DATE - 100 < DRYOFF_DATE)

#------------------------------------------------------------------
# DIM

# add days in milk, 
df4$DIM <- as.Date(as.character(df4$KONTROLDATO), format="%Y-%m-%d")-
  as.Date(as.character(df4$CALVING_DATE), format="%Y-%m-%d")

# drop duplicates with overlapping DIM
df4 <- df4 %>% 
  arrange(DYR_ID, KONTROLDATO, desc(CALVING_DATE)) %>%
  distinct(DYR_ID, KONTROLDATO, .keep_all = TRUE)

df_curve <- df4 # saving data will all controls during lactation phases
rm(df3, dryoff); gc()

## df5: removing first 5 DIM, and create the SCC pre coloumn
df5 <- df4 %>%
  filter(DIM > 5) 

#--------------------------------------------------------------
# creating pre coloumns

# create the SCCpre coloumn
df5 <- df5 %>%
  group_by(DYR_ID, PARITY) %>%
  summarise(SCCpre = last(SCC)) %>%
  mutate(SCCpre = lag(SCCpre)) %>%
  left_join(df5, by = c('DYR_ID', 'PARITY')) %>%
  group_by(DYR_ID, PARITY) %>%
  mutate(SCCpre = replace(SCCpre, -1, NA)) %>%
  dplyr::select(-SCCpre, SCCpre)

# create the MILKpre coloumn
df5 <- df5 %>%
  group_by(DYR_ID, PARITY) %>%
  summarise(MILKpre = last(MILK)) %>%
  mutate(MILKpre = lag(MILKpre)) %>%
  left_join(df5, by = c('DYR_ID', 'PARITY')) %>%
  group_by(DYR_ID, PARITY) %>%
  mutate(MILKpre = replace(MILKpre, -1, NA)) %>%
  dplyr::select(-MILKpre, MILKpre)

# keep only last SCC pre dryoff and first SCC +5 DIM after calving
df6 <- df5 %>% drop_na()

rm(df4, df5)
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


## df8: + other_treat
df9 <- left_join(df8, other_treat, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
# keep only treatments close to and in dry-off period
df9 <- df9 %>% 
  filter(OTHER_AB_DATE < CALVING_DATE |is.na(OTHER_AB_DATE)) %>%
  filter(OTHER_AB_DATE >= DRYOFF_DATE |is.na(OTHER_AB_DATE))
#convert all NA's in OTHER_AB to 0 (0= NOT AB treated)
df9$OTHER_AB[is.na(df9$OTHER_AB)] = 0
# drop duplicates if multiple AB treatment
df10 <- df9 %>% 
  arrange(DYR_ID, KONTROLDATO, desc(OTHER_AB_DATE)) %>%
  distinct(DYR_ID, KONTROLDATO, .keep_all = TRUE)

rm(df6, df7, df8, df9, dryoff_treat, other_treat, teat_treat); gc()

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
# final data

# remove date variables and place variables in order:
df_pcr <- df_all_dates %>%
  dplyr::select(DYR_ID, BES_ID, HERDTYPE, BREED_BI, PARITY, IMI, 
                SCC, SCCpre, MILK, MILKpre, 
                PCR_TEST, DRY_TREAT, OTHER_AB, TEAT_TREAT,
                RES_MAJOR, RES_MINOR)

# create logSCC
df_pcr <- df_pcr %>%
  mutate(logSCC = log(SCC)) %>%
  mutate(logSCCpre = log(SCCpre))

#----------------------------------------------------------------------------------------

save.image("M:/PCR_data/PCR_merge.RData") 

