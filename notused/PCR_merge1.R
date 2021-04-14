
# Maj Beldring, majbh@sund.ku.dk
# UCPH, 2020

# PCR_merge: Merge cleaned and prepared data

# merging to full data INCLUDING dry off dates ("goldninger" data)
# using "goldninger means we will loose appr. 1/3 of data (or more..)

# merge1 timeline:
#1 calving date
#2 lacation phase (max set to 320 days), Parity responds to this period
#3 dry off date
#4 dry off phase: PCR + treatment (tretments max 50 days after dry-off)
#5 IMI post calving

# to do
## redo treatment joining to avoid data loss (do as in merge2)
## repeat steps in merge2 after seasonal effect has been added in merge2




#-------------------------------------------------------
# Packages and settings:

library(tidyverse)
library(lubridate) # for date wrangling
memory.size()            # Checking your memory size
memory.limit()           # Checking the set limit
memory.limit(size=56000) # suggest for 64 bit



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
  mutate(HERDTYPE = case_when(HERD_TYPE == 'con' ~ 1, HERD_TYPE == 'eco' ~ 0)) %>%
  dplyr::select(-HERD_TYPE)

glimpse(df1) # HERDTYPE: 1=con, 0=eco

dplyr::n_distinct(production$DYR_ID)  # 2.521.615 unique DYR_ID production
dplyr::n_distinct(df1$DYR_ID)         # 2.520.295 unique DYR_ID production
dplyr::n_distinct(production$BES_ID)  # 3933
dplyr::n_distinct(df1$BES_ID)         # 3920

rm(herd, production); gc()



#----------------------------------------------------------
## df2: + breed

df2 <- full_join(df1, breed, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
df2 <- df2 %>% 
  drop_na() %>%
  dplyr::select(-RACE) # 1=holstein, 2=jersey, 3=other dairy breeds

glimpse(df2) # BREED: 1=Holstein, 2=Jersey, 3= other
dplyr::n_distinct(df2$DYR_ID)   # 2.518.572
dplyr::n_distinct(df2$BES_ID)   # 3918

rm(df1, breed); gc()



#--------------------------------------------------------------------
## df3 + df4: + calvings (create DIM + parity )

df3 <- full_join(df2, calvings, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)

# df4: calving date will be start for each lactation
df4 <- df3 %>% 
  drop_na() %>%
  filter(CALVING_DATE < KONTROLDATO) %>%
  filter(KONTROLDATO - 350 < CALVING_DATE) %>% # setting max lact phase to 350 days
  arrange(DYR_ID, KONTROLDATO, desc(CALVING_DATE)) %>%
  distinct(DYR_ID, KONTROLDATO, .keep_all = TRUE)

glimpse(df4)
# data loss:
dplyr::n_distinct(df4$DYR_ID)   # 2.377.244
dplyr::n_distinct(df4$BES_ID)   # 3913

rm(df2, df3, calvings); gc()



#--------------------------------------------------------------------
## df5: + dryoff
dplyr::n_distinct(dryoff$DYR_ID)  # 1.391.660 (2010-2020); so expect a big data los

df5 <- full_join(df4, dryoff, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
df5 <- df5 %>% 
  drop_na() %>%
  filter(DRYOFF_DATE > CALVING_DATE) %>%
  filter(DRYOFF_DATE - 380 < CALVING_DATE) # last kontroldate + 30 days

glimpse(df5) # 
dplyr::n_distinct(df5$DYR_ID)   # 1.093.967 (1.391.660 in dryoff data)
dplyr::n_distinct(df5$BES_ID)   # 3272 (loosing 700 herds..)

rm(df4, dryoff); gc()



#------------------------------------------------------------------
# df5: DIM and LAC

# DIM: add days in milk, 
df5$DIM <- as.Date(as.character(df5$KONTROLDATO), format="%Y-%m-%d")-
  as.Date(as.character(df5$CALVING_DATE), format="%Y-%m-%d")

# LAC: lactation length: Be carefult as only sporadic dryoff registration is available
df5$LAC <- as.Date(as.character(df5$DRYOFF_DATE), format="%Y-%m-%d")-
   as.Date(as.character(df5$CALVING_DATE), format="%Y-%m-%d")

df5 <- df5 %>% 
  mutate(DIM = as.numeric(DIM)) %>%
  mutate(LAC = as.numeric(LAC))


#--------------------------------------------------------------
# df5 + df6: IMI post calving

# create the IMI post coloumn
df5 <- df5 %>%
  group_by(DYR_ID, PARITY) %>%
  summarise(SCCpost = first(SCC)) %>%
  mutate(SCCpost = lead(SCCpost)) %>%
  left_join(df5, by = c('DYR_ID', 'PARITY')) %>%
  group_by(DYR_ID, PARITY) %>%
  mutate(SCCpost = replace(SCCpost, -1, NA)) %>%
  dplyr::select(-SCCpost, SCCpost)

# df6: now make a coloumn with all IMI or not
IMI <- df5 %>%
  dplyr::select(DYR_ID, PARITY, SCCpost) %>%
  drop_na()
df6 <- df5 %>%
  dplyr::select(-SCCpost)
df6 <- left_join(df6, IMI, by = c("DYR_ID", "PARITY"), sort="TRUE",allow.cartesian=TRUE)

# dropping all observations with no post IMI reported:
df6 <- df6 %>%
  drop_na()


glimpse(df6) # 
dplyr::n_distinct(df5$DYR_ID)   # 1.260.852 
dplyr::n_distinct(df6$DYR_ID)   # 751.523 (loss due to: dryoff and next IMI)
dplyr::n_distinct(df5$BES_ID)   # 3376
dplyr::n_distinct(df6$BES_ID)   # 3270 herds; No sign. loss (600 lost with dryoff join)

rm(df5, IMI); gc()
df_curve <- df6

#-----------------------------------------------------------------------------
# prepare pcr and treatments before merging:

# first convert treats (and pcr tests to factors:
teat_treat <- teat_treat %>%
  mutate(TEAT_TREAT = factor(TEAT_TREAT))

dryoff_treat <- dryoff_treat %>%
  mutate(DRY_TREAT = factor(DRY_TREAT))

other_treat <- other_treat %>%
  mutate(OTHER_AB = factor(OTHER_AB))

pcr <- pcr %>%
  mutate(PCR_TEST = factor(PCR_TEST)) %>%
  mutate(RES_MINOR = factor(RES_MINOR)) %>%
  mutate(RES_MINOR = factor(RES_MINOR))


#-----------------------------------------------------------------------------
# Treatments: Teat sealant, dryoff, other AB

## df7+8: + teat seal
df7 <- left_join(df6, teat_treat, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
# keep only teat treatment in dryoff period
df8 <- df7 %>% 
  group_by(DYR_ID, PARITY) %>%
  filter(TEAT_DATE >= DRYOFF_DATE |is.na(TEAT_DATE)) %>%
  filter(TEAT_DATE - 50 < DRYOFF_DATE |is.na(TEAT_DATE))

#convert all NA's in TEAT_TREAT to 0 (0= NOT PCR tested)
df8$TEAT_TREAT = factor(df8$TEAT_TREAT, levels=c(levels(df8$TEAT_TREAT), 0))
df8$TEAT_TREAT[is.na(df8$TEAT_TREAT)] = 0
glimpse(df8) # 
dplyr::n_distinct(df8$DYR_ID)   #702.045
dplyr::n_distinct(df8$BES_ID)   #3261



## df9+10: + dryoff_treat
df9 <- left_join(df8, dryoff_treat, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
# keep only treatments in dry-off period
df10 <- df9 %>% 
  group_by(DYR_ID, PARITY) %>%
  filter(DRYTREAT_DATE >= DRYOFF_DATE |is.na(DRYTREAT_DATE)) %>%
  filter(DRYTREAT_DATE - 50 < DRYOFF_DATE  |is.na(DRYTREAT_DATE))

#convert all NA's in DRY_TEST to 0 (0= NOT PCR tested)
df10$DRY_TREAT = factor(df10$DRY_TREAT, levels=c(levels(df10$DRY_TREAT), 0))
df10$DRY_TREAT[is.na(df10$DRY_TREAT)] = 0
glimpse(df10) # 
dplyr::n_distinct(df10$DYR_ID)   # 377.444
dplyr::n_distinct(df10$BES_ID)   # 2881

str(df11)

## df11+12: + other_treat
df11 <- left_join(df10, other_treat, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
# keep only treatments close to and in dry-off period
df11 <- df11 %>% 
  filter(OTHER_AB_DATE >= DRYOFF_DATE |is.na(OTHER_AB_DATE)) %>%
  filter(OTHER_AB_DATE - 50 < DRYOFF_DATE |is.na(OTHER_AB_DATE))
#convert all NA's in OTHER_AB to 0 (0= NOT AB treated)
df11$OTHER_AB[is.na(df11$OTHER_AB)] = 0
# drop duplicates if multiple AB treatment
df12 <- df11 %>% 
  arrange(DYR_ID, KONTROLDATO, desc(OTHER_AB_DATE)) %>%
  distinct(DYR_ID, KONTROLDATO, .keep_all = TRUE)



rm(df6, df7, df8, df9, df10, df11, dryoff_treat, other_treat, teat_treat); gc()



#----------------------------------------------------------------
# PCR tests

## df11: +pcr
df13 <- left_join(df12, pcr, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)

# keep only PCR pre calving and maximum 36 days before treatment
df13 <- df13 %>% 
  filter(PCR_DATE < DRYTREAT_DATE |is.na(PCR_DATE)) %>%
  filter(PCR_DATE + 50 > DRYTREAT_DATE |is.na(PCR_DATE)) # regulations treat: 35 days after test
#convert all NA's in PCR_TEST to 0 (0= NOT AB treated)
df13$PCR_TEST = factor(df13$PCR_TEST, levels=c(levels(df13$PCR_TEST), 0))
df13$PCR_TEST[is.na(df13$PCR_TEST)] = 0

dplyr::n_distinct(df13$DYR_ID)   # 133965
dplyr::n_distinct(df13$BES_ID)   # 2645

df_all_dates <- df13
rm(pcr, df12, df13); gc()


#-------------------------------------------------------------------------------------
# final data preparation:

# create logSCC and IMI post calving:
df_all_dates <- df_all_dates %>%
  mutate(logSCC = log(SCC)) %>%
  mutate(logSCCpost = log(SCCpost)) %>%
  mutate(IMI = case_when(SCCpost < 200 ~ 0, SCCpost >= 200 ~ 1)) %>%
  mutate(IMI = factor(IMI))

df_curve <- df_curve %>%
  mutate(logSCC = log(SCC)) %>%
  mutate(logSCCpost = log(SCCpost)) %>%
  mutate(IMI = case_when(SCCpost < 200 ~ 0, SCCpost >= 200 ~ 1)) %>%
  mutate(IMI = factor(IMI))


# df_curve & df_pcr: change all parities >3 to 4, and keep only 2,3,4:
df_curve <- df_curve %>% 
  filter(PARITY > 1) %>%
  mutate(PARITY = replace(PARITY, PARITY > 3, 4)) %>%
  mutate(PARITY = factor(PARITY)) 
 
df_all <- df_all_dates %>% 
  filter(PARITY > 1) %>%
  mutate(PARITY = replace(PARITY, PARITY > 3, 4)) %>%
  mutate(PARITY = factor(PARITY)) 


rm(df_all_dates); gc()

str(df_curve)
str(df_all)

dplyr::n_distinct(df_curve$DYR_ID)   # 291.991
dplyr::n_distinct(df_curve$BES_ID)   # 2834

dplyr::n_distinct(df_all$DYR_ID)   # 72.299
dplyr::n_distinct(df_all$BES_ID)   # 2507

#------------------------------------------------------------------------
# saving data:

# df_curve: full lactation curve incl. IMI post, but no treatments and No +5 DIM cut off
# df_all: full lacation


save.image("M:/PCR_data/PCR_merge1.RData") # merged data with dryoff dates


#------------------------------------------------------------------
# preparation to be concidered pre modelling:

# removing first 5 DIM (not saved. Do this in model/descriptives scripts)
df_pcr <- df_pcr %>%
  filter(DIM > 5)

df_curve <- df_curve %>%
  dplyr::select(DYR_ID, BES_ID, HERDTYPE, BREED, PARITY,
                SCC, logSCC, IMI, MILK)






