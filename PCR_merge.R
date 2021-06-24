
# Maj Beldring, majbh@sund.ku.dk
# UCPH, 2020

# PCR_merge: Merge cleaned and prepared data
# full data WITHOUT dryoff dates from "goldninger" data

# to do:
## include next calving data (post_calving_date); for easier joining 
## from _temp: Evaluate pcr tests (should there be a CNS coloumn?)
## from _temp: res_minor and res_major: Factor with either 0 or 1.
## include seasonal effect based on KONTROLDATO as a coloumn (e.g. winter/summer)
## create AB_LAC for all AB treatments during lac phase (and AB_dry for dry off phase)



# merge2 timeline:
#1 calving date
#2 lacation phase 
#3 dry off phase: PCR + treatment (treatments max 50 days after test / regulation max 35)
#4 IMI post calving


#-------------------------------------------------------
# Packages and settings:

library(tidyverse)
library(lubridate) # for date wrangling
memory.size()            # Checking your memory size
memory.limit()           # Checking the set limit
memory.limit(size=56000) # suggest for 64 bit: size=56000


# Loading data: 
load("M:/PCR_data/PCR_prepare.RData")
rm(majordry, minor, pcr_full); gc()
rm(dryoff); gc()



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
## df3 + df4: + calvings (create DIM + parity)

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


#------------------------------------------------------------------
# DIM

# add days in milk, 
df4$DIM <- as.Date(as.character(df4$KONTROLDATO), format="%Y-%m-%d")-
  as.Date(as.character(df4$CALVING_DATE), format="%Y-%m-%d")

df4 <- df4 %>% 
  mutate(DIM = as.numeric(DIM))

df5 <- df4 # since we skipped the dryoff step and want the same number of steps as in merge1


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
dplyr::n_distinct(df5$DYR_ID)   # 2.377.244
dplyr::n_distinct(df6$DYR_ID)   # 1.551.121
dplyr::n_distinct(df5$BES_ID)   # 3913
dplyr::n_distinct(df6$BES_ID)   # 3843 herds;

rm(df4, df5, IMI); gc()
df_curve <- df6



#-----------------------------------------------------------------------------
# prepare pcr and treatments before merging:

# 1: create branch of master data frame with only calving date and last control date:
# The last control hereby functions as the an alternative dry off date
branch <- df6 %>% 
  arrange(DYR_ID, PARITY, desc(KONTROLDATO)) %>%
  distinct(DYR_ID, PARITY, .keep_all = TRUE)
branch <- branch %>%
  dplyr::select(DYR_ID, PARITY, KONTROLDATO, CALVING_DATE)


# 2: join each treatment data set with the branch
other_treat <- left_join(branch, other_treat, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
other_treat <- other_treat %>%
  group_by(DYR_ID, PARITY) %>%
  filter(OTHER_AB_DATE > CALVING_DATE |is.na(OTHER_AB_DATE)) %>%
  filter(OTHER_AB_DATE - 400 < CALVING_DATE |is.na(OTHER_AB_DATE)) %>%
  filter(OTHER_AB_DATE >= KONTROLDATO |is.na(OTHER_AB_DATE)) %>%
  filter(OTHER_AB_DATE - 50 < KONTROLDATO |is.na(OTHER_AB_DATE)) %>%
  dplyr::select(DYR_ID, PARITY, OTHER_AB, OTHER_AB_DATE)
other_treat <- other_treat %>%
  filter(OTHER_AB == "1") %>% # keep only AB treatments 
  drop_na()
other_treat <- other_treat %>% 
  arrange(DYR_ID, PARITY, desc(OTHER_AB_DATE)) %>%
  distinct(DYR_ID, PARITY, .keep_all = TRUE)



teat_treat <- left_join(branch, teat_treat, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
teat_treat <- teat_treat %>%
  group_by(DYR_ID, PARITY) %>%
  filter(TEAT_DATE >= KONTROLDATO |is.na(TEAT_DATE)) %>%
  filter(TEAT_DATE > CALVING_DATE |is.na(TEAT_DATE)) %>%
  filter(TEAT_DATE - 400 < CALVING_DATE |is.na(TEAT_DATE)) %>%
  filter(TEAT_DATE - 50 < KONTROLDATO |is.na(TEAT_DATE)) %>%
  dplyr::select(DYR_ID, PARITY, TEAT_TREAT, TEAT_DATE)
teat_treat <- teat_treat %>%
  drop_na()



dryoff_treat <- left_join(branch, dryoff_treat, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
dryoff_treat <- dryoff_treat %>%
  group_by(DYR_ID, PARITY) %>%
  filter(DRYTREAT_DATE >= KONTROLDATO |is.na(DRYTREAT_DATE)) %>%
  filter(DRYTREAT_DATE > CALVING_DATE |is.na(DRYTREAT_DATE)) %>%
  filter(DRYTREAT_DATE - 400 < CALVING_DATE |is.na(DRYTREAT_DATE))%>%
  filter(DRYTREAT_DATE - 50 < KONTROLDATO |is.na(DRYTREAT_DATE)) %>%
  dplyr::select(DYR_ID, PARITY, DRY_TREAT, DRYTREAT_DATE)
dryoff_treat <- dryoff_treat %>%
  drop_na()
 
 
#
save.image("M:/PCR_data/PCR_merge_temp.RData") # keep this as PCR majors might changes!!!
#

  
pcr <- left_join(branch, pcr, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
pcr <- pcr %>%
  drop_na()
pcr <- pcr %>%
  filter(PCR_DATE > CALVING_DATE |is.na(PCR_DATE)) %>%
  filter(PCR_DATE - 400 < CALVING_DATE |is.na(PCR_DATE)) %>%
  filter(PCR_DATE - 50 < KONTROLDATO |is.na(PCR_DATE)) %>%
  filter(PCR_DATE + 36 > KONTROLDATO |is.na(PCR_DATE)) %>%
  dplyr::select(DYR_ID, PARITY, PCR_DATE, RES_majordry, RES_MINOR, PCR_TEST)



# 3: convert treats (and pcr tests to factors):
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


rm(branch); gc()




#-----------------------------------------------------------------------------
# Join treatments to master (df6)


# Teat sealing : df7+8: + teat_treat
df7 <- left_join(df6, teat_treat, by = c("DYR_ID", "PARITY"), sort="TRUE",allow.cartesian=TRUE)
df8 <- df7 %>% 
  arrange(DYR_ID, KONTROLDATO, desc(TEAT_DATE)) %>%
  distinct(DYR_ID, KONTROLDATO, .keep_all = TRUE)

#convert all NA's in TEAT_TREAT to 0 (0= NOT PCR tested)
df8$TEAT_TREAT = factor(df8$TEAT_TREAT, levels=c(levels(df8$TEAT_TREAT), 0))
df8$TEAT_TREAT[is.na(df8$TEAT_TREAT)] = 0

glimpse(df8) # 
dplyr::n_distinct(df8$DYR_ID)   # 1.551.121 (# 1.551.121 in df6/df_curve)
dplyr::n_distinct(df8$BES_ID)   # 3843 (same as in df6)


# clean up:
rm(df6, df7, teat_treat, branch); gc()




#-----------------------------------------------------------------------------
# Treatment at dry-off: 

## df9+10: + dryoff_treat
df9 <- left_join(df8, dryoff_treat, by = c("DYR_ID", "PARITY"), sort="TRUE",allow.cartesian=TRUE)
# keep only treatments in dry-off period
df10 <- df9 %>% 
  arrange(DYR_ID, KONTROLDATO, desc(DRYTREAT_DATE)) %>%
  distinct(DYR_ID, KONTROLDATO, .keep_all = TRUE)

#convert all NA's in DRY_TEST to 0 (0= NOT PCR tested)
df10$DRY_TREAT = factor(df10$DRY_TREAT, levels=c(levels(df10$DRY_TREAT), 0))
df10$DRY_TREAT[is.na(df10$DRY_TREAT)] = 0
glimpse(df10) # 
dplyr::n_distinct(df10$DYR_ID)   # 1.551.121
dplyr::n_distinct(df10$BES_ID)   # 3843



# clean up:
rm(df8, df9, dryoff_treat); gc()

save.image("M:/PCR_data/PCR_merge2_temp2.RData") 


#--------------------------------------------------------------------------------
# Other AB treatments during dry-off:

## df11+12: + other_treat
df11 <- left_join(df10, other_treat, by = c("DYR_ID", "PARITY"), sort="TRUE",allow.cartesian=TRUE)

df12 <- df11 # so mathicng rest of script (had a distinct step here not needed)
 
#convert all NA's in OTHER_AB to 0 (0= NOT AB treated)
df12$OTHER_AB = factor(df12$OTHER_AB, levels=c(levels(df12$OTHER_AB), 0))
df12$OTHER_AB[is.na(df12$OTHER_AB)] = 0


dplyr::n_distinct(df12$DYR_ID)   # 1.551.121 (no data loss)
dplyr::n_distinct(df12$BES_ID)   # 3843


rm(df10, df11, other_treat); gc()


#----------------------------------------------------------------
# PCR tests

## df11: +pcr
df13 <- left_join(df12, pcr, by = c("DYR_ID", "PARITY"), sort="TRUE",allow.cartesian=TRUE)

df13 <- df13 %>% 
  arrange(DYR_ID, KONTROLDATO, desc(PCR_DATE)) %>%
  distinct(DYR_ID, KONTROLDATO, .keep_all = TRUE)


#convert all NA's in PCR_TEST to 0 (0= NOT AB treated)
df13$PCR_TEST = factor(df13$PCR_TEST, levels=c(levels(df13$PCR_TEST), 0))
df13$PCR_TEST[is.na(df13$PCR_TEST)] = 0

dplyr::n_distinct(df13$DYR_ID)   # 1.551.121
dplyr::n_distinct(df13$BES_ID)   # 3843

df_pcr <- df13
rm(pcr, df12, df13); gc()


#-------------------------------------------------------------------------------------
# final data 

# create logSCC
df_pcr <- df_pcr %>%
  mutate(logSCC = log(SCC)) %>%
  mutate(logSCCpost = log(SCCpost))

df_curve <- df_curve %>%
  mutate(logSCC = log(SCC)) %>%
  mutate(logSCCpost = log(SCCpost))

# convert SCCpost to IMI or not, cut off 200.000:
df_curve <- df_curve %>%
  mutate(IMI = case_when(SCCpost < 200 ~ 0, SCCpost >= 200 ~ 1)) %>%
  mutate(IMI = factor(IMI))

df_pcr <- df_pcr %>%
  mutate(IMI = case_when(SCCpost < 200 ~ 0, SCCpost >= 200 ~ 1)) %>%
  mutate(IMI = factor(IMI))



# df_model: remove date variables and keep Parity 2,3,4+:
df_model <- df_pcr %>%
  dplyr::select(DYR_ID, BES_ID, HERDTYPE, BREED, PARITY, DIM, 
                SCC, logSCC, SCCpost, IMI, MILK, 
                PCR_TEST, RES_MAJORDRY, RES_MINOR, DRY_TREAT, OTHER_AB, TEAT_TREAT)

df_model <- df_model %>%
  filter(logSCC > 0) %>%
  filter(PARITY > 1)

df_model <- df_model %>% 
  mutate(
  PARITY = as.numeric(PARITY),
  PARITY = replace(PARITY, PARITY > 3, 4),
  PARITY = as.factor(PARITY))

df_model <- df_model %>%
  mutate(BREED = factor(BREED)) %>%
  mutate(HERDTYPE = factor(HERDTYPE))

# create with herds with min 50 animals and 200 obs.
df_model <- df_model %>%
  group_by(BES_ID) %>%
  filter(n() > 200)

# df_model <- df_model %>%
#   filter(length(BES_ID) > 50)

df_model <- df_model %>%
  mutate(BES_ID = factor(BES_ID))
# don't do it with DYR_ID. Not needed for now. Data must be smaller for this


#------------------------------------------------------------------------
# saving data:

# df_curve: df6 ; full lactation curve including IMI post calving (no +5 DIM cut off)
# df_pcr: full lacation

save.image("M:/PCR_data/PCR_merge.RData") 


#------------------------------------------------------------------
# preparation to be applied in modelling scripts

# removing first 5 DIM (not saved. Do this in model/descriptives scripts)
df_pcr <- df_pcr %>%
  filter(DIM > 5)


# convert BES_ID and DR_ID to factors
df_model <- df_model %>%
  mutate(BES_ID = factor(BES_ID)) %>%
  mutate(DYR_ID = factor(DYR_ID)) %>%

  
df_curve <- df_curve %>%
  dplyr::select(DYR_ID, BES_ID, DIM, HERDTYPE, BREED, PARITY,
                SCC, logSCC, IMI, MILK)

# create with herds with min 50 animals and 200 obs.
df_model1 <- df_all %>%
  group_by(BES_ID, DYR_ID) %>%
  filter(n() > 200)

# create coloumn counting herd occurences
df <- df %>%
  group_by(BES_ID, PARITY) %>%
  mutate(count = n())




