
# Maj Beldring, majbh@sund.ku.dk
# UCPH, 2020

# PCR_merge: Merge cleaned and prepared data
# Script #3 in PCR project

# Version 3 of merging...

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
  mutate(HERDTYPE = case_when(HERD_TYPE == 'con' ~ 1, HERD_TYPE == 'eco' ~ 0)) %>%
  dplyr::select(-HERD_TYPE)

glimpse(df1) # HERDTYPE: 1=con, 2=eco
dplyr::n_distinct(production$DYR_ID)  # 2.521.615 unique DYR_ID production
dplyr::n_distinct(df1$DYR_ID)         # 2.520.295 unique DYR_ID production
# df1: No significant data loss

rm(herd, production); gc()

#----------------------------------------------------------
## df2: + breed

df2 <- full_join(df1, breed, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
df2 <- df2 %>% 
  drop_na() %>%
  dplyr::select(-BREED) %>%
  rename(BREED = BREED_BI)

glimpse(df2) # BREED: 1=Holstein, 2=Jersey, 3= other
dplyr::n_distinct(df1$DYR_ID)   # 2.520.295 
dplyr::n_distinct(df2$DYR_ID)   # 2.518.572
# df2: No significant data loss

rm(df1, breed); gc()


#-----------------------------------------------------------------------
# joining production & calvings -> df1 (all parities are included here)

df1 <- full_join(production, calvings, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
df1_na <- sapply(df1, function(x) sum(is.na(x)))
dplyr::n_distinct(df1$DYR_ID)  # 2.709.849 unique DYR_ID production
df1 <- df1 %>% drop_na()


# df2: full lactation phase for descriptive: parity, breed, SCC, PCR, treatment, DIM
df2 <- df1 %>% 
  filter(CALVING_DATE < KONTROLDATO) %>%
  filter(KONTROLDATO - 310 < CALVING_DATE)

# df2: Add days in milk, DIM
df2$DIM <- as.Date(as.character(df2$KONTROLDATO), format="%Y-%m-%d")-
  as.Date(as.character(df2$CALVING_DATE), format="%Y-%m-%d")

# df3: drop duplicates where DIM < 310
df3 <- df2 %>% 
  arrange(DYR_ID, KONTROLDATO, desc(CALVING_DATE)) %>%
  distinct(DYR_ID, KONTROLDATO, .keep_all = TRUE)


# df4: add breed to df3:
df4 <- full_join(df3, breed, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
df4 <- df4 %>% drop_na() # loosing 430.000 observations from df4 - mainly cattle without production data

# df_curve: copy df5 to df_curve; which include all SCC and milk values for curve analysis
df_curve <- df4

# df5: removing first 5 days in milk from df4:
df5 <- df4 %>%
  filter(DIM > 5) 

rm(df1, df2, df3, df4, breed, calvings,production, vetpcr, dryoff, herd)

# df6: create the SCCpre coloumn
df6 <- df5 %>%
  group_by(DYR_ID, PARITY) %>%
  summarise(SCCpre = last(SCC)) %>%
  mutate(SCCpre = lag(SCCpre)) %>%
  left_join(df5, by = c('DYR_ID', 'PARITY')) %>%
  group_by(DYR_ID, PARITY) %>%
  mutate(SCCpre = replace(SCCpre, -1, NA)) %>%
  select(-SCCpre, SCCpre)

# df7: create new dataframe with MILKpre
df7 <- df6 %>%
  group_by(DYR_ID, PARITY) %>%
  summarise(MILKpre = last(MILK)) %>%
  mutate(MILKpre = lag(MILKpre)) %>%
  left_join(df6, by = c('DYR_ID', 'PARITY')) %>%
  group_by(DYR_ID, PARITY) %>%
  mutate(MILKpre = replace(MILKpre, -1, NA)) %>%
  select(-MILKpre, MILKpre)

# df8: rename coloumns and remove NAs, so only 1 SCC for each DYR and DATE is listed
df8 <- df7 %>% drop_na()

rm(df6, df7)
gc()

# merge vetpcr and treatments before joining with df8
df8a <- full_join(pcr, treatments, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
df8b <- df8a %>% 
  select(-DISEASE, -RES) %>%
  rename(TREATED = DISEASE_BI, RESULT = RES_BI)

# replace NA values in TREATED with 2=NO (1=YES already applied)
df8c <- df8b %>% mutate(
  TREATED = as.character(TREATED),
  TREATED = ifelse(is.na(TREATED), 2, TREATED),
  TREATED = as.factor(TREATED)
)

# keep only PCR tests from max 35 days before treatment
df8d <- df8c %>% 
  filter(PCR_DATE < TREATMENT_DATE |is.na(TREATMENT_DATE)) %>%
  filter(TREATMENT_DATE - 36 < PCR_DATE |is.na(TREATMENT_DATE)) %>% 
  select(-TREATMENT_DATE)

# df9: df8 + df8d, keep only PCR dates before calving data and max 150 days before (ideally 95 days)
df9 <- full_join(df8, df8d, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
df_all <- df9 # keep data with DYR_ID not PCR tested: Duplciated PCR, handle like TREATMENT DATES df8d

rm(df8, df8a, df8b, df8c, df8d)

# keep only PCR tests before calving, and max 150 days before
df10 <- df9 %>% 
  drop_na() %>% # remove DYR_ID not PCR tested
  filter(PCR_DATE < CALVING_DATE) %>%
  filter(CALVING_DATE - 150 < PCR_DATE) %>% 
  select(-PCR_DATE)

rm(df9)
rm(df_pathogen)
# count BES_ID before removing in df11

# df11: final cleaning on df10
df11 <- df10 %>%
  mutate(C_MONTH = lubridate::month(CALVING_DATE)) %>%
  select(-BIRTH, -KONTROLDATO, - CALVING_DATE, -BREED, -DIM) %>%
  rename(BREED = BREED_BI) 

# change all parities >3 to 4
df12 <- df11 %>% mutate(
    PARITY = as.numeric(PARITY),
    PARITY = replace(PARITY, PARITY > 3, 4),
    PARITY = as.factor(PARITY)
  )

rm(df10, df11, pcr, treatments)
  
# must redo this step: df_pcr has not logtSCC included
# create logSCC and tSCC and loftSCC
df13 <- df12 %>%
  mutate(tSCC = SCC * MILK) %>%
  mutate(tSCCpre = SCCpre * MILKpre) %>%
  mutate(logSCC = log(SCC)) %>%
  mutate(logSCCpre = log(SCCpre)) %>%
  mutate(logtSCCpre = log(tSCCpre)) %>%
  mutate(logtSCC = log(tSCC))

# redo this step as well with select - now I have 2 MILK in my final
# relocate, so coloumns are ordered
#df14 <- df13 %>%
#   select(DYR_ID, )
# df14 <- df13[, c(1, 2, 11, 5, 4, 6, 3, 15, 14, 13, 12, 16, 7, 8, 9, 10)]

df_pcr <- df14

rm(df12, df13, df14) 

#----------------------------------------------------------------------------------------

save.image("M:/data/PCR_merge.RData") 

