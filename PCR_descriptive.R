
# Maj Beldring Henningsen, majbh@sund.ku.dk
# PCR - population description and basic statistics

#-------------------------------------------------------
# Packages and settings:

library(tidyverse)
library(lubridate) # for date wrangling
Sys.setlocale("LC_ALL","English") # for date formats

#-------------------------------------------------------
# Loading data:

load("M:/data/PCR_merge.RData")
load("M:/data/PCR_clean.RData")

#----------------------------------------------------------
# notes:

# use table to check:
# table(df$breed, df$sex)

#---------------------------------------------------------
# count population:

dplyr::n_distinct(production$DYR_ID)  # 2.521.615 unique DYR_ID production
dplyr::n_distinct(calvings$DYR_ID)    # 2.587.261 unique DYR_ID calvings
dplyr::n_distinct(dryoff$DYR_ID)      # 1.391.660 unique DYR_ID dryoff
dplyr::n_distinct(breed$DYR_ID)       # 6.584.677 unique DYR_ID breed
dplyr::n_distinct(pcr$DYR_ID)         # 459.337   unique DYR_ID pcr
dplyr::n_distinct(treatments$DYR_ID)  # 817.747   unique DYR_ID treatments

# check unique BES_ID
dplyr::n_distinct(production$BES_ID)  # 3933 unique BES_ID production
dplyr::n_distinct(df_pcr$BES_ID)        # 1474 unique BES_ID herd


#---------------------------------------------------------
# popualation numbers, outcome variable and predictor variables:

# count average SCC1, SCC2, tSCC and MILK1
summary(df_pcr)

# DYR_ID and BES_ID:
dplyr::n_distinct(df_pcr$DYR_ID)  # 318886 DYR_ID

# animals in each parity, 2,3,4
pcr_parity1 <- df_pcr %>%
  n_distinct(DYR_ID) %>%
  n_distinct(BES_ID)

#-----------------------------------------------
# mean and CI

df_summaries1 <- df_pcr %>%
  mutate(variable = SCC) %>%
  group_by(PARITY) %>%
  summarise(Mean = mean(variable), StdDev = sqrt(var(variable)), min = min(variable), q1 = quantile(variable, prob=0.25))
df_summaries1

df_summaries2 <- df_pcr %>%
  mutate(variable = SCC) %>%
  group_by(BREED) %>%
  summarise(Mean = mean(variable), StdDev = sqrt(var(variable)), min = min(variable), q1 = quantile(variable, prob=0.25))
df_summaries2


