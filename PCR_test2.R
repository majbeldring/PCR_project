

# Maj Beldring, majbh@sund.ku.dk

# PCR project - test data 2
# Selected test data, organic and conventional herds with only holstein, Parity 2
# take from full data. Group by holstein and Parity. Then count each BES. Take those with most

### Only 6 farms (3 organic, 3 con)
### Only Holstein
### Only Parity 2
### Max 1000 Animals

#-----------------------------------------------------------------
# Packages and settings:
library(tidyverse)
library(data.table)
library(GGally)
library("ggpubr") # for ggscatter in correlation figure
library(rstatix) # for sample_by_n
library(gridExtra)
Sys.setlocale("LC_ALL","English") # data formatting

#--------------------------------------------------------------------
# Loading data:

load("M:/data/PCR_merge.RData")

#-------------------------------------------------------------------

df_test1 <- df_pcr %>%
  filter(PARITY == 2) %>% 
  filter(BREED_BI == 1) 


df_test12 <- df_curve %>%
  filter(PARITY == 2) %>% 
  filter(BREED_BI == 1) 


