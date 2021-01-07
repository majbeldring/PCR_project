
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
load("M:/PCR_data/PCR_merge.RData")
rm(production, breed, calvings, dryoff, herd)
gc()

#---------------------------------------------------------




#------------------------------------------------------------
# save cleaned data:

save.image("M:/data/PCR_dryoff.RData")
gc()



