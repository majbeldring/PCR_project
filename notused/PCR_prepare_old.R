
# Maj Beldring, majbh@sund.ku.dk
# UCPH, 2020

# PCR_prepare: Preparing cleaned data for merging
# Script #2 in PCR project

# to do:
### include klebsiella and e. coli in majors: ask Line for others

#-------------------------------------------------------
# Packages and settings:

library(tidyverse)
Sys.setlocale("LC_ALL","English") # for date formats
memory.size()            # Checking your memory size
memory.limit()           # Checking the set limit
memory.limit(size=56000) # suggest for 64 bit
options(stringsAsFactors = FALSE) # prevent factorizing caracters

#-------------------------------------------------------
# Loading data: 

load("M:/PCR_data/PCR_clean.RData")

#------------------------------------------------------
# Breed:

breed <- breed %>% 
  mutate(RACE = if_else(str_detect(RACE, pattern = "Holstein"), "holstein", RACE)) %>%
  mutate(RACE = if_else(str_detect(RACE, pattern = "Jersey"), "jersey", RACE)) %>%
  mutate(RACE = if_else(str_detect(RACE, pattern = "broget"), "other", RACE)) %>%
  mutate(RACE = if_else(str_detect(RACE, pattern = "alkerace$"), "other", RACE)) %>%
  mutate(RACE = if_else(str_detect(RACE, pattern = "Krydsning"), "other", RACE))

breed <- dplyr::filter(breed, grepl('holstein|jersey|other', RACE)) #keep only 3

# add coloumn to breed wit numerious values:
breed <- breed %>% 
  mutate(BREED = case_when(RACE == "holstein" ~ 1, 
                              RACE == "jersey" ~ 2, 
                              RACE == "other" ~ 3)) 

#-------------------------------------------------------------------------------
# vetpcr- > major : four major pathogens

str(vetpcr)

# keeping only pcr pathogens. Recall: agalactiae = B.strep
major <- dplyr::filter(vetpcr, grepl('aureus|uberis|dysgalactiae|B-strep', PATHOGEN)) 

major <- major %>% 
  mutate(PATHOGEN = if_else(str_detect(PATHOGEN, pattern = "dysgalactiae$"), "s.dys", PATHOGEN)) %>%
  mutate(PATHOGEN = if_else(str_detect(PATHOGEN, pattern = "uberis$"), "s.uberis", PATHOGEN)) %>%
  mutate(PATHOGEN = if_else(str_detect(PATHOGEN, pattern = "aureus$"), "s.aureus", PATHOGEN)) %>%
  mutate(PATHOGEN = if_else(str_detect(PATHOGEN, pattern = "B-strep$"), "B.strep", PATHOGEN)) %>% 
  relocate(DYR_ID, PCR_DATE, PCR_VALUE, PATHOGEN)

major <- major %>% 
  rename(MAJOR = PATHOGEN)

# create pcr data with only 1 PCR_VALUE per animal per test date. Keeping only the lowest value, as the lower the more POS, 
major <- major %>% 
  dplyr::select(DYR_ID, PCR_DATE, PCR_VALUE, MAJOR) %>%
  arrange(DYR_ID, PCR_DATE, PCR_VALUE) %>%
  distinct(DYR_ID, PCR_DATE, .keep_all = TRUE) %>%
  mutate(RES_MAJOR = case_when(PCR_VALUE < 37 ~ 1, PCR_VALUE >= 37 ~ 0)) %>%
  rename(PCR_MAJOR = PCR_VALUE) %>%
  dplyr::select(DYR_ID, PCR_DATE, PCR_MAJOR, RES_MAJOR, MAJOR) 

glimpse(major)
dplyr::n_distinct(major$DYR_ID)  # 459.337 unique DYR_ID pcr (one less than in vetpcr)

#-------------------------------------------------------------------------------
# vetpcr- > minor : PCR result for non of the four major pathogens

minor <- dplyr::filter(vetpcr, !grepl('aureus|uberis|dysgalactiae|B-strep', PATHOGEN)) 

minor <- minor %>% 
  rename(MINOR = PATHOGEN) 

# create pcr data with only 1 PCR_VALUE per animal per test date. Keeping only the lowest value, as the lower the more POS, 
minor <- minor %>% 
  dplyr::select(DYR_ID, PCR_DATE, PCR_VALUE, MINOR) %>%
  arrange(DYR_ID, PCR_DATE, PCR_VALUE) %>%
  distinct(DYR_ID, PCR_DATE, .keep_all = TRUE) %>%
  mutate(RES_MINOR = case_when(PCR_VALUE < 37 ~ 1, PCR_VALUE >= 37 ~ 0)) %>%
  rename(PCR_MINOR = PCR_VALUE) %>%
  dplyr::select(DYR_ID, PCR_DATE, PCR_MINOR, RES_MINOR, MINOR) 

glimpse(minor)
dplyr::n_distinct(minor$DYR_ID)  # 459197 unique DYR_ID pcr 

#-------------------------------------------------------------------------------
# merge major and minor

pcr <- inner_join(major, minor, sort="TRUE",allow.cartesian=TRUE)
pcr <- pcr %>%
  add_column(PCR_TEST = 1) 

#------------------------------------------------------------------------------
# vetpcr- > pcr_full: all pathogens from PCR tests

# keeping only pcr pathogens. Recall: agalactiae = B.strep
pcr_full <- vetpcr %>% 
  mutate(PATHOGEN = if_else(str_detect(PATHOGEN, pattern = "dysgalactiae$"), "s.dys", PATHOGEN)) %>%
  mutate(PATHOGEN = if_else(str_detect(PATHOGEN, pattern = "uberis$"), "s.uberis", PATHOGEN)) %>%
  mutate(PATHOGEN = if_else(str_detect(PATHOGEN, pattern = "aureus$"), "s.aureus", PATHOGEN)) %>%
  mutate(PATHOGEN = if_else(str_detect(PATHOGEN, pattern = "B-strep$"), "B.strep", PATHOGEN)) %>% 
  mutate(PATHOGEN = if_else(str_detect(PATHOGEN, pattern = "Gaer$"), "yeast", PATHOGEN)) %>% 
  relocate(DYR_ID, PCR_DATE, PCR_VALUE, PATHOGEN) %>%
  add_column(PCR_test = 1)

# Keep selected coloumns and create POS/NEG:
pcr_full <- pcr_full %>% 
  dplyr::select(DYR_ID, PCR_test, PCR_DATE, PCR_VALUE, PATHOGEN) %>%
  mutate(RES = case_when(PCR_VALUE < 37 ~ 1, PCR_VALUE >= 37 ~ 0)) %>%
  dplyr::select(DYR_ID, PCR_test, PCR_DATE, PCR_VALUE, RES, PATHOGEN) 


# pcr_major, pcr_minor, pcr_all replaces vetpcr
rm(vetpcr)
gc()

#------------------------------------------------------------------
# Treatments; dryoff, Teat sealing, other AB treatments

# DRYOFF treatments, with 1 for treatment:
dryoff_treat <- dplyr::filter(treatments, grepl('Goldningsbehandling', DISEASE))
dryoff_treat <- dryoff_treat %>% 
  mutate(DRY_TREAT = case_when(DISEASE == "Goldningsbehandling" ~ 1)) %>% 
  rename(DRYTREAT_DATE = TREATMENT_DATE) %>%
  dplyr::select(-DISEASE, -AB)

# TEAT SEALANT, with 1 for teat treated
teat_treat <- dplyr::filter(treatments, grepl('pattelukning', DISEASE))
teat_treat <- teat_treat %>% 
  mutate(TEAT_TREAT = case_when(DISEASE == "Intern pattelukning" ~ 1)) %>% 
  rename(TEAT_DATE = TREATMENT_DATE) %>%
  dplyr::select(-DISEASE, -AB)

# Other AB treatments
# remove teat seal and goldningsbehandling from treatments
# rename to other_treat
other_treat <- dplyr::filter(treatments, !grepl('pattelukning|Goldningsbehandling', DISEASE))
other_treat <- other_treat %>% 
  rename(OTHER_AB_DATE = TREATMENT_DATE, OTHER_AB = AB) %>%
  dplyr::select(-DISEASE)

str(other_treat)

rm(treatments)
gc()

#------------------------------------------------------------
# save cleaned data:

save.image("M:/PCR_data/PCR_prepare.RData")


