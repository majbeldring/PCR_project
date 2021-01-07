
# Maj Beldring, majbh@sund.ku.dk
# UCPH, 2020

# PCR_prepare: Preparing cleaned data for merging
# Script #2 in PCR project

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
  mutate(BREED = if_else(str_detect(BREED, pattern = "Holstein"), "holstein", BREED)) %>%
  mutate(BREED = if_else(str_detect(BREED, pattern = "Jersey"), "jersey", BREED)) %>%
  mutate(BREED = if_else(str_detect(BREED, pattern = "broget"), "other", BREED)) %>%
  mutate(BREED = if_else(str_detect(BREED, pattern = "alkerace$"), "other", BREED)) %>%
  mutate(BREED = if_else(str_detect(BREED, pattern = "Krydsning"), "other", BREED))

breed <- dplyr::filter(breed, grepl('holstein|jersey|other', BREED)) #keep only 3

# add coloumn to breed wit numerious values:
breed <- breed %>% 
  mutate(BREED_BI = case_when(BREED == "holstein" ~ 1, 
                              BREED == "jersey" ~ 2, 
                              BREED == "other" ~ 3)) %>%
  mutate(BREED = factor(BREED)) %>%
  mutate(BREED_BI = factor(BREED_BI))

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
  rename(MAJOR = PATHOGEN) %>%
  mutate(MAJOR = factor(MAJOR))

# create pcr data with only 1 PCR_VALUE per animal per test date. Keeping only the lowest value, as the lower the more POS, 
major <- major %>% 
  dplyr::select(DYR_ID, PCR_DATE, PCR_VALUE, MAJOR) %>%
  arrange(DYR_ID, PCR_DATE, PCR_VALUE) %>%
  distinct(DYR_ID, PCR_DATE, .keep_all = TRUE) %>%
  mutate(RES_MAJOR = case_when(PCR_VALUE < 37 ~ 1, PCR_VALUE >= 37 ~ 0)) %>%
  dplyr::select(DYR_ID, PCR_DATE, RES_MAJOR, MAJOR) %>%
  mutate(RES_MAJOR = factor(RES_MAJOR))

glimpse(major)
dplyr::n_distinct(major$DYR_ID)  # 459.337 unique DYR_ID pcr (one less than in vetpcr)

#-------------------------------------------------------------------------------
# vetpcr- > minor : PCR result for non of the four major pathogens

minor <- dplyr::filter(vetpcr, !grepl('aureus|uberis|dysgalactiae|B-strep', PATHOGEN)) 

minor <- minor %>% 
  rename(MINOR = PATHOGEN) %>%
  mutate(MINOR = factor(MINOR))

# create pcr data with only 1 PCR_VALUE per animal per test date. Keeping only the lowest value, as the lower the more POS, 
minor <- minor %>% 
  dplyr::select(DYR_ID, PCR_DATE, PCR_VALUE, MINOR) %>%
  arrange(DYR_ID, PCR_DATE, PCR_VALUE) %>%
  distinct(DYR_ID, PCR_DATE, .keep_all = TRUE) %>%
  mutate(RES_MINOR = case_when(PCR_VALUE < 37 ~ 1, PCR_VALUE >= 37 ~ 0)) %>%
  dplyr::select(DYR_ID, PCR_DATE, RES_MINOR, MINOR) %>%
  mutate(RES_MINOR = factor(RES_MINOR))

glimpse(minor)
dplyr::n_distinct(minor$DYR_ID)  # 459197 unique DYR_ID pcr 

#-------------------------------------------------------------------------------
# merge major and minor

pcr <- inner_join(major, minor, sort="TRUE",allow.cartesian=TRUE)
pcr <- pcr %>%
  add_column(PCR_TEST = 1) %>%
  mutate(PCR_TEST = factor(PCR_TEST))

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
  mutate(PATHOGEN = factor(PATHOGEN))  %>%
  add_column(PCR_test = 1) %>%
  mutate(PCR_test = factor(PCR_test))

# create POS/NEG coloumn
pcr_full <- pcr_full %>% 
  dplyr::select(DYR_ID, PCR_test, PCR_DATE, PCR_VALUE, PATHOGEN) %>%
  mutate(RES = case_when(PCR_VALUE < 37 ~ 1, PCR_VALUE >= 37 ~ 0)) %>%
  dplyr::select(DYR_ID, PCR_test, PCR_DATE, RES, PATHOGEN) %>%
  mutate(RES = factor(RES))


# pcr_major, pcr_minor, pcr_all replaces vetpcr
rm(vetpcr)
gc()

#------------------------------------------------------------------
# Treatments; dryoff, Teat sealing, other AB treatments

# DRYOFF treatments, with 1 for treatment:
dryoff_treat <- dplyr::filter(treatments, grepl('Goldningsbehandling', DISEASE))
dryoff_treat <- dryoff_treat %>% 
  mutate(DRY_TREAT = case_when(DISEASE == "Goldningsbehandling" ~ 1)) %>% 
  mutate(DRY_TREAT = factor(DRY_TREAT)) %>%
  rename(DRYTREAT_DATE = TREATMENT_DATE) %>%
  dplyr::select(-DISEASE, -AB)

# TEAT SEALANT, with 1 for teat treated
teat_treat <- dplyr::filter(treatments, grepl('pattelukning', DISEASE))
teat_treat <- teat_treat %>% 
  mutate(TEAT_TREAT = case_when(DISEASE == "Intern pattelukning" ~ 1)) %>% 
  mutate(TEAT_TREAT = factor(TEAT_TREAT)) %>%
  rename(TEAT_DATE = TREATMENT_DATE) %>%
  dplyr::select(-DISEASE, -AB)

# Other AB treatments
# remove teat seal and goldningsbehandling from treatments
# rename to other_treat
other_treat <- dplyr::filter(treatments, !grepl('pattelukning|Goldningsbehandling', DISEASE))
other_treat <- other_treat %>% 
  rename(OTHER_AB_DATE = TREATMENT_DATE, OTHER_AB = AB) %>%
  mutate(OTHER_AB = factor(OTHER_AB))%>%
  dplyr::select(-DISEASE)

str(other_treat)

rm(treatments)
gc()

#------------------------------------------------------------
# save cleaned data:

save.image("M:/PCR_data/PCR_prepare.RData")

