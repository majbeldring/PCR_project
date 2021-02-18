
# Maj Beldring, majbh@sund.ku.dk
# UCPH, 2020

# PCR_misc
# various testing for PCR project

#-------------------------------------------------------
# Packages and settings:

library(tidyverse)
library(lubridate) # for date wrangling
#Sys.setlocale("LC_ALL","English") # for date formats
memory.size()            # Checking your memory size
memory.limit()           # Checking the set limit
memory.limit(size=56000) # suggest for 64 bit

options(stringsAsFactors = FALSE) # prevent factorizing caracters

# load("M:/PCR_data/PCR_prepare.RData")


#------------------------------------------------------------------------
# checking individual animals in data:

dplyr::n_distinct(production$DYR_ID)  # 2.521.615 unique DYR_ID production
dplyr::n_distinct(calvings$DYR_ID)    # 2.587.261 unique DYR_ID calvings
dplyr::n_distinct(dryoff$DYR_ID)      # 1.391.660 unique DYR_ID dryoff
dplyr::n_distinct(breed$DYR_ID)       # 6.584.677 unique DYR_ID breed
dplyr::n_distinct(pcr$DYR_ID)         # 459.337   unique DYR_ID pcr
dplyr::n_distinct(treatments$DYR_ID)  # 817.747   unique DYR_ID treatments
dplyr::n_distinct(df_curve$DYR_ID)    # 1.247.089   unique DYR_ID treatments
dplyr::n_distinct(df11$DYR_ID)        # 188.948   unique DYR_ID treatments

# check unique BES_ID
dplyr::n_distinct(production$BES_ID)  # 3933 unique BES_ID production
dplyr::n_distinct(df_pcr$BES_ID)      # 1474 unique BES_ID herd
dplyr::n_distinct(herd$BES_ID)        # 2641 unique BES_ID herd

# check NA in data:
#df1_na <- sapply(df1, function(x) sum(is.na(x)))

# check for NA after joining:
# df_pcr %>%
#   select(everything()) %>%  # replace to your needs
#   summarise_all(funs(sum(is.na(.))))

#--------------------------------------------------------
# fixed lksygdomskode: (made by Matt)

# errors do to double quotes and commas inside quotes
# loading disease codings...
textfile <- readLines("lksygdomskode.csv")
textfile <- gsub('\"\"', "'", textfile)
textfile <- gsub('\"', "", textfile)
textfile <- gsub("\'", '"', textfile)
cat(textfile, sep="\n", file="lksygdomskode_fixed.csv")

lksygdomskode <- read.csv("lksygdomskode_fixed.csv")

#-------------------------------------------------------
# create diagnose sheet for Søren:

# Create treatment data to check for all AB treatments (for Søren & Jeanette)
sundhed       <- read_csv("M:/data/sundhed.csv") 
lksygdomskode <- read.csv("M:/data/lksygdomskode_fixed.csv") #debugged prior loading

lksygdomskode %>% mutate(LKSK_ID = ID)
treatments <- left_join(sundhed, lksygdomskode %>% mutate(LKSK_ID = ID) , by = "LKSK_ID") %>%
  count(LKSYGKODE, LKSYGTEKST) %>%
  arrange(desc(n)) %>%
  mutate(UsesAB = NA_character_)
write_excel_csv2(treatments, "forsoeren.csv") #saving as write_excel prevents ?,?,? to be changed 

rm(sundhed, lksygdomskode); gc()

#-------------------------------------------------------
# other AB treatments - AB diagnoses identified by Jeanette and Søren

AB_treatments <- read_csv("M:/PCR_data/AB_treatments.csv")

# include a coloumn to lksygdomskode (from PCR_clean) with AB treatment or not
# AB_diseases, where 0= no AB, 1= AB or NA: Created from Jeanette and Sørens definition
AB_treatments <- AB_treatments %>%
  dplyr::select(-LKSYGTEKST, -n, -UsesAB, -UsesAB2, -Comment)
lksygdomskode <- full_join(lksygdomskode, AB_treatments, by = "LKSYGKODE", sort="TRUE",allow.cartesian=TRUE) %>% 
  dplyr::slice(1:241)

#---------------------------------------------------------
# HERD TYPE testing

brugsart      <- read_csv("M:/data/brugsart.csv")
brugsartkode  <- read_csv("M:/data/brugsartkode.csv")

dplyr::n_distinct(brugsart$BES_ID)      # 3944 unique BES_ID herd
dplyr::n_distinct(herd$BES_ID)          # 3935 unique BES_ID herd
dplyr::n_distinct(df1$BES_ID)           # 3933

brugsart <- brugsart %>%
  rename(ID = BRUGSART_ID) %>%
  group_by(DATO_TIL) %>% 
  replace_na(list(DATO_TIL = as.Date("2020-06-01")))

brugsartkode <- dplyr::filter(brugsartkode, grepl('lk', BRUGSARTTEKST)) # keep only milk herds
brugsartkode <- brugsartkode %>%
  dplyr::select(ID, BRUGSARTTEKST) %>%
  rename(HERD_TYPE = BRUGSARTTEKST) %>%
  mutate(HERD_TYPE = if_else(str_detect(HERD_TYPE, pattern = "logisk$"), "eco", HERD_TYPE)) %>%
  mutate(HERD_TYPE = if_else(str_detect(HERD_TYPE, pattern = "lk"), "con", HERD_TYPE))

# create herd dataset by joining code and brugsart:
herd <- left_join(brugsart, brugsartkode , by = "ID") %>%
  dplyr::select(-ID) %>%
  drop_na() %>%
  mutate(HERD_TYPE = if_else(str_detect(HERD_TYPE, pattern = "logisk$"), "eco", HERD_TYPE)) %>%
  mutate(HERD_TYPE = if_else(str_detect(HERD_TYPE, pattern = "lk"), "con", HERD_TYPE)) %>%
  mutate(HERD_TYPE = factor(HERD_TYPE))

# merge production and herd type
df1 <- left_join(production, herd, by = "BES_ID", sort="TRUE",allow.cartesian=TRUE)
df1 <- df1 %>% 
  filter(DATO_FRA <= KONTROLDATO) %>% 
  filter(DATO_TIL >= KONTROLDATO) %>%
  dplyr::select(-DATO_FRA, -DATO_TIL)

gc()

#-------------------------------------------------------
# Major pathogens from vetpcr:

# keeping only pcr pathogens. Recall: agalactiae = B.strep
major4 <- dplyr::filter(vetpcr, grepl('aureus|uberis|dysgalactiae|B-strep', PATHOGEN)) 

major4 <- major4 %>% 
  mutate(PATHOGEN = if_else(str_detect(PATHOGEN, pattern = "dysgalactiae$"), "s.dys", PATHOGEN)) %>%
  mutate(PATHOGEN = if_else(str_detect(PATHOGEN, pattern = "uberis$"), "s.uberis", PATHOGEN)) %>%
  mutate(PATHOGEN = if_else(str_detect(PATHOGEN, pattern = "aureus$"), "s.aureus", PATHOGEN)) %>%
  mutate(PATHOGEN = if_else(str_detect(PATHOGEN, pattern = "B-strep$"), "B.strep", PATHOGEN)) %>% 
  relocate(DYR_ID, PCR_DATE, PCR_VALUE, PATHOGEN)

# create coloumn with PATOGEN levels as numbers
major4 <- major4 %>% mutate(PATHOGEN_BI = case_when(PATHOGEN == "s.dys" ~ 1,
                                                    PATHOGEN == "s.uberis" ~ 2,
                                                    PATHOGEN == "s.aureus" ~ 3,
                                                    PATHOGEN == "B.strep" ~ 4))

major4 <- major4 %>% 
  mutate(PATHOGEN = factor(PATHOGEN)) %>%
  mutate(PATHOGEN_BI = factor(PATHOGEN_BI)) %>%
  add_column(PCR_test = 1) %>%
  mutate(PCR_test = factor(PCR_test))

# create pcr data with only 1 PCR_VALUE per animal per test date. Keeping only the lowest value, as the lower the more POS, 
major4 <- major4 %>% 
  dplyr::select(DYR_ID, PCR_test, PCR_DATE, PCR_VALUE, PATHOGEN, PATHOGEN_BI) %>%
  arrange(DYR_ID, PCR_DATE, PCR_VALUE) %>%
  distinct(DYR_ID, PCR_DATE, .keep_all = TRUE) %>%
  mutate(RES_MAJOR = case_when(PCR_VALUE < 37 ~ 1, PCR_VALUE >= 37 ~ 0)) %>%
  dplyr::select(DYR_ID, PCR_test, PCR_DATE, RES_MAJOR, PATHOGEN, PATHOGEN_BI) %>%
  mutate(RES = factor(RES))


#-------------------------------------------------------
# loading UTF-8: problems with Jeanttes diagnoses
# following doesn't work

#Sys.setlocale("LC_ALL","Danish") # for date formats
vetpcrkode    <- read_csv("M:/data/vetpcrkode.csv")
Encoding(vetpcrkode$NAVNKORT) <- "UTF-8"

# vetpcrkode: rename Gær
str(vetpcrkode)
unique(vetpcrkode$NAVNKORT)

vetpcrkode <- vetpcrkode %>% 
  mutate(NAVNKORT = if_else(str_detect(NAVNKORT, pattern = "Alger"), "algies", NAVNKORT)) %>%
  mutate(NAVNKORT = if_else(str_detect(NAVNKORT, pattern = "G\xe6r"), "yeast", NAVNKORT)) %>% 
  mutate(NAVNKORT = factor(NAVNKORT)) %>%
  group_by(NAVNKORT) %>%
  sapply(levels)

test <- vetpcr %>% recode(PATHOGEN, "Gær" = "yeast", .default = levels(PATHOGEN))
test <- mutate(vetpcrkode, NAVNKORT = fct_recode(NAVNKORT, "G\xe6r" = "yeast"))

unique(vetpcr$PATHOGEN) # there are both Alger and Yeast (G\xe6r) among pathogens

test %>% 
  group_by(PATHOGEN) %>%
  sapply(levels)

#------------------------------------------------------------
# calculate the DIM in PCR_merge with df3:

# df3$DIM <- as.period(interval(df3$KONTROLDATO, df3$CALVING_DATE)) # not working

# DIM - add days in milk, 
df4$DIM <- as.Date(as.character(df4$KONTROLDATO), format="%Y-%m-%d")-
  as.Date(as.character(df4$CALVING_DATE), format="%Y-%m-%d")

# drop duplicates with overlapping DIM
df4 <- df4 %>% 
  arrange(DYR_ID, KONTROLDATO, desc(CALVING_DATE)) %>%
  distinct(DYR_ID, KONTROLDATO, .keep_all = TRUE)

#----------------------------------------------------------
# create logSCC and tSCC and logtSCC
df13 <- df12 %>%
  mutate(tSCC = SCC * MILK) %>%
  mutate(tSCCpre = SCCpre * MILKpre) %>%
  mutate(logSCC = log(SCC)) %>%
  mutate(logSCCpre = log(SCCpre)) %>%
  mutate(logtSCCpre = log(tSCCpre)) %>%
  mutate(logtSCC = log(tSCC))

#-----------------------------------------------------------
# pre coloumns, load PCR_merge; use df_curve

# SCCpre coloumn
df_SCC <- df_curve %>%
  group_by(DYR_ID, PARITY) %>%
  summarise(SCCpre = last(SCC)) %>%
  mutate(SCCpre = lag(SCCpre)) %>%
  left_join(df_curve, by = c('DYR_ID', 'PARITY')) %>%
  group_by(DYR_ID, PARITY) %>%
  mutate(SCCpre = replace(SCCpre, -1, NA)) %>%
  dplyr::select(-SCCpre, SCCpre)

# create the MILKpre coloumn
df_milk <- df_curve %>%
  group_by(DYR_ID, PARITY) %>%
  summarise(MILKpre = last(MILK)) %>%
  mutate(MILKpre = lag(MILKpre)) %>%
  left_join(df_curve, by = c('DYR_ID', 'PARITY')) %>%
  group_by(DYR_ID, PARITY) %>%
  mutate(MILKpre = replace(MILKpre, -1, NA)) %>%
  dplyr::select(-MILKpre, MILKpre)

#------------------------------------------------------------
# save cleaned data:

#save.image("M:/data/PCR_misc.RData")
gc()



