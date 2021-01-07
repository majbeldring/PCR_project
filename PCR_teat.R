
# Maj Beldring, majbh@sund.ku.dk

# PCR_teat
# check teat treatment (both only at farms registering it, and at all farms)

# Supervisor meetng:
# ecdf plots....
# Only take farms we are sure do teat sealing...
# statify by parity and breed and herd type and then check...

# should all be part of the 

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
# Loading data

# treatments
sundhed       <- read_csv("M:/data/sundhed.csv") 
lksygdomskode <- read.csv("M:/data/lksygdomskode_fixed.csv") #debugged prior loading
# calvings
kaelvninger   <- read_csv("M:/data/kaelvninger.csv") 
# production
yktr          <- read_csv("M:/data/yktr.csv")
# dryoff
goldninger    <- read_csv("M:/data/goldninger.csv")

#-------------------------------------------------------
# cleaning & selecting only teat sealing treatments:

# Treatments -> "teat_treat"
treatments <- sundhed %>%
  filter(SYGDOMSDATO > as.Date("2009-12-31")) %>%
  dplyr::select(DYR_ID, SYGDOMSDATO, LKSK_ID) %>%
  drop_na() %>%
  rename(TREATMENT_DATE = SYGDOMSDATO, ID = LKSK_ID)

treatments <- left_join(treatments, lksygdomskode , by = "ID") %>%
  dplyr::select(DYR_ID, TREATMENT_DATE, LKSYGTEKST) %>%
  rename(DISEASE = LKSYGTEKST)


# Select only teat treatment and create new coloumn with 1 for teat treated
teat_treat <- dplyr::filter(treatments, grepl('pattelukning', DISEASE))
teat_treat <- treatments %>% 
  mutate(TEAT = case_when(DISEASE == "Intern pattelukning" ~ 1)) %>% 
  mutate(TEAT = factor(TEAT)) %>%
  rename(DATE_TEAT = TREATMENT_DATE)


# yktr -> "control"
control <- yktr %>%
  dplyr::select(DYR_ID, BES_ID, KONTROLDATO, CELLETAL, KGMAELK) %>%
  drop_na() %>%
  filter(CELLETAL > 0) %>%
  filter(KGMAELK > 0) %>%
  filter(KGMAELK < 100) %>%
  filter(KONTROLDATO > as.Date("2009-12-31")) %>%
  mutate(IMI = case_when(CELLETAL < 200 ~ 0, CELLETAL > 199 ~ 1)) %>%
  mutate(IMI = factor(IMI)) %>%
  rename(SCC = CELLETAL, MILK = KGMAELK)

# kaelvninger -> "calvings"
calvings <- kaelvninger %>%
  mutate_if(~'POSIXt' %in% class(.x), as.Date) %>% # change date format
  dplyr::select(DYR_ID, KAELVEDATO, KAELVNINGSNR) %>%
  filter(KAELVEDATO > as.Date("2009-12-31")) %>%
  drop_na() %>%
  rename(CALVING_DATE = KAELVEDATO, PARITY = KAELVNINGSNR) %>%
  mutate(PARITY = factor(PARITY))

# dry-off
dryoff <- goldninger %>%
  filter(GOLDNINGSDATO > as.Date("2009-12-31")) %>%
  dplyr::select(DYR_ID, GOLDNINGSDATO) %>%
  drop_na() %>%
  rename(DRYOFF_DATE = GOLDNINGSDATO)

#-------------------------------------------------------------------------------
# Merging:

# Notes: first 5 days in milk not excluded...

# calving + control -> misc_1:
misc_1 <- full_join(control, calvings, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
misc_1 <- misc_1 %>% drop_na()

# Full lactation phase for descriptive: parity, breed, SCC, PCR, treatment, DIM
misc_1 <- misc_1 %>% 
  filter(CALVING_DATE < KONTROLDATO) %>%
  filter(KONTROLDATO - 310 < CALVING_DATE)

# create the SCCpre coloumn
misc_1 <- misc_1 %>%
  group_by(DYR_ID, PARITY) %>%
  summarise(SCCpre = last(SCC)) %>%
  mutate(SCCpre = lag(SCCpre)) %>%
  left_join(misc_1, by = c('DYR_ID', 'PARITY')) %>%
  group_by(DYR_ID, PARITY) %>%
  mutate(SCCpre = replace(SCCpre, -1, NA)) %>%
  dplyr::select(-SCCpre, SCCpre)
# create new dataframe with MILKpre
misc_1 <- misc_1 %>%
  group_by(DYR_ID, PARITY) %>%
  summarise(MILKpre = last(MILK)) %>%
  mutate(MILKpre = lag(MILKpre)) %>%
  left_join(misc_1, by = c('DYR_ID', 'PARITY')) %>%
  group_by(DYR_ID, PARITY) %>%
  mutate(MILKpre = replace(MILKpre, -1, NA)) %>%
  dplyr::select(-MILKpre, MILKpre)

misc_1 <- misc_1 %>% drop_na() # Now we only have the first and the last day

# merge with teat_treat:
misc_2 <- full_join(misc_1, teat_treat, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
str(misc_2)

# this step took a very long time.. Didn't work if converting to characters in the beginning
misc_2 <- misc_2 %>% 
  mutate(TEAT = ifelse(is.na(TEAT), 0, TEAT)) %>%
  mutate(TEAT = as.factor(TEAT))

misc_2 <- misc_2 %>% dplyr::select(-DISEASE)

# merge with golding data:
misc_3 <- full_join(misc_2, dryoff, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)

# Keeping only relevant dryoff date, calving date + 100 
misc_4 <- misc_3 %>% 
  filter(DRYOFF_DATE < CALVING_DATE) %>%
  filter(CALVING_DATE - 100 < DRYOFF_DATE)

# keep only Teat treatment 
misc_5 <- misc_4 %>% 
  filter(TREATMENT_DATE < CALVING_DATE |is.na(TREATMENT_DATE)) %>%
  filter(TREATMENT_DATE + 5 > DRYOFF_DATE |is.na(TREATMENT_DATE))

rm(calvings, control, dryoff, goldninger, kaelvninger, treatments, yktr, misc_1, misc_2, teat_treat); gc()

df_teat <- misc_5

# log transform:
df_teat <- df_teat %>%
  mutate(logSCC = log(SCC)) %>%
  mutate(logSCCpre = log(SCCpre))

#---------------------------------------------------------------------
# table TEAT vs IMI

table_teat <- table(df_teat$TEAT, df_teat$IMI) # A = rows, B = columns
table_teat

#--------------------------------------------------------------------
# Visualizing:

# Barplot of table with percentage: ALL teat treatments:
df_teat %>% 
  group_by(TEAT) %>% 
  count(IMI) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = TEAT, y = prop)) +
  geom_col(aes(fill = IMI), position = "dodge") +
  geom_text(aes(label = scales::percent(prop), 
                y = prop, 
                group = IMI),
            position = position_dodge(width = 0.9),
            vjust = 1.5)


# boxplot:

# SCC_pre
p1 <- ggplot(df_teat, aes(x=TEAT, y=logSCCpre, col=TEAT)) +
  geom_boxplot() +
  xlab('Teat treated') +
  ylab('logSCC pre calving') +
  labs(title = "SCCpre")

# SCC_pre
p2 <- ggplot(df_teat, aes(x=TEAT, y=logSCC, col=TEAT)) +
  geom_boxplot() +
  xlab('Teat treated') +
  ylab('logSCC post calving') +
  labs(title = "SCC post")

grid.arrange(p1, p2, ncol=2) # SCCpre & SCCpost vs TEAT treatment

# straitfy by parity:


#--------------------------------------------------------------------
# logistic regression with df_teat

# when scc is log transformed, do this:
glm_teat <- glm(formula = IMI ~ logSCCpre + TEAT, family = binomial, data = df_teat)
summary(glm_teat)
drop1(glm_teat)

# To obtain full data megre with: Breed, Herd_type, All treatments, PCR tests
# create data set with ONLY treatments and tests 150 days before calving...

#------------------------------------------------------------
# save cleaned data:

save.image("M:/PCR_data/PCR_teat.RData")
gc()






