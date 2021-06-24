

# Maj Beldring Henningsen, majbh@sund.ku.dk

# PCR_model1 for PCR project
# nls / Wilmink



#-------------------------------------------------------
# Packages and settings:

library(tidyverse)
library(gridExtra)
library(data.table)
library(plotly)
library(GGally)
library(nlstools) # for bootstrapping
library(nlme) # for nlslist
library(nlshelper) # for tidy(fit)
#library(ggExtra)
#library(ggalluvial)
Sys.setlocale("LC_ALL","English") # data formatting
memory.size()            # Checking your memory size
memory.limit()           # Checking the set limit
memory.limit(size=56000) # suggest for 64 bit
options(stringsAsFactors = FALSE) # prevent factorizing caracters


#-------------------------------------------------------
# Loading data:

load("M:/PCR_data/PCR_nls.RData")     # to retrieve parameters from nls output 
load("M:/PCR_data/PCR_merge2.RData") 
rm(df_curve, df_pcr); gc() # keep only model data

#-----------------------------------------------------------

# pre preparing data; keep only selected variables:
df <- df_model %>% 
  dplyr::select(BES_ID, DYR_ID, PARITY, BREED, HERDTYPE, DIM, SCC, logSCC, MILK, IMI, DRY_TREAT, PCR_TEST, RES_MAJOR, OTHER_AB, TEAT_TREAT)

# create coloumn counting herd occurences
df <- df %>%
  group_by(BES_ID, PARITY) %>%
  mutate(count = n())

# peparing Parity data:
## divide into parities
## (choose only herd with min 200 observations (count coloum))
df_2 <- df %>%
  filter(PARITY == 2)
df_3 <- df %>%
  filter(PARITY == 3)
df_4 <- df %>%
  filter(PARITY == 4)


# small test data - 9 random selected herds (holstein, parity 2)
df_test <- df_curve %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC, MILK) %>%
  filter(BES_ID == 3555412 | BES_ID == 4523412 | BES_ID == 4305912 | BES_ID == 3194512 | 
           BES_ID == 3989812|  BES_ID == 3507612 | BES_ID == 5243512 | BES_ID == 1006112 |  
           BES_ID == 3169112 | BES_ID == 3892412 | BES_ID == 4966212 | 
           BES_ID == 5143912 | BES_ID == 5834912 | BES_ID == 5974112 | BES_ID == 6628012 |
           BES_ID == 7729812 | BES_ID == 9986698 | BES_ID == 10021731 )


#---------------------------------------------------------
# Population

dplyr::n_distinct(df_model$BES_ID) # 3474 unique herds
dplyr::n_distinct(df_model$DYR_ID) # 997.784 unique animals


summary(df_2)
summary(df_3)
summary(df_4)


#----------------------------------------------------------
# ecdf plots:

p_IMI <- ggplot(df_2, aes(logSCC, colour = IMI)) + stat_ecdf(geom = "step") + 
  labs(title="IMI or not", x="logSCC") + 
  theme_bw()
p_IMI

p_DRY <- ggplot(df_2, aes(logSCC, colour = DRY_TREAT)) + stat_ecdf(geom = "step") + 
  labs(title="IMI or not", x="logSCC") + 
  theme_bw()
p_DRY



#------------------------------------------------------------------
# normal ggplot 

# logSCC
p1 <- ggplot(data = df_test, aes(x = DIM, y = logSCC, colour = BES_ID)) +   
  geom_point() + theme_bw() +
  ggtitle("DIM vs logSCC") +
  ylab("logSCC") + xlab("DIM") +
  theme(legend.position = "none")
p1



## TEST data (facet_wrap for herds applied)

# plot each herd, logSCC
p3 <- ggplot(data = df_test, 
             aes(x = DIM, y = logSCC, colour = BES_ID)) + 
  geom_point() +
  geom_smooth(colour = "black")+
  ggtitle("9 random herds: con, holstein, parity 2") +
  ylab("logSCC")+
  xlab("DIM")+
  theme_bw()+  
  theme(legend.position = "none") +
  facet_wrap( ~ BES_ID)
p3



# nls in ggplot - ONLY with test data when facet_wrap is applied
p_nls <- ggplot(df_test, aes(x = DIM, y = logSCC, fill = BES_ID, colour = BES_ID)) + 
  geom_point() +
  ggtitle("nls, logSCC~DIM") +
  stat_smooth(method = 'nls', formula = y ~ a + b * x + exp(-(exp(k)) * x)*d, se = FALSE, 
              method.args = list(start=c(a = 4, b = 0.003, k = -2, d = 2.6), 
                                 control=nls.control(maxiter=200)), colour = "black")+
  geom_smooth() + 
  facet_wrap( ~ BES_ID)
p_nls





#--------------------------------------------------------------------
# Wilmink curve based on nls

# STARTLIST defined and equation 8
st <- list(a = 3.9, b = 0.0027, k = -1.94, d = 2.6)
f_nls <- logSCC ~ a + b * DIM + exp(-(exp(k)) * DIM)*d | BES_ID


#---------------------------------------------------------------------
# 1: Curves for dry treated vs not dry treated

## organic (herdtype = 0)
## No other AB treats 
df_eco_treat <- df %>% 
  filter(HERDTYPE == 0) %>%
  filter(OTHER_AB == 0) %>%
  filter(DRY_TREAT == 1)
df_eco_notreat <- df %>% 
  filter(HERDTYPE == 0) %>%
  filter(OTHER_AB == 0) %>%
  filter(DRY_TREAT == 0)

treat_2 <- df_eco_treat %>%
  filter(PARITY == 2) %>%
  filter(count > 200)
treat_3 <- df_eco_treat %>%
  filter(PARITY == 3) %>%
  filter(count > 200)
treat_4 <- df_eco_treat %>%
  filter(PARITY == 4) %>%
  filter(count > 200)

notreat_2 <- df_eco_notreat %>%
  filter(PARITY == 2) %>%
  filter(count > 200)
notreat_3 <- df_eco_notreat %>%
  filter(PARITY == 3) %>%
  filter(count > 200)
notreat_4 <- df_eco_notreat %>%
  filter(PARITY == 4) %>%
  filter(count > 200)



# Run nls:

# Treated, organic, no other AB treat: 
n_2_treat <- nlsList(f_nls, treat_2, start = sapply(st, mean),
               control = list(maxiter = 400, tol = 1e-05, minFactor = 1/1024, 
                              printEval = FALSE, warnOnly = TRUE))
out_treat2 <- coef(n_2_treat) %>%
  drop_na()

n_3_treat <- nlsList(f_nls, treat_3, start = sapply(st, mean),
                     control = list(maxiter = 400, tol = 1e-05, minFactor = 1/1024, 
                                    printEval = FALSE, warnOnly = TRUE))
out_treat3 <- coef(n_3_treat) %>%
  drop_na()

n_4_treat <- nlsList(f_nls, treat_4, start = sapply(st, mean),
                     control = list(maxiter = 400, tol = 1e-05, minFactor = 1/1024, 
                                    printEval = FALSE, warnOnly = TRUE))
out_treat4 <- coef(n_4_treat) %>%
  drop_na()


