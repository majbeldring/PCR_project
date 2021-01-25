#--------------------------------------------------------
# Maj Beldring Henningsen, majbh@sund.ku.dk

# visualizing lactation curves pre dry-off
# Model 1: Wilmink SCC curve

# Variables for initial model fitting:
# DYR_ID, BES_ID, logSCC, DIM, MILK (for creating tSCC if needed) 

#-------------------------------------------------------
# Packages and settings:

library(tidyverse)
library(data.table)
library(plotly)
library(GGally)
library(tidymodels)
#library(ggExtra)
#library(ggalluvial)
Sys.setlocale("LC_ALL","English") # data formatting
memory.size()            # Checking your memory size
memory.limit()           # Checking the set limit
memory.limit(size=56000) # suggest for 64 bit
options(stringsAsFactors = FALSE) # prevent factorizing caracters

#------------------------------------------------------
# to do:
## line 150: single gradient
## line 164: .x ?
## line 166: debug, tidy
## nest
## create IMI data and non IMI data
## plot curves, IMI and not IMI
## nls, IMI and not IMI

#-------------------------------------------------------
# Loading and prepare data:

load("M:/PCR_data/PCR_merge.RData") 
rm(df_all_dates, df_pcr); gc() # keep only curve data

# test data: holstein, parity 2, conventionel
# (keeping MILK as well for visualization)
df_nls <- df_curve %>%
  mutate(logSCC = log(SCC)) %>%
  filter(BREED == "holstein") %>%
  filter(PARITY == 2) %>%
  filter(HERDTYPE == 1) %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC, MILK) %>%
  filter(BES_ID == 3555412 | BES_ID == 4523412 | BES_ID == 4305912 ) %>%
  mutate(DIM = as.numeric(DIM)) %>%
  mutate(BES_ID = as.factor(BES_ID)) %>%
  mutate(DYR_ID = as.factor(DYR_ID))

# BES_ID == 3937512 # removing this herd to avoid singular gradient. Unknown why. Problaly start values..

str(df_nls)

# con:  BES_ID == 10206884 | BES_ID == 5516712 | BES_ID == 2262612
# eco:  | BES_ID == 1016612 | BES_ID == 3937512| BES_ID == 5233712

# random herds: 3555412, 4523412, 4305912

#----------------------------------------------------------
# visualization 

# ecdf plot:
p0 <- ggplot(df_nls, aes(DIM, logSCC, colour = BES_ID)) + stat_ecdf(geom = "step") + 
  labs(title="ecdf plot", y = "logSCC", x="DIM") + 
  theme_bw()
p0

# normal ggplot - logSCC
p1 <- ggplot(data = df_nls, aes(x = DIM, y = logSCC, colour = BES_ID)) +   
  geom_point() + theme_bw() +
  ggtitle("DIM vs logSCC") +
  ylab("logSCC") + xlab("DIM") +
  theme(legend.position = "none")
p1

# normal ggplot - milk
p2 <- ggplot(data = df_nls, aes(x = DIM, y = MILK, colour = BES_ID)) +   
  geom_point() + theme_bw() +
  ggtitle("DIM vs MILK") +
  ylab("logSCC") + xlab("DIM") +
  theme(legend.position = "none")
p2

# plot each herd, logSCC
p3 <- ggplot(data = df_nls, 
            aes(x = DIM, y = logSCC, colour = BES_ID)) + 
  geom_point() +
  geom_smooth()+
  ggtitle("2 random herds: con, holstein, parity 2") +
  ylab("logSCC")+
  xlab("DIM")+
  theme_bw()+  
  theme(legend.position = "none") +
  facet_wrap( ~ BES_ID)
p3

# plot each herd, MILK
p4 <- ggplot(data = df_nls, 
             aes(x = DIM, y = MILK, colour = BES_ID)) + 
  geom_point() +
  geom_smooth()+
  ggtitle("2 random herds: con, holstein, parity 2") +
  ylab("MILK")+
  xlab("DIM")+
  theme_bw()+  
  theme(legend.position = "none") +
  facet_wrap( ~ BES_ID)
p4

# con:  BES_ID == 10206884 | BES_ID == 5516712 | BES_ID == 2262612
# eco:  | BES_ID == 1016612 | BES_ID == 3937512| BES_ID == 5233712


#-----------------------------------------------------------------------
# wilmink basic equation - from Graessboells eq. 8 (but with single log and SCC rather than tSCC)

nls1 <- nls(formula = logSCC ~ a + b * DIM + exp(-(exp(c)) * DIM)*d, data = df_nls, 
            start = list(a = 25, b = -1, c = -3, d = -1), 
            control = list(
              maxiter = 100, tol = 1e-05, minFactor = 1/1024, 
              printEval = FALSE, warnOnly = TRUE), na.action = na.exclude, 
            algorithm = "default", trace = FALSE)

# output:
summary(nls1)
par(mfrow=c(2,1))
plot(profile(nls1))

#---------------------------------------------------------
# nls in ggplot 

p_nls <- ggplot(df_nls, aes(x = DIM, y = logSCC, fill = BES_ID, colour = BES_ID)) + 
  geom_point() +
  ggtitle("nls, logSCC~DIM") +
  stat_smooth(method = 'nls', formula = y ~ a + b * x + exp(-(exp(c)) * x)*d, se = FALSE, 
              method.args = list(start=c(a = 25, b = -1, c = -3, d = -1), 
                                 control=nls.control(maxiter=200)), colour = "black")+
  #stat_function(fun = fun.1)+
  geom_smooth() + 
  facet_wrap( ~ BES_ID)
p_nls

#-------------------------------------------------------------------------
# intercorrelation of wilmink curve parametres

# tidy nodel
tidy_nls <- df_nls %>%
  select(c("BES_ID", "DIM", "logSCC")) %>%  # select needed columns
  nest(yields = c(logSCC, DIM)) %>%
  mutate(model = map(
    yields, ~ nls(formula = logSCC ~ a + b * DIM + exp(-(exp(c)) * DIM)*d,
                  start = c(a = 25, b = -1, c = -3, d = -1),
                  data = .x))) # data must be vector, not dataframe

# model tidying + adjusting p values:
slopes_nls <- tidy_nls %>%
  mutate(coefs = map(model, tidy)) %>%
  unnest(coefs) 


tidy_nls
slopes_nls

pairs_nls <- slopes_nls %>% 
  select(BES_ID, term, estimate) %>% 
  pivot_wider(names_from = term, values_from = estimate, id_cols = BES_ID  )

ggpairs(pairs_nls, columns = c("a","b","c","d"))

#---------------------------------------------------------------------------
# the different outputs

coef(summary(nls1))


# END OF CODE..


#-------------------------------------------------------------------------
# biblio of wilmink examples:

# creating using eq. 8: Here single log and SCC instead of tSCC (test these later)
nls(formula = logSCC ~ a + b * DIM + exp(-(exp(c)) * DIM)*d, data = df_nls, 
    start = list(a = 25, b = -1, c = -3, d = -1), 
    control = list(
      maxiter = 100, tol = 1e-05, minFactor = 1/1024, 
      printEval = FALSE, warnOnly = TRUE), na.action = na.exclude, 
    algorithm = "default", trace = FALSE)

# output:
summary(nls1)
par(mfrow=c(2,1))
plot(profile(nls1))

# nlsLM
f <- function(DIM,a,b,c) {a*(1-exp( -1*(x/b)^c) )} 
nls(logSCC~f(DIM,a,b,c),start=list(a=1,b=1, c=1) ) 
nlsLM(logSCC~f(DIM,a,b,c),start=list(a=1,b=1, c=1) )

# run over each herd and then each parity... Not working
sapply(  split( df_2012 , df_2012$BES_ID), function(d){ dat <- list2env(d)
nlsfit <- nls( form = logSCC ~ a * (1-exp(-b * DIM)), 
               data=df_2012, start= list( a=max(logSCC), b=-11),
               control= control1) 
list(a = coef(nlsfit)[1], b = coef(nlsfit)[2])} )

# from iCull_fab line 81
nls1 <- nls(logSCC~a*(DIM^b)*exp(-exp(d)*DIM),start=list(a=30,b=0.2,d=-6),
            data = df_model1, algorithm="port",
            lower=c(1,0,-10),upper=c(70,0.7,-4),
            control=list(maxiter=200,warnOnly=TRUE))


#model$Wilmink
nls(formula = trait ~ a + b * e^(-k * dim) + c * dim, data = x, 
    start = list(a = 25, b = -7, c = -0.03, k = 0.1), control = list(
      maxiter = 100, tol = 1e-05, minFactor = 0.0009765625, 
      printEval = FALSE, warnOnly = TRUE), na.action = na.exclude, 
    algorithm = "default", trace = FALSE)

#model$WilminkA
nls(formula = trait ~ a + b * e^(-k * dim) + c * (dim/100), data = x, 
    start = list(a = 25, b = -7, c = -3, k = 0.1), control = list(
      maxiter = 100, tol = 1e-05, minFactor = 0.0009765625, 
      printEval = FALSE, warnOnly = TRUE), na.action = na.exclude, 
    algorithm = "default", trace = FALSE)

#model$WilminkB
nls(formula = trait ~ a + (b - a) * (1 - e^(-k * dim)) - c * 
      dim, data = x, start = list(a = 20, b = 30, c = 0.005, k = 0.08), 
    control = list(maxiter = 100, tol = 1e-05, minFactor = 0.0009765625, 
                   printEval = FALSE, warnOnly = TRUE), na.action = na.exclude, 
    algorithm = "default", trace = FALSE)

