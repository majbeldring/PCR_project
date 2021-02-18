

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
library(tidymodels)
library('minpack.lm') # nlsLM (not using this for now)
library('nls2') # nls2 (not using this for now)
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

load("M:/PCR_data/PCR_merge1.RData") 
rm(df_all_dates, df_pcr); gc() # keep only curve data

#-----------------------------------------------------------
# pre preparing data; add logSCC and IMI post calving:

# pre preparing data; keep only selected variables:
df_nls <- df_curve %>% 
  dplyr::select(BES_ID, DYR_ID, PARITY, BREED, HERDTYPE, DIM, SCC, SCCpost, MILK, IMI)

# choosing only conventionel herds and holstein and parity 2
df_test <- df_nls %>%
  drop_na() %>%
  filter(SCC > 1) %>%
  filter(BREED == 1) %>%
  filter(HERDTYPE == 1)

df_2 <- df_test %>%
  mutate(DIM = as.numeric(DIM))

df_2 <- df_2 %>%
  mutate(logSCC = log(SCC)) %>%
  mutate(logSCCpost = log(SCCpost)) %>%
  dplyr::select(-IMI) %>%
  mutate(IMI = case_when(SCCpost < 200 ~ 0, SCCpost >= 200 ~ 1)) %>%
  mutate(IMI = factor(IMI))



df_2 <- df_2 %>%
  mutate(BES_ID = factor(BES_ID)) %>%
  mutate(DYR_ID = factor(DYR_ID))

df_backup <- df_nls

# pre preparing data; keep only selected variables:
df_nls <- df_nls %>% 
  dplyr::select(BES_ID, DYR_ID, PARITY, BREED, HERDTYPE, DIM, logSCC, MILK, IMI)


df_nls <- df_nls %>%
  mutate(BES_ID = as.factor(BES_ID)) %>%
  mutate(DYR_ID = as.factor(DYR_ID))

# removing herds with less than 50 animals and 200 observations
# df_nls <- df_curve %>% 
#   dplyr::select(BES_ID, DYR_ID, PARITY, BREED, HERDTYPE, DIM, logSCC, MILK, IMI) %>%
#   summarise(BES_NO = length(BES_ID)) %>%
#   filter(BES_NO > 50)
# df_nls %>% 
#   group_by(BES_ID) %>%
#   summarise(BES_NO = length(BES_ID))
# df_nls <- df_pcr %>%
#   filter(length(BES_ID) > 50)


# choosing only conventionel herds and holstein
df_test <- df_nls %>%
  filter(logSCC > 0) %>%
  filter(BREED == "holstein") %>%
  filter(HERDTYPE == 1)

# choosing only Parity 2
df_test <- df_nls %>%
  filter(logSCC > 0) %>%
  filter(BREED == "holstein") %>%
  filter(HERDTYPE == 1)


rm(df_curve); gc()

str(df_nls)

#--------------------------------------------------------
# preparing data:


# peparing general IMI data, all herds:
df_IMI <- df6 %>%
  mutate(logSCC = log(SCC)) %>%
  filter(logSCC > 0) %>%
  filter(BREED == "holstein") %>%
  filter(PARITY == 2) %>%
  filter(IMI == 1) %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC, MILK) %>%
  mutate(DIM = as.numeric(DIM)) %>%



df_IMI <- df6 %>%
  mutate(logSCC = log(SCC)) %>%
  filter(logSCC > 0) %>%
  filter(BREED == "holstein") %>%
  filter(PARITY == 2) %>%
  filter(HERDTYPE == 1) %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC, MILK, IMI) %>%
  mutate(DIM = as.numeric(DIM)) %>%
  mutate(BES_ID = as.factor(BES_ID)) %>%
  mutate(DYR_ID = as.factor(DYR_ID))


# parity 2, holstein
# (keeping MILK as well for visualization)
df_2 <- df_curve %>%
  mutate(logSCC = log(SCC)) %>%
  filter(logSCC > 0) %>%
  filter(BREED == "holstein") %>%
  filter(PARITY == 2) %>%
  #filter(HERDTYPE == 1) %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC, MILK) %>%
  mutate(DIM = as.numeric(DIM)) %>%
  mutate(BES_ID = as.factor(BES_ID)) %>%
  mutate(DYR_ID = as.factor(DYR_ID))
## remove outlies df_2: (choosen from extreme parameters values and NAs in nls)
# drops_2 <- c("x","z")
# add_rownames(df_nls, var = "BES_ID")
# drops <- herd_na %>%
#   select(-a, -b, -k, -d)
# df_nls[ , !(names(df_nls) %in% drops)]



# parity 3, holstein
# (keeping MILK as well for visualization)
df_3 <- df_curve %>%
  mutate(logSCC = log(SCC)) %>%
  filter(logSCC > 0) %>%
  filter(BREED == "holstein") %>%
  filter(PARITY == 3) %>%
  #filter(HERDTYPE == 1) %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC, MILK) %>%
  mutate(DIM = as.numeric(DIM)) %>%
  mutate(BES_ID = as.factor(BES_ID)) %>%
  mutate(DYR_ID = as.factor(DYR_ID))


# parity 4+, holstein
# (keeping MILK as well for visualization)
df_4 <- df_curve %>%
  mutate(logSCC = log(SCC)) %>%
  filter(logSCC > 0) %>%
  filter(BREED == "holstein") %>%
  filter(PARITY != 1) %>%
  filter(PARITY != 2) %>%
  filter(PARITY != 3) %>%
  #filter(HERDTYPE == 1) %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC, MILK) %>%
  mutate(DIM = as.numeric(DIM)) %>%
  mutate(BES_ID = as.factor(BES_ID)) %>%
  mutate(DYR_ID = as.factor(DYR_ID))


# small test data - 9 random selected herds (holstein, parity 2)
df_test <- df_curve %>%
  mutate(logSCC = log(SCC)) %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC, MILK) %>%
  mutate(DIM = as.numeric(DIM)) %>%
  mutate(BES_ID = as.factor(BES_ID)) %>%
  mutate(DYR_ID = as.factor(DYR_ID))%>%
  filter(BES_ID == 3555412 | BES_ID == 4523412 | BES_ID == 4305912 | BES_ID == 3194512 | 
           BES_ID == 3989812|  BES_ID == 3507612 | BES_ID == 5243512 | BES_ID == 1006112 |  
           BES_ID == 3169112 | BES_ID == 3892412 | BES_ID == 4966212 | 
           BES_ID == 5143912 | BES_ID == 5834912 | BES_ID == 5974112 | BES_ID == 6628012 |
           BES_ID == 7729812 | BES_ID == 9986698 | BES_ID == 10021731 )
         
         
           # BES_ID == 1549112 | BES_ID == 5834912 | BES_ID == 5974112 | BES_ID == 6628012 |
           # BES_ID == 7729812 | BES_ID == 9986698 | BES_ID == 10021731 |
           # 
           # BES_ID == 1549112 | BES_ID == 5834912 | BES_ID == 5974112 | BES_ID == 6628012 |
           # BES_ID == 7729812 | BES_ID == 9986698 | BES_ID == 10021731


#---------------------------------------------------------
# model1 population

dplyr::n_distinct(df_nls$BES_ID) # 2296 unique herds (2520 with eco)
dplyr::n_distinct(df_nls$DYR_ID) # 657.394 unique animals (718.156 with eco)
summary(df_nls)

# output counts
# rows in herd_para: equal to unique BES_ID in df_nls
sapply(herd_para, function(x) sum(is.na(x))) # 36 for each parameter
sum(is.na(herd_para)) # 144 (36x4)

# creating dataframe with NAs from output
herd_na <- herd_para[rowSums(is.na(herd_para)) > 0,] # names of herds with NAs


#----------------------------------------------------------
# visualization of data - p_nls is with nls regression line

# ecdf plot:
p_IMI <- ggplot(df_nls, aes(DIM, MILK, colour = IMI)) + stat_ecdf(geom = "step") + 
  labs(title="IMI or not", y = "logSCC", x="DIM") + 
  theme_bw()
p_IMI

p_IMI <- ggplot(df_2, aes(logSCC, colour = IMI)) + stat_ecdf(geom = "step") + 
  labs(title="IMI or not", x="logSCC") + 
  theme_bw()
p_IMI


# ecdf plot:
p0 <- ggplot(df_test, aes(DIM, logSCC, colour = BES_ID)) + stat_ecdf(geom = "step") + 
  labs(title="ecdf plot", y = "logSCC", x="DIM") + 
  theme_bw()
p0


# normal ggplot - logSCC
p1 <- ggplot(data = df_test, aes(x = DIM, y = logSCC, colour = BES_ID)) +   
  geom_point() + theme_bw() +
  ggtitle("DIM vs logSCC") +
  ylab("logSCC") + xlab("DIM") +
  theme(legend.position = "none")
p1

# normal ggplot - milk
p2 <- ggplot(data = df_test, aes(x = DIM, y = MILK, colour = BES_ID)) +   
  geom_point() + theme_bw() +
  ggtitle("DIM vs MILK") +
  ylab("logSCC") + xlab("DIM") +
  theme(legend.position = "none")
p2

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

# plot each herd, MILK
p4 <- ggplot(data = df_test, 
             aes(x = DIM, y = MILK, colour = BES_ID)) + 
  geom_point() +
  geom_smooth(colour = "black")+
  ggtitle("grouped by herds") +
  ylab("MILK")+
  xlab("DIM")+
  theme_bw()+  
  theme(legend.position = "none") +
  facet_wrap( ~ BES_ID)
p4


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



#-----------------------------------------------------------------------
# choosing start parameters - NOT DONE
# for now the following is used (a = 3.9, b = 0.0027, k = -1.94, d = 2.6)

# approach: log transform until linear -> do lm -> use output parameters
eq8 <- min(df_test$logSCC) * 0.5 # why the 0.5 ???
lm_eq8 <- lm(log(logSCC - eq8) ~ DIM, data=df_test)
start <- list(a=exp(coef(model.0)[1]), b=coef(model.0)[2], c=eq8)
model <- nls(logSCC ~ a + b * DIM + exp(-(exp(k)) * DIM)*d, data = df_test, start = start)



#-----------------------------------------------------------------------
# wilmink basic equation - from Graessboells eq. 8 
# this runs without upper and lower, runs with full data
# applying port algorithm if bounds (upper, lower) will be used 

# parity 2, holstein only
nls_2 <- nls(formula = logSCC ~ a + b * DIM + exp(-(exp(k)) * DIM)*d, data = df_2, 
             start = list(a = 3.9, b = 0.0027, k = -1.94, d = 2.6),
             control = list(maxiter = 100, tol = 1e-05, minFactor = 1/1024, 
                            printEval = FALSE, warnOnly = TRUE), 
             algorithm = "port", trace = FALSE)

# output parity 2, holstein only
summary(nls_2)
par(mfrow=c(2,2))
plot(profile(nls_2)) # visualize confint
coef(summary(nls_2))
confint(nls_2)


# IMI
df_IMI1 <- df_IMI %>% 
  filter(IMI == 1)

nls_IMI <- nls(formula = logSCC ~ a + b * DIM + exp(-(exp(k)) * DIM)*d, data = df_IMI, 
             start = list(a = 3.9, b = 0.0027, k = -1.94, d = 2.6),
             control = list(maxiter = 100, tol = 1e-05, minFactor = 1/1024, 
                            printEval = FALSE, warnOnly = TRUE), 
             algorithm = "port", trace = FALSE)

# output parity 2, holstein only
summary(nls_IMI)
par(mfrow=c(2,2))
plot(profile(nls_IMI)) # visualize confint
coef(summary(nls_IMI))
confint(nls_IMI)


#-----------------------------------------------------------------------
# Retrieving parameters for each herd
## THIS WORKS but still several singular gradients for full data (holstein/parity)

# define startlist and equation 8
st <- list(a = 3.9, b = 0.0027, k = -1.94, d = 2.6)
f_nls <- logSCC ~ a + b * DIM + exp(-(exp(k)) * DIM)*d | BES_ID

# parity 2, holstein only, DF=df_2
m_2 <- nlsList(f_nls, df_2, start = sapply(st, mean),
               control = list(maxiter = 400, tol = 1e-05, minFactor = 1/1024, 
                              printEval = FALSE, warnOnly = TRUE))

tidy(m_2) # error.. differing number of rows: 1, 0 (work with test data)
tidy(m_2, conf.int=TRUE) # error as above. (test data: singular error)
out_m2 <- coef(m_2)
m2_na <- out_m2[rowSums(is.na(out_m2)) > 0,] # names of herds with NAs


# parity 3, holstein only, DF=df_3
m_3 <- nlsList(f_nls, df_3, start = sapply(st, mean),
               control = list(maxiter = 400, tol = 1e-05, minFactor = 1/1024, 
                              printEval = FALSE, warnOnly = TRUE))

tidy(m_3) # error.. differing number of rows: 1, 0 (work with test data)
tidy(m_3, conf.int=TRUE) # error as above. (test data: singular error)
out_m3 <- coef(m_3)
m3_na <- out_m3[rowSums(is.na(out_m3)) > 0,] # names of herds with NAs


# parity 4+, holstein only, DF=df_4
m_4 <- nlsList(f_nls, df_4, start = sapply(st, mean),
               control = list(maxiter = 400, tol = 1e-05, minFactor = 1/1024, 
                              printEval = FALSE, warnOnly = TRUE))

tidy(m_4) # error.. differing number of rows: 1, 0 (work with test data)
tidy(m_4, conf.int=TRUE) # error as above. (test data: singular error)
out_m4 <- coef(m_4)
m4_na <- out_m4[rowSums(is.na(out_m4)) > 0,] # names of herds with NAs

#------------------------------------------------------------------------------


# define startlist and equation 8
st <- list(a = 3.9, b = 0.0027, k = -1.94, d = 2.6)
f_nls <- logSCC ~ a + b * DIM + exp(-(exp(k)) * DIM)*d | BES_ID


###### WITH IMI

df_IMI <- df_2 %>%
  filter(IMI == 1)

# IMI: parity 2, holstein only, DF=df_2
m_IMI <- nlsList(f_nls, df_IMI, start = sapply(st, mean),
               control = list(maxiter = 400, tol = 1e-05, minFactor = 1/1024, 
                              printEval = FALSE, warnOnly = TRUE))

tidy(m_IMI) # error.. differing number of rows: 1, 0 (work with test data)
tidy(m_IMI, conf.int=TRUE) # error as above. (test data: singular error)
out_IMI <- coef(m_IMI)
mIMI_na <- out_IMI[rowSums(is.na(out_IMI)) > 0,] # names of herds with NAs


######### WITHOUT IMI


df_no <- df_2 %>%
  filter(IMI == 0)

# No IMI: parity 2, holstein only, DF=df_2
m_no <- nlsList(f_nls, df_no, start = sapply(st, mean),
                 control = list(maxiter = 400, tol = 1e-05, minFactor = 1/1024, 
                                printEval = FALSE, warnOnly = TRUE))

tidy(m_no) # error.. differing number of rows: 1, 0 (work with test data)
tidy(m_no, conf.int=TRUE) # error as above. (test data: singular error)
out_no <- coef(m_no)
mno_na <- out_IMI[rowSums(is.na(out_no)) > 0,] # names of herds with NAs



######### PLOT

out_IMI <- out_IMI %>%
  drop_na()

out_no <- out_no %>%
  drop_na()
  

ggplot() + 
  geom_histogram(data = out_IMI, aes(x = a, fill = "r"), alpha = 0.3, binwidth = 0.1) +
  geom_density(data = out_no, aes(x = a, fill = "b"), alpha = 0.3) +
  scale_colour_manual(name ="out_IMI", values = c("r" = "red", "b" = "blue"), labels=c("b" = "blue values", "r" = "red values")) +
  scale_fill_manual(name ="out_IMI", values = c("r" = "red", "b" = "blue"), labels=c("b" = "blue values", "r" = "red values"))


###

# IMI
out_IMI <- out_IMI %>%
  drop_na() %>%
  filter(a>1) %>%
  filter(a<6) %>%
  filter(b>0.001) %>%
  filter(b<0.005) %>%
  filter(k > (-3)) %>%
  filter(k < (-1))  %>%
  filter(d>0.9) %>%
  filter(d<3.5)

IMI_a <- qplot(out_IMI$a,
              geom="histogram",
              binwidth = 0.1,  
              main = "a: IMI, parity 2, holstein", 
              xlab = "a values",  
              fill=I("white"), col=I("green4"))

IMI_b <- qplot(out_IMI$b,
              geom="histogram",
              binwidth = 0.0001,  
              main = "b: IMI, parity 2, holstein", 
              xlab = "b values",  
              fill=I("white"), col=I("dodgerblue4"))

IMI_k <- qplot(out_IMI$k,
              geom="histogram",
              binwidth = 0.1,  
              main = "k: IMI, parity 2, holstein", 
              xlab = "k values",  
              fill=I("white"), col=I("darkorange"))

IMI_d <- qplot(out_IMI$d,
              geom="histogram",
              binwidth = 0.1,  
              main = "d: IMI, parity 2, holstein", 
              xlab = "d values",  
              fill=I("white"), col=I("firebrick4"))

# No IMI
out_no <- out_no %>%
  drop_na() %>%
  filter(a>1) %>%
  filter(a<6) %>%
  filter(b>0.001) %>%
  filter(b<0.005) %>%
  filter(k > (-3)) %>%
  filter(k < (-1))  %>%
  filter(d>0.9) %>%
  filter(d<3.5)

NO_a <- qplot(out_no$a,
               geom="histogram",
               binwidth = 0.1,  
               main = "a: NO IMI, parity 2, holstein", 
               xlab = "a values",  
               fill=I("white"), col=I("green4"))

NO_b <- qplot(out_no$b,
               geom="histogram",
               binwidth = 0.0001,  
               main = "b: NO IMI, parity 2, holstein", 
               xlab = "b values",  
               fill=I("white"), col=I("dodgerblue4"))

NO_k <- qplot(out_no$k,
               geom="histogram",
               binwidth = 0.1,  
               main = "k: NO IMI, parity 2, holstein", 
               xlab = "k values",  
               fill=I("white"), col=I("darkorange"))

NO_d <- qplot(out_no$d,
               geom="histogram",
               binwidth = 0.1,  
               main = "d: NO IMI, parity 2, holstein", 
               xlab = "d values",  
               fill=I("white"), col=I("firebrick4"))

# plotting all parity 2 histograms:
grid.arrange(IMI_a, IMI_b, IMI_k, IMI_d, 
             NO_a, NO_b, NO_k, NO_d,
             ncol=4, nrow=2)



#--------------------------------------------------------------------------
# histogram of parameters from herd_para. Holstein only

# parity 2, 
#a, Holstein only
out_m2 <- out_m2 %>%
  drop_na() %>%
  filter(a>1) %>%
  filter(a<6)
p2_a <- qplot(out_m2$a,
              geom="histogram",
              binwidth = 0.1,  
              main = "a: parity 2, holstein", 
              xlab = "a values",  
              fill=I("white"), col=I("green4"))
#b, Holstein only
out_m2 <- out_m2 %>%
  drop_na() %>%
  filter(b>0.001) %>%
  filter(b<0.005)
p2_b <- qplot(out_m2$b,
              geom="histogram",
              binwidth = 0.0001,  
              main = "b: parity 2, holstein", 
              xlab = "b values",  
              fill=I("white"), col=I("dodgerblue4"))
#k, Holstein only
out_m2 <- out_m2 %>%
  drop_na() %>%
  filter(k > (-3)) %>%
  filter(k < (-1))
p2_k <- qplot(out_m2$k,
              geom="histogram",
              binwidth = 0.1,  
              main = "k: parity 2, holstein", 
              xlab = "k values",  
              fill=I("white"), col=I("darkorange"))
#d, Holstein only
out_m2 <- out_m2 %>%
  drop_na() %>%
  filter(d>0.9) %>%
  filter(d<3.5)
p2_d <- qplot(out_m2$d,
              geom="histogram",
              binwidth = 0.1,  
              main = "d: parity 2, holstein", 
              xlab = "d values",  
              fill=I("white"), col=I("firebrick4"))

# plotting all parity 2 histograms:
p2_a; p2_b; p2_k; p2_d



# parity 3, 
#a, Holstein only
out_m3 <- out_m3 %>%
  drop_na() %>%
  filter(a>1) %>%
  filter(a<6)
p3_a <- qplot(out_m3$a,
              geom="histogram",
              binwidth = 0.1,  
              main = "a: parity 3, holstein", 
              xlab = "a values",  
              fill=I("white"), col=I("green4"))
#b, Holstein only
out_m3 <- out_m3 %>%
  drop_na() %>%
  filter(b>0.001) %>%
  filter(b<0.005)
p3_b <- qplot(out_m3$b,
              geom="histogram",
              binwidth = 0.0001,  
              main = "b: parity 3, holstein", 
              xlab = "b values",  
              fill=I("white"), col=I("dodgerblue4"))
#k, Holstein only
out_m3 <- out_m3 %>%
  drop_na() %>%
  filter(k> (-3)) %>%
  filter(k< (-1))
p3_k <- qplot(out_m3$k,
              geom="histogram",
              binwidth = 0.1,  
              main = "k: parity 3, holstein", 
              xlab = "k values",  
              fill=I("white"), col=I("darkorange"))
#d, Holstein only
out_m3 <- out_m3 %>%
  drop_na() %>%
  filter(d>0.9) %>%
  filter(d<3.5)
p3_d <- qplot(out_m3$d,
              geom="histogram",
              binwidth = 0.1,  
              main = "d: parity 3, holstein", 
              xlab = "d values",  
              fill=I("white"), col=I("firebrick4"))

# plotting all parity 3 histograms:
par(mfrow=c(2,2)) 
p3_a; p3_b; p3_k; p3_d


# parity 4+, 
#a, Holstein only
out_m4 <- out_m4 %>%
  drop_na() %>%
  filter(a>1) %>%
  filter(a<6)
p4_a <- qplot(out_m4$a,
              geom="histogram",
              binwidth = 0.1,  
              main = "a: parity 4+, holstein", 
              xlab = "a values",  
              fill=I("white"), col=I("green4"))
#b, Holstein only
out_m4 <- out_m4 %>%
  drop_na() %>%
  filter(b>0.001) %>%
  filter(b<0.005)
p4_b <- qplot(out_m4$b,
              geom="histogram",
              binwidth = 0.0001,  
              main = "b: parity 4+, holstein", 
              xlab = "b values",  
              fill=I("white"), col=I("dodgerblue4"))
#k, Holstein only
out_m4 <- out_m4 %>%
  drop_na() %>%
  filter(k> (-3)) %>%
  filter(k< (-1))
p4_k <- qplot(out_m4$k,
              geom="histogram",
              binwidth = 0.1,  
              main = "k: parity 4+, holstein", 
              xlab = "k values",  
              fill=I("white"), col=I("darkorange"))
#d, Holstein only
out_m4 <- out_m4 %>%
  drop_na() %>%
  filter(d>0.9) %>%
  filter(d<3.5)
p4_d <- qplot(out_m4$d,
              geom="histogram",
              binwidth = 0.1,  
              main = "d: parity 4+, holstein", 
              xlab = "d values",  
              fill=I("white"), col=I("firebrick4"))

# plotting all parity 4 histograms:
grid.arrange(p4_a, p4_b, p4_k, p4_d, ncol=2, nrow=2)


#-------------------------------------------------------------------------
# ggpairs tidy model: intercorrelation of wilmink curve parametres
# Onl working with test data (map function and singular gradient issues w. full data)

tidy_nls <- df_test %>%
  select(c("BES_ID", "DIM", "logSCC")) %>%  # select needed columns
  nest(yields = c(logSCC, DIM)) %>%
  mutate(model = map(
    yields, ~ nls(formula = logSCC ~ a + b * DIM + exp(-(exp(k)) * DIM)*d,
                  start = c(a = 3.9, b = 0.0027, k = -1.94, d = 2.6),
                  data = .x, 
                  control = list(
                    maxiter = 400, tol = 1e-05, minFactor = 1/1024, 
                    printEval = FALSE, warnOnly = TRUE),
                  algorithm = "port", 
                  lower=c(0.0001,0.000001,-10,0.0001),upper=c(20,0.7,-0.001,10),
                  trace = TRUE))) # data must be vector, not dataframe


# model tidying + adjusting p values:
slopes_nls <- tidy_nls %>%
  mutate(coefs = map(model, tidy)) %>%
  unnest(coefs) 

tidy_nls
slopes_nls

pairs_nls <- slopes_nls %>% 
  select(BES_ID, term, estimate) %>% 
  pivot_wider(names_from = term, values_from = estimate, id_cols = BES_ID  )

ggpairs(pairs_nls, columns = c("a","b","k","d"))

# END OF CODE..


#-------------------------------------------------------------------------
# biblio of wilmink examples: (not working properly yet)

# nlsLM
f <- function(DIM,a,b,k) {a*(1-exp( -1*(x/b)^k) )} 
nls(logSCC~f(DIM,a,b,k),start=list(a=1,b=1, k=1) ) 
nlsLM(logSCC~f(DIM,a,b,k),start=list(a=1,b=1, k=1) )

# run over each herd and then each parity... Not working
sapply(  split( df_test , df_test$BES_ID), function(d){ dat <- list2env(d)
nlsfit <- nls( form = logSCC ~ a + b * DIM + exp(-(exp(k)) * DIM)*d, 
               data=dat, 
               start= list(a = 4, b = 0.003, k = -2, d = 2.6),
               control= control1) 

list(a = coef(nlsfit)[1], b = coef(nlsfit)[2])} )

# Carsten: nls example from iCull_fab line 81
nls1 <- nls(logSCC~a*(DIM^b)*exp(-exp(d)*DIM),start=list(a=30,b=0.2,d=-6),
            data = df_model1, algorithm="port",
            lower=c(1,0,-10),upper=c(70,0.7,-4),
            control=list(maxiter=200,warnOnly=TRUE))


# grouped herds: this is not running
df_herds <- lapply(unique(df_test$BES_ID),function(i) {datasubs=df_test[df_test$BES_ID==i,];
coef(nls(logSCC ~ a + b * DIM + exp(-(exp(k)) * DIM)*d, 
         data=datasubs, 
         start = list(a = 3.9, b = 0.0027, k = -1.94, d = 2.6),
         algorithm="port", 
         control = list(maxiter = 100, tol = 1e-05, minFactor = 1/1024, 
                        printEval = FALSE, warnOnly = TRUE)))
})
df_herds <- data.frame(df_herds)
names(df_herds) <- levels(df_test$BES_ID)
df_herds <- t(df_herds)
df_herds


# grouped herds another approach: this is not running
sapply(  split( df_test , df_test$BES_ID), function(d){ dat <- list2env(d)
nlsfit <- nls( form = logSCC ~ a + b * DIM + exp(-(exp(k)) * DIM)*d, 
               data=dat, 
               start = list(a = 3.9, b = 0.0027, k = -1.94, d = 2.6),
               algorithm="port",
               control = list(maxiter = 100, tol = 1e-05, minFactor = 1/1024, 
                              printEval = FALSE, warnOnly = TRUE)) 

list(a = coef(nlsfit)[1], b = coef(nlsfit)[2], k = coef(nlsfit)[3], d = coef(nlsfit)[4])} )


# plot parameters
p2_a <- out_m2 %>%
  drop_na() %>%
  filter(a>1) %>%
  filter(a<6) %>%
  ggplot(out_m2, aes(x=a)) + 
  geom_histogram(color="green4", fill="green4")
