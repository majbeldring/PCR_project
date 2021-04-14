
# Maj Beldring, majbh@sund.ku.dk
# Basic lm analysis for Paper 1 (epi 2)

# data cleaning to do, in clean or merge script:
## exclude all other AB treatments
## include also non PCR tested animal (PCR tested: YES/NO, PCR results: POS/NEG/NA)
## us na.omit when modelling coloumns that can include NAs...

# analysis to do:
## glm on pcr and scc as binominal output
## drop1 (F test) for model testing, lowest AIC
## predict.glm for results (summary)
## Assess cofounding using the criterion of changes in effect sizes of 20%
## cox regression????
## AG table???
## create any 2x2 tables???
## explore NEG PCR -> TREATED: Extract all animals with this.. Create table..
## exp(coef(lm4)) ???

#-------------------------------------------------------
# Packages and settings:

library(tidyverse)
Sys.setlocale("LC_ALL","English")
library(rstatix) # for pairswise_t_test
library(ggpubr)
library(doBy) # or summaryBy function (stratified analysis)

#------------------------------------------------------------------------
# Loading data = workspace with cleaned data from script: "prepare?"

load("M:/data/PCR_merge.RData")
rm(df_curve, df_all)
gc()

df_pcr <- df_pcr %>% mutate(IMI = factor(IMI)) # not included yet in _merge.data
str(df_pcr)

#----------------------------------------------------------------------
######### correlation SCC2 vs continuous risk factors #################

cor(df_pcr[,c('SCC','MILKpre','SCCpre')]) # scc
cor(df_pcr[,c('logSCC','MILKpre','logSCCpre')]) # logscc

# RESULTS:
# we see that MILKpre only has a small negative influence
# logSCCpre has a higher influence

#--------------------------------------------------------------------
######### TABLES ####################################################
# with PCR numbers of positives and negatives..

table_treated <- table(df_pcr$RESULT, df_pcr$TREATED) # A = rows, B = columns
table_treated
table_IMI <- table(df_pcr$RESULT, df_pcr$IMI) # A = rows, B = columns
table_IMI

#--------------------------------------------------------------------
######### test: tSCC or SCC ########################################

# Overall test non Parity stratified
lm_s <- lm(logSCC ~ logSCCpre, data = df_pcr) 
lm_ts <- lm(logtSCC ~ logtSCCpre, data = df_pcr) 
AIC(lm_s,lm_ts) # lowest AIC =  lm_ts , so tSCC should be used

summary(lm_ts)

# stratifying, since it might only be a benefit for early parities

# Parity 2:
df_pcr2 <- df_pcr %>% filter(PARITY == 2)
lm_s2 <- lm(logSCC ~ logSCCpre, data = df_pcr2) 
lm_ts2 <- lm(logtSCC ~ logtSCCpre, data = df_pcr2) 
AIC(lm_s2,lm_ts2) # lowest AIC =  lm_ts2; so tSCC for parity 2

# Parity 3:
df_pcr3 <- df_pcr %>% filter(PARITY == 3)
lm_s3 <- lm(logSCC ~ logSCCpre, data = df_pcr3) 
lm_ts3 <- lm(logtSCC ~ logtSCCpre, data = df_pcr3) 
AIC(lm_s3,lm_ts3) # lowest AIC =  lm_ts3 , so tSCC for parity 3

# Parity >= 4:
df_pcr4 <- df_pcr %>% filter(PARITY == 4)
lm_s4 <- lm(logSCC ~ logSCCpre, data = df_pcr4) 
lm_ts4 <- lm(logtSCC ~ logtSCCpre, data = df_pcr4) 
AIC(lm_s4,lm_ts4) # lowest AIC =  lm_ts4, so tSCC for +4 parity

rm(df_pcr2, df_pcr3, df_pcr4)
gc()

# Conclusion: From AIC only (no further analysis done): we use tSCC

#--------------------------------------------------------------------
######### lm with continuous output (logSCC) ########################

# method: drop1(lm,test='F')

# lm full models: 
lm1 <- lm(logtSCC ~ logtSCCpre + MILKpre + C_MONTH + BREED + PARITY + BREED:PARITY + RESULT + TREATED + RESULT:TREATED, data = df_pcr)
drop1(lm1,test='F') # dropping C_month

lm2 <- lm(logtSCC ~ logtSCCpre + MILKpre + BREED + PARITY + BREED:PARITY + RESULT + TREATED + RESULT:TREATED, data = df_pcr)
drop1(lm2,test='F') # all good values, no non significant -> lm2 might be a good model

lm3 <- lm(logtSCC ~ logtSCCpre + MILKpre + BREED + PARITY + RESULT + TREATED, data = df_pcr) # no interaction
drop1(lm3,test='F') # all good values, no non significant -> lm4 might be a good model

lm4 <- lm(logtSCC ~ logtSCCpre + MILKpre + BREED + PARITY, data = df_pcr) # no interaction
drop1(lm4,test='F') # all good values, no non significant -> lm5 might be a good model

AIC(lm1,lm2) # lowest AIC is the best = lm2
AIC(lm2, lm3) # same, so lm2 and lm3 is the same...
AIC (lm2, lm4) # lowest AIC is the best = lm2 - so better with interaction

# Output: Not sure all these are needed. Check what S?ren uses
summary(lm1)            # OR= 
anova(lm1,test='Chisq') # df= , p value: 
confint(lm1)            # CI 95%: 
plot(lm1, which = 1) # residuals vs fitted
exp(coef(lm1))

summary(lm2)            # OR= 
anova(lm2,test='Chisq') # df= , p value: 
confint(lm2)            # CI 95%: 
plot(lm2, which = 1) # residuals vs fitted
exp(coef(lm2))

summary(lm3)            # OR= 
anova(lm3,test='Chisq') # df= , p value: 
confint(lm3)            # CI 95%: 
plot(lm3, which = 1) # residuals vs fitted
exp(coef(lm3))

summary(lm4)            # OR= 
anova(lm4,test='Chisq') # df= , p value: 
confint(lm4)            # CI 95%: 
plot(lm4, which = 1) # residuals vs fitted
exp(coef(lm4))

# conclusion for report:

# prediction plots:
ggPredict(lm2,interactive=TRUE) # ggPredict ???
effect_plot(lm2, pred = displ) # effect_plot ??
#predict(lm2,  interval = "confidence") #NO. just print prediction values...

#--------------------------------------------------------------------
######### Confounding lm model #####################################

# Breed and Parity. Adding one at a time

# start model:
lmcon <- lm(logtSCC ~ logtSCCpre, data = df_pcr)
summary(lmcon) # 0.05448
anova(lmcon)

# adding Parity to start model:
lmcon1 <- lm(logtSCC ~ logtSCCpre + PARITY, data = df_pcr)
summary(lmcon1) #Multiple R-squared:  0.06203,	Adjusted R-squared:  0.06202

# adding breed to start model:
lmcon2 <- lm(logtSCC ~ logtSCCpre + BREED, data = df_pcr)
summary(lmcon2) # 0.06203,	Adjusted R-squared:  0.06202

# check differences in effetc size with R squared

library(effectsize)
effectsize(lmcon)
effectsize(lmcon1)
effectsize(lmcon2)

#------------------------------------------------------------------
############# check confounding other method, continuous ##########
# first create tables 

# Alternative - Use exact logistic regression
elrm.dat <- data.frame(flock.size,feed,n.case,n.tot=n.control+n.case)

# please note: iter = 200000 means this will take a while, start by perhaps trying with 
# iter = 50000 and burnIn = 5000
elrm.salm <- elrm(formula = n.case/n.tot ~ flock.size + feed,
                  interest = ~flock.size + feed, iter = 2000000, burnIn=100000,dataset=elrm.dat)
summary(elrm.salm)
# Strong effect of both - further evidence of confounding.

#-----------------------------------------------------------------
########### Confounding check again, continuous #################

# checking confounding by model analysis:
# creating models we will be analyzing:
lm_non    <- lm(logSCC ~ logSCCpre, data=df_pcr) # basic uni model
lm_breed  <- lm(logSCC ~ logSCCpre + BREED, data=df_pcr) # only breed
lm_parity <- lm(logSCC ~ logSCCpre + PARITY, data=df_pcr) # only parity
lm_both   <- lm(logSCC ~ logSCCpre + PARITY + BREED, data=df_pcr) #breed + parity
lm_inter  <- lm(logSCC ~ logSCCpre + BREED + PARITY + BREED:PARITY, data=df_pcr) # interaction breed:parity


# Anova
anova(lm_breed,test='Chisq') # 
anova(lm_parity,test='Chisq')  # 
anova(lm_both,test='Chisq')
anova(lm_inter,test='Chisq') #  

AIC(lm_breed,lm_parity) # parity is lower 
AIC(lm_breed,lm_inter)  # inter is lower
AIC(lm_inter,lm_both)   # inter has the lowest

# size confounder:
summary(lm_non) # 
summary(lm_breed) # 
summary(lm_parity) # 
summary(lm_inter)

step(lm_inter, data=milk, direction="both")

# checking confounding with summaryBy function:
# Stratifying by one factor
# same as the boxplot output, just in numbers
summaryBy(logSCC  ~ BREED , data = df_pcr, 
          FUN = function(x) { c(m = mean(x,na.rm=T), s = sd(x,na.rm=T)) } )
summaryBy(logSCC  ~ PARITY , data = df_pcr, 
          FUN = function(x) { c(m = mean(x,na.rm=T), s = sd(x,na.rm=T)) } )

# instead of summaryBy, by can be used instead:
by(df_pcr$logSCCpre, df_pcr$BREED, summary)
by(df_pcr$logSCCpre, df_pcr$PARITY, summary)
by(df_pcr$logSCCpre, interaction(df_pcr$BREED,df_pcr$PARITY), summary)


#----------------------------------------------------------------------------
################## glm model, dict. output, IMI ###########################

glm1 <- glm(formula = IMI ~ logtSCCpre + MILKpre + C_MONTH + BREED + PARITY + 
              RESULT + RESULT:TREATED, 
            family = binomial, data = df_pcr)
drop1(glm1) # ALL significant
summary(glm1)
anova(glm1)
# a nicer output:
broom::tidy(glm1) 
# check the estimates: In the following output, after transforming from qlogis to p, we find how the variable affect in p the model in:
# estimate_plogis
glm1_no_intercept %>% 
  broom::tidy() %>% 
  mutate(estimate_plogis = plogis(estimate)) %>% 
  select(term, estimate, estimate_plogis, everything())
exp(cbind(OR=coef(glm1),confint(glm1)))

glm2 <- glm(formula = IMI ~ logtSCCpre + MILKpre + C_MONTH + BREED + PARITY + RESULT + TREATED + RESULT:TREATED, 
            family = binomial, data = df_pcr)
drop1(glm2) # ALL significant
summary(glm2)

glm2 <- glm(formula = IMI ~ logtSCCpre + MILKpre + SCC_MONTH + BREED + PARITY + 
              BREED:PARITY + RESULT + TREATED + RESULT:TREATED, 
            family = binomial, data = df_pcr)
drop1(glm2,test='F') # ALL significant

glm_test <- glm(formula = IMI ~ logtSCCpre + MILKpre + C_MONTH + BREED + PARITY + 
              BREED:PARITY + RESULT + TREATED + RESULT:TREATED, 
            family = binomial, data = df_test)
drop1(glm_test,test='F') # only RESULT:TREATED and logtSCCpre significant

#----------------------------------------------------------------------------
################## glm model, dict. output, PCR RESULT ######################



#-------------------------------------------------------------------------------
#################### glm confounding check ####################################
library(chest)

results1 <- chest_speedglm(
  crude = "IMI ~ logtSCCpre", xlist = c("PARITY", "BREED"),
  data = df_pcr
)
chest_plot(results1)
chest_forest(results1)

results_test <- chest_speedglm(
  crude = "IMI ~ logtSCCpre", xlist = c("PARITY", "BREED"),
  data = df_test
)
chest_forest(results_test)


