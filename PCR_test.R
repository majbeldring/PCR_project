
# Maj Beldring, majbh@sund.ku.dk

# PCR project, test data, double random test, max 1000 observations

# Packages and settings:
library(tidyverse)
library(data.table)
library(GGally)
library("ggpubr") # for ggscatter in correlation figure
library(rstatix) # for sample_by_n
library(gridExtra)
Sys.setlocale("LC_ALL","English") # data formatting

"-------------------------------------------------------------------"
# Loading data:

load("M:/data/PCR_merge.RData")

#-------------------------------------------------------------------
# Creating random test data

# using replace=TRUE, to make sure all breed and parities are equally represented
df_test <- sample_n_by(df_pcr, BREED, PARITY, size = 75, replace = TRUE)
df_test <- df_test %>% 
  distinct(DYR_ID, .keep_all = TRUE) %>% 
  # janitor::tabyl(BREED, PARITY)
  identity()

#---------------------------------------------------------------
# Distributions SCC and SCCpre

p1 <- ggplot(df_test, aes(x=SCC)) +
  geom_histogram(binwidth=.5, colour="darkblue", fill="white")
p2 <- ggplot(df_test, aes(x=SCCpre)) +
  geom_histogram(binwidth=.5, colour="darkred", fill="white")
p3 <- ggplot(df_test, aes(x=logSCC)) +
  geom_histogram(binwidth=.5, colour="darkblue", fill="lightblue")
p4 <- ggplot(df_test, aes(x=logSCCpre)) +
  geom_histogram(binwidth=.5, colour="darkred", fill="#FF9999")

grid.arrange(p1, p3, ncol=2) # SCCpost vs logSCCpost
grid.arrange(p2, p4, ncol=2) # SCCpre vs logtSCCpre

# Results:
# we get a gaussian distribution (normal) when we log transform

#-----------------------------------------------------------------
# Before any other analysis: check for tSCC or SCC:

lm <- lm(logSCC ~ logSCCpre, data = df_pcr) 
lm_t <- lm(logtSCC ~ logtSCCpre, data = df_pcr) 
AIC(lm,lm_t) # lowest AIC =  lm_t , so tSCC should be used

# Results: we only use the AIC, as we look at the whole model, and not just the variables
# df      AIC
# lm    3 3888.228
# lm_t  3 3811.280

#-------------------------------------------------------------------
# Correlation 

# visualize first correlation by pairs:
ggpairs(data = df_test,
        mapping = aes(color = PARITY),
        columns = c("logSCC", "logSCCpre", "MILKpre", "PARITY", "BREED"),
        upper = list(continuous = wrap("cor", size = 2.5))
)

# correlation between logtSCC and logtSCCpre, including visualization
cor(df_test[,c('logtSCC','logtSCCpre')]) # correlation coefficient r=0.24, p=3.6e-16
cor.test(df_test$logtSCC, df_test$logtSCCpre, nethod="pearson", conf.level= 0.95) # for full results, incl. CI
p5 <- ggscatter(df_test, x = "logtSCCpre", y = "logtSCC", color= "PARITY", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "logtSCC pre calving", ylab = "logtSCC post calving")

# correlation between logtSCC and logtSCCpre, NON Treated only
df_test1 <- df_test %>% filter(TREATED == 0)
cor(df_test1[,c('logtSCC','logtSCCpre')]) # correlation coefficient r=0.29, p=1.4e-07
p6 <- ggscatter(df_test1, x = "logtSCCpre", y = "logtSCC", color= "PARITY",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "logtSCC pre calving", ylab = "logtSCC post calving non treated only")

grid.arrange(p5, p6, ncol=2) # regression

# correlation full data:
cor(df_pcr[,c('logtSCC','logtSCCpre')]) 
df_pcr1 <- df_pcr %>% filter(TREATED == 0)
cor(df_pcr1[,c('logtSCC','logtSCCpre')])

# correlation between logtSCC and MILKpre, including visualization
cor(df_test[,c('logtSCC','MILKpre')]) # scc vs milkpre, r=0.003, p=0.91 : not significant..
ggscatter(df_test, x = "MILKpre", y = "logtSCC", color= "PARITY",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Milk pre calving", ylab = "logtSCC post calving")

# Results pearssons correlation test: logSCC vs logSCCpre, where r=0: no linear relationsship
# tSCC vs tSCCpre: r=0.25 (and 0.29 for non treated): So we have a weak linear relationsship
# no relationsship found between MILK pre and SCCpost, also shown by the p value..
# the latter is most likely since we have applied tSCC rather than SCC

# other scatterplots with regression lines of interest:

# logSCC post (NOT t) vs MILK pre: # r=-0.065, p=0.033
ggscatter(df_test, x = "MILKpre", y = "logSCC", color= "PARITY",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Milk pre calving", ylab = "logSCC post calving")


# "pairs" scatterplot logSCC vs prelogSCC, grouped by breed and parity:
ggplot(data = df_test,
       mapping = aes(x = logSCCpre, y = logSCC)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "logSCC pre dry-off", y = "logSCC post dry-off") +
  theme_bw() +
  theme(legend.title = element_blank()) +
  facet_grid(PARITY ~ BREED)

#--------------------------------------------------------------------
# TABLES 

table1_test <- table(df_test$BREED,df_test$PARITY) # A = rows, B = columns
table1_test # BREED vs PARITY
table2_test  <- table(df_test$RESULT, df_test$TREATED) # A = rows, B = columns
table2_test # PCR results vs treated animals
table3_test <- table(df_test$RESULT, df_test$IMI) # A = rows, B = columns
table3_test # pcr results vs new IMI

# visualizing the result from the three table

# RESULTS: 65 % of PCR NEG cows are treated at dry-off, and 24% of the positive are not treated
df_test %>% 
  group_by(RESULT) %>% 
  count(TREATED) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = RESULT, y = prop)) +
  geom_col(aes(fill = TREATED), position = "dodge") +
  geom_text(aes(label = scales::percent(prop), 
                y = prop, 
                group = TREATED),
            position = position_dodge(width = 0.9),
            vjust = 1.5)

# RESULTS: POS ang NEG PCR results in parity groups: Most POS in parity 4, and less in PARITY 2
df_test %>% 
  group_by(PARITY) %>% 
  count(RESULT) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = PARITY, y = prop)) +
  geom_col(aes(fill = RESULT), position = "dodge") +
  geom_text(aes(label = scales::percent(prop), 
                y = prop, 
                group = RESULT),
            position = position_dodge(width = 0.9),
            vjust = 1.5)

# RESULTS: POS ang NEG PCR results in parity groups: Most POS in BREED 4, and less in BREED 2
df_test %>% 
  group_by(BREED) %>% 
  count(RESULT) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = BREED, y = prop)) +
  geom_col(aes(fill = RESULT), position = "dodge") +
  geom_text(aes(label = scales::percent(prop), 
                y = prop, 
                group = RESULT),
            position = position_dodge(width = 0.9),
            vjust = 1.5)

# RESULTS: 32.7 % of all cow in Parity 4 has IMI post calving, 25% in parity3, and 19.3% in parity 2
df_pcr %>% 
  group_by(PARITY) %>% 
  count(IMI) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = PARITY, y = prop)) +
  geom_col(aes(fill = IMI), position = "dodge") +
  geom_text(aes(label = scales::percent(prop), 
                y = prop, 
                group = IMI),
            position = position_dodge(width = 0.9),
            vjust = 1.5) 

# RESULTS: More or less equally distribution of IMI in the 5 parity groups post calvings
df_pcr %>% 
  group_by(BREED) %>% 
  count(IMI) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = BREED, y = prop)) +
  geom_col(aes(fill = IMI), position = "dodge") +
  geom_text(aes(label = scales::percent(prop), 
                y = prop, 
                group = IMI),
            position = position_dodge(width = 0.9),
            vjust = 1.5)

#------------------------------------------------------------
# population histograms..
# these three plots are good for big data...

# histogram by breed and parity: Better for big data!!!
ggplot(data = df_test, mapping = aes(x = logSCC, fill = PARITY)) +
  geom_histogram(alpha = 0.5, bins = 50) +
  labs(x = "logSCC", y = "Count",
       title = "logSCC by BREED and PARITY") +
  facet_grid(. ~ BREED) +
  theme_bw()

ggplot(data = df_pcr, mapping = aes(x = logSCC, fill = PARITY)) +
  geom_histogram(alpha = 0.5, bins = 50) +
  labs(x = "logSCC", y = "Count",
       title = "logSCC by BREED and PARITY") +
  facet_grid(. ~ BREED) +
  theme_bw()

# as above but no fill=breed: Better for big data
ggplot(data = df_test, mapping = aes(x = logSCC, fill = BREED)) +
  geom_histogram(alpha = 0.5, bins = 50) +
  labs(x = "logSCC", y = "Count",
       title = "logSCC by BREED and PARITY") +
  facet_grid(. ~ PARITY) +
  theme_bw()

# Histograms over population, breed and parity stratified
# doesn't make much sense here, as we have more or less chosen how many
# animals we want in each group. But good for big data..
p15 <- ggplot(df_pcr, aes(x=BREED))+
  geom_bar(color="darkgreen", fill="lightgreen") +
  labs(title = "Animals in each BREED and Parity group") +
  facet_wrap( ~ PARITY)

p16 <- ggplot(df_test, aes(x=BREED))+
  geom_bar(color="darkblue", fill="lightblue") +
  labs(title = "Animals in each BREED and Parity group") +
  facet_wrap( ~ PARITY)
grid.arrange(p15, p16, ncol=2) # regression

#--------------------------------------------------------------
# boxplots - All boxplots are also good for big data
# could be done better if treated animals where excluded for 4 weeks or so..

# SCCpre vs Control month
ggplot(df_pcr, aes(x=as.factor(SCC_MONTH), y=logSCCpre)) +
  geom_boxplot() +
  xlab('Month') +
  ylab('logSCC pre calving') +
  labs(title = "Average SCC level vs Month of control")
#SCCpost vs Calving Month
ggplot(df_pcr, aes(x=as.factor(C_MONTH), y=logSCC)) +
  geom_boxplot() +
  xlab('Month') +
  ylab('log SCC pre calving') +
  labs(title = "Average SCC level vs Month of calving")
# as above with full data
ggplot(df_pcr, aes(x=as.factor(C_MONTH), y=logSCCpre)) +
  geom_boxplot() +
  xlab('Month') +
  ylab('log SCC pre calving') +
  labs(title = "Average SCC level vs Month of calving")

# boxplot SCC pre calving, pathogen specific, logSCC
# POS vs NEG cases on the X axis:
ggplot(df_pcr, aes(x=PATHOGEN, y=logSCCpre, col=RESULT)) +
  geom_boxplot()
# IMI 
ggplot(df_pcr, aes(x=PATHOGEN, y=logSCCpre, col=IMI)) +
  geom_boxplot()
# as above with full data..
ggplot(df_pcr, aes(x=PATHOGEN, y=logSCCpre, col=RESULT)) +
  geom_boxplot()
# IMI 
ggplot(df_pcr, aes(x=PATHOGEN, y=logSCCpre, col=IMI)) +
  geom_boxplot()

# SCC boxplot for each parity 
# Stratify by breed and colour by parity. logSCC:
ggplot(df_pcr, aes(x=PARITY, y=logSCCpre, col=PARITY)) +
  geom_boxplot() +
  facet_wrap( ~ BREED)
# Stratify by parity and colour by breed, logSCC:
ggplot(df_pcr, aes(x=BREED, y=logSCCpre, col=BREED)) +
  geom_boxplot() +
  facet_wrap( ~ PARITY)

#--------------------------------------------------------------------
# extra plots: Scatter with regression where confounding can be identified

#------------------------------------------------------------------------
# scatterplot + regression: Here we also see confounding!! As lines are not parallel

# vs SCCpre, regression for results and grouped by parity
ggplot(data = df_test,
       mapping = aes(x = logSCCpre, y = logSCC, fill = RESULT)) +
  geom_point(aes(colour = PARITY)) +
  geom_smooth(method = "lm") +
  labs(x = "logSCC pre dry-off", y = "logSCC post dry-off") +
  theme_bw()

# vs MILKpre
ggplot(data = df_test,
       mapping = aes(x = MILKpre, y = logtSCC, fill = PARITY)) +
  geom_point(aes(colour = BREED)) +
  geom_smooth(method = "lm") +
  labs(x = "MILK pre dry-off", y = "logSCC post dry-off") +
  theme_bw()

# vs MILKpre, Grouped by results
ggplot(data = df_test,
       mapping = aes(x = MILKpre, y = logSCC, fill = RESULT)) +
  geom_point(aes(colour = BREED)) +
  geom_smooth(method = "lm") +
  labs(x = "MILK pre dry-off", y = "logSCC post dry-off") +
  theme_bw()

# Scatterplot, grouped by Parity and BREED, RESULT and TREATED
# shows as breed 4 and 5 are more similar. Maybe these can be merged...
ggplot(df_test, aes(x=logSCCpre, y=logSCC, col=BREED)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap( ~ PARITY)

ggplot(df_test, aes(x=logSCCpre, y=logSCC, col=PARITY)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap( ~ RESULT)

ggplot(df_test, aes(x=logSCCpre, y=logSCC, col=PARITY)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap( ~ TREATED)

# there is something about confounding if the lines are parallel.....

###########################################################################
######## END description and visualization. Now lm and glm can be done
###########################################################################

#----------------------------------------------------------------------------
# glm model, dict. output, IMI

# glm1: test data with IMI as output..
glm1 <- glm(formula = IMI ~ logtSCCpre + MILKpre + C_MONTH + BREED + PARITY + 
              RESULT + RESULT:TREATED, 
            family = binomial, data = df_test)
drop1(glm1) # drop lowest: Breed; after drop new will be AIC 1172.0

# drop breed:
glm2 <- glm(formula = IMI ~ logtSCCpre + MILKpre + BREED + PARITY + 
              RESULT + RESULT:TREATED, 
            family = binomial, data = df_test)
drop1(glm2) # lowest: C_month: 1170.9

# drop C_month:
glm3 <- glm(formula = IMI ~ logtSCCpre + MILKpre + PARITY + BREED +
              RESULT + RESULT:TREATED, 
            family = binomial, data = df_test)
drop1(glm3) # Remove non: this will give the best AIC value..

summary(glm3) # AIC overall: AIC: 1172, Fischer 4. Multiple Breed not significant
anova(glm3, test='LR') # only SCC, milk and interaction gives significance

# check the estimates: In the following output, after transforming from qlogis to p, we find how the variable affect in p the model in:
# update(glm1, .~.-1) -> glm1_no_intercept # if we want to remove intercept
glm3 %>% 
  broom::tidy() %>% 
  mutate(estimate_plogis = plogis(estimate)) %>% 
  select(term, estimate, estimate_plogis, everything())
plogis(confint(glm3)) # no CI crosses 1, so there is significance between the groups
exp(cbind(OR=coef(glm3),confint(glm3))) # backtransform give other result.

# result: Full model is: glm3
# results:
# Breed and C_month removed
# No CI values include 0 or 1.
# Only logSCC have positive impact on IMI - meaning the higher logSCC the more chance of IMI


# repeating with all data;

glm1_all <- glm(formula = IMI ~ logtSCCpre + MILKpre + C_MONTH + BREED + PARITY + 
              RESULT + RESULT:TREATED, 
            family = binomial, data = df_pcr)
drop1(glm1_all) # drop lowest: C_month

glm2_all <- glm(formula = IMI ~ logtSCCpre + MILKpre + BREED + PARITY + 
                  RESULT + RESULT:TREATED, 
                family = binomial, data = df_pcr)
drop1(glm2_all) # drop lowest: C_month

# checing with stan_glm
library(rstanarm)



#---------------------------------------------------------------------
# glm1: test data with PCR as output..

glm5 <- glm(formula = RESULT ~ logtSCCpre + PATHOGEN + MILKpre + BREED + PARITY, 
            family = binomial, data = df_test)
glm6 <- glm(formula = RESULT ~ logtSCCpre + PATHOGEN + MILKpre + BREED + PARITY, 
            family = binomial, data = df_pcr)
drop1(glm6) # ALL significant
summary(glm2)
anova(glm2)
# a nicer output:
broom::tidy(glm2) 
# check the estimates: In the following output, after transforming from qlogis to p, we find how the variable affect in p the model in:
# estimate_plogis
update(glm2, .~.-1) -> glm2_no_intercept
glm6 %>% 
  broom::tidy() %>% 
  mutate(estimate_plogis = plogis(estimate)) %>% 
  select(term, estimate, estimate_plogis, everything())
plogis(confint(glm6)) # no CI crosses 1, so there is significance between the groups

exp(cbind(OR=coef(glm2),confint(glm2))) # not sure how to interpret this result.

library(bayesplot)
theme_set(bayesplot::theme_default())




stan_test <- stan_glm(RESULT ~ logtSCCpre + PATHOGEN + MILKpre + BREED + PARITY,
                        data = df_pcr, family = binomial(link = "logit"))
summary(stan_test)
prior_summary(stan_test)

pp_check(stan_test, plotfun = "stat", stat = "mean")
pp_check(stan_test, plotfun = "dens_overlay")

