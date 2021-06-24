
# Maj Beldring, majbh@sund.ku.dk
# Descriptive analysis for PCR project

# 2x2 tables etc
# (curves/ecdf etc are found in PCR_curves)

# MISSING: ecdf plot (done in PCR_model1)

#-------------------------------------------------------
# Packages and settings:

library(tidyverse)
library(data.table)
library(GGally)
Sys.setlocale("LC_ALL","English") # data formatting

# Loading data:
load("M:/PCR_data/PCR_merge.RData")



#-------------------------------------------------------------------
# TEST DATA VISUALIZATION 


#df_test <- sample_n_by(df_test1, BREED, size = 300, replace = FALSE)
# using replace=TRUE, to make sure all breed and parities are equally represented
df_test <- sample_n_by(df_pcr, BREED, PARITY, size = 75, replace = TRUE)
df_test <- df_test %>% 
  distinct(DYR_ID, .keep_all = TRUE) %>% 
  # janitor::tabyl(BREED, PARITY)
  identity()

# pairs:
ggpairs(data = df_test,
        mapping = aes(color = PARITY),
        columns = c("logSCC", "logSCCpre", "MILKpre", "PARITY", "BREED"),
        upper = list(continuous = wrap("cor", size = 2.5))
)

# scatterplot + regression: Here we also see confounding!! As lines are not parallel
ggplot(data = df_test,
       mapping = aes(x = logSCCpre, y = logSCC, fill = PARITY)) +
  geom_point(aes(colour = BREED)) +
  geom_smooth(method = "lm") +
  labs(x = "logSCC pre dry-off", y = "logSCC post dry-off") +
  theme_bw()

ggplot(data = df_test,
       mapping = aes(x = MILKpre, y = logSCC, fill = PARITY)) +
  geom_point(aes(colour = BREED)) +
  geom_smooth(method = "lm") +
  labs(x = "MILK pre dry-off", y = "logSCC post dry-off") +
  theme_bw()

# scatterplot grouped by parity and breed:
ggplot(data = df_test,
       mapping = aes(x = logSCCpre, y = logSCC)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "logSCC pre dry-off", y = "logSCC post dry-off") +
  theme_bw() +
  theme(legend.title = element_blank()) +
  facet_grid(PARITY ~ BREED)

# Scatterplot, grouped by Parity and BREED, RESULT and TREATED
# there is something about confounding if the lines are parallel.....
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





#--------------------------------------------------------------------
# table: PCR results major pathogens vs treatments;


table_treated <- table(df_pcr$RESULT, df_pcr$TREATED) # A = rows, B = columns
table_treated
table_IMI <- table(df_pcr$RESULT, df_pcr$IMI) # A = rows, B = columns
table_IMI

table_test<- table(df_test$BREED,df_test$PARITY) # A = rows, B = columns
table_test




#---------------------------------------------------------------------
# table: TEAT vs IMI


table_teat <- table(df_pcr$TEAT, df_pcr$IMI) # A = rows, B = columns
table_teat


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


# boxplot, SCC_pre
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




#------------------------------------------------------------
# BIG DATA various plots


# distributions SCC and SCCpre - we see here why log transforming SCC
ggplot(df_pcr, aes(x=SCC)) +
  geom_histogram(binwidth=.5, colour="darkblue", fill="white")
ggplot(df_pcr, aes(x=SCCpre)) +
  geom_histogram(binwidth=.5, colour="darkred", fill="white")
# log, logSCC and logSCCpre:
ggplot(df_pcr, aes(x=logSCC)) +
  geom_histogram(binwidth=.5, colour="darkblue", fill="lightblue")
ggplot(df_pcr, aes(x=logSCCpre)) +
  geom_histogram(binwidth=.5, colour="darkred", fill="#FF9999")


# histogram by breed and parity
ggplot(data = df_pcr, mapping = aes(x = logSCC, fill = PARITY)) +
  geom_histogram(alpha = 0.5, bins = 50) +
  labs(x = "logSCC", y = "Count",
       title = "logSCC by BREED and PARITY") +
  facet_grid(. ~ BREED) +
  theme_bw()

# as above but no fill=breed
ggplot(data = df_pcr, mapping = aes(x = logSCC, fill = BREED)) +
  geom_histogram(alpha = 0.5, bins = 50) +
  labs(x = "logSCC", y = "Count",
       title = "logSCC by BREED and PARITY") +
  facet_grid(. ~ PARITY) +
  theme_bw()

# Histograms over population, breed and parity stratified
ggplot(df_pcr, aes(x=BREED))+
  geom_bar(color="darkgreen", fill="lightblue") +
  labs(title = "Animals in each BREED and Parity group") +
  facet_wrap( ~ PARITY)

# PCR POS and NEG overall:
df_pcr %>% 
  group_by(TREATED) %>% 
  count(RESULT) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = TREATED, y = prop)) +
  geom_col(aes(fill = RESULT), position = "dodge") +
  geom_text(aes(label = scales::percent(prop), 
                y = prop, 
                group = RESULT),
            position = position_dodge(width = 0.9),
            vjust = 1.5)

df_pcr %>% 
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

# PCR POS and NEG in each PARITY:
df_pcr %>% 
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

# PCR POS and NEG in each BREED:
df_pcr %>% 
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

# IMI in each PARITY:
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
# IMI in each BREED:
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





#--------------------------------------------------------------
# boxplot BIG DATA 

# SCCpre vs Control month
ggplot(df_pcr, aes(x=as.factor(SCC_MONTH), y=logSCCpre, col=SCC_MONTH)) +
  geom_boxplot() +
  xlab('Month') +
  ylab('logSCC pre calving') +
  labs(title = "Average SCC level vs Month of control")
#SCCpre vs Calving Month
ggplot(df_pcr, aes(x=as.factor(C_MONTH), y=logSCCpre, col=C_MONTH)) +
  geom_boxplot() +
  xlab('Month') +
  ylab('log SCC pre calving') +
  labs(title = "Average SCC level vs Month of calving")

# boxplot SCC pre calving, pathogen specific, logSCC
# POS vs NEG cases on the X axis:
p10 <- ggplot(df_pcr, aes(x=PATHOGEN, y=logSCCpre, col=RESULT)) +
  geom_boxplot()
p11 <- ggplot(df_test, aes(x=PATHOGEN, y=logSCCpre, col=RESULT)) +
  geom_boxplot()
grid.arrange(p10, p11, ncol=2) # regression

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



