

# Maj Beldring Henningsen, majbh@sund.ku.dk

# visualizing SCC curves with wilmink

#-------------------------------------------------------
# Packages and settings:

library(tidyverse)
library(data.table)
library(plotly)
library(GGally)
library(tidymodels)
Sys.setlocale("LC_ALL","English") # data formatting
memory.size()            # Checking your memory size
memory.limit()           # Checking the set limit
memory.limit(size=56000) # suggest for 64 bit
options(stringsAsFactors = FALSE) # prevent factorizing caracters


#--------------------------------------------------------------------------------------------
#load data

load("M:/PCR_data/PCR_merge.RData") 
rm(df_all_dates, df_pcr); gc() # keep only curve data

# created animal level data
# 11 random cow IDs, holstein, perity 2, conventionel
data <- df_curve %>%
  mutate(logSCC = log(SCC)) %>%
  filter(BREED == "holstein") %>%
  filter(PARITY == 2) %>%
  filter(HERDTYPE == 1) %>%
  dplyr::select(DYR_ID, DIM, logSCC) %>%
  mutate(DIM = as.numeric(DIM)) %>%
  mutate(DYR_ID = as.factor(DYR_ID))


#-----------------------------------------------------------------------------------------------
#plot all cows

p <- ggplot(data = data, aes(x = DIM, y = logSCC, colour = DYR_ID)) +   geom_point()+
  theme_bw()+
  ylab("logSCC")+
  xlab("DIM")+
  theme(legend.position = "none")
p

#unique(data$CowId)
CowIds <- as.vector(unique(data$DYR_ID))
# CowIds.list <- split(CowIds, seq(length(CowIds)))

CowIds.initial.sample <- c(1012005613, 1012005808, 1016709051, 1017467338, 1017925966, 1018098868, 1007585294, 1007585365, 1007585508, 1007585757, 1007585837)

p <- ggplot(data = data %>% filter(CowId %in% CowIds.initial.sample), 
            aes(x = PeriodFinishDate, y = MilkYield, colour = CowId)) + 
  geom_point() +
  geom_smooth()+
  ylab("Milk yield (L for week)")+
  xlab("Date")+
  theme_bw()+  
  theme(legend.position = "none")
#p
s <- p + facet_wrap( ~ CowId)
s


CowIds.sample <- sort(c(852,1909,9855,9933,9924,9886,9926,1991,1982) )#removed low dim/early dryoff, 9772,9775'

#remove observations with milk yield NA
data <- data %>% 
  filter(!is.na(MilkYield))

# create calving and dryoff dates - nb assumes only one lactation of data
# and add milk yeild per day
# DaysInMilk variable is the days in milk for that weekly period! i.e. 0 to 7.
data <- data %>% 
  mutate(StartOfMilkSupply = PeriodFinishDate - DaysInMilk) %>% 
  mutate(EndOfMilkSupply = PeriodFinishDate -7 + DaysInMilk) %>% 
  mutate(MilkYieldPerDay = MilkYield/DaysInMilk)  #imperfect, date for end of period, not midpoint



summary_data <- data %>%    
  group_by(CowId) %>%  #group by lactation if multiple lactations
  summarise(CalvingDate = min(StartOfMilkSupply, na.rm=TRUE), 
            Dryoff = max(EndOfMilkSupply, na.rm=TRUE),
            MaxDIM = Dryoff - CalvingDate)

#join summary to original data
data <- data %>% 
  left_join(summary_data)

#create proper midpoint - WORK IN PROGRESS
data <- data %>% 
  # mutate(PeriodMidDate = mean(c(StartOfMilkSupply),  
  #                            (EndOfMilkSupply))) #this doesn't work, base mean function not vectorised
  mutate(
    MY.DATE1=as.Date(StartOfMilkSupply),
    MY.DATE2=as.Date(EndOfMilkSupply)) %>% 
  rowwise %>%
  mutate(PeriodMidDate=mean.Date(c(MY.DATE1,MY.DATE2))) %>% 
  #alternate mid date approach from:
  #https://stackoverflow.com/a/26590499/4927395
  #not quite right for start and end of lactation!
  # date 1 needs to be min of start and calving? 
  # date 2 needs to be max of end and dryoff? 
  mutate(DIM = as.numeric(PeriodFinishDate - CalvingDate))


p <- ggplot(data %>% filter(CowId %in% CowIds.sample), 
            aes(x = DIM, y = MilkYieldPerDay, fill = CowId, colour = CowId)) + 
  geom_point() +
  stat_smooth(method = 'nls', formula = y ~ a + b*exp(-.05*x) + c*x, se = FALSE, 
              method.args = list(start=c(a=31,b=-11,c=-.07), control=nls.control(maxiter=200)), colour = "green")+
  #stat_function(fun = fun.1)+
  geom_smooth()
#p
s <- p+ facet_wrap( ~ CowId)
s


nlsobject <- nls(formula = MilkYieldPerDay  ~ a + b*exp(d*DIM) + c*DIM, 
                 data %>% filter(CowId %in% c(852)),
                 start = c(a=31,b=-11,c=-.07, d=-.05))

summary(nlsobject)


#-------------------------------------------------------------------------------------------------------
#tidymodels approach to multiple nls/ols

tidymodel_data <- data %>%
  ungroup() %>% #was rowwise_df, so needed to ungroup(), https://stackoverflow.com/a/33292159/4927395
  select(c("CowId", "DIM", "MilkYieldPerDay")) %>%  #select only essential columns. Data to nest within CowID, because multiple observations for each CowID at each DIM
  nest(yields = c(MilkYieldPerDay, DIM)) %>%
  mutate(model = map(
    #         yields, ~ lm(MilkYieldPerDay ~ DIM, 
    yields, ~ nls(formula = MilkYieldPerDay  ~ a + b*exp(-.05*DIM) + c*DIM,
                  start = c(a=31,b=-11,c=-.07),
                  data = .x)))


#         yields, ~ nls(formula = MilkYieldPerDay  ~ a + b*exp(d*DIM) + c*DIM, 
#                   start = c(a=31,b=-11,c=-.07, d=-.05)),
# data = .x))

# Next, let's tidy() those models to get out the coefficients, 
# and adjust the p-values for multiple comparisons while we're at it.

slopes_db <- tidymodel_data %>%
  mutate(coefs = map(model, tidy)) %>%
  unnest(coefs) 
# %>%
#   filter(term == "year_ending") %>%
#   mutate(p.value = p.adjust(p.value))

pairs_data <- slopes_db %>% select(CowId, term, estimate) %>% 
  pivot_wider(names_from = term, values_from = estimate, id_cols = CowId  )

library(GGally)
ggpairs(pairs_data, columns = c("a","b","c"))