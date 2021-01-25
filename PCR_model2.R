#--------------------------------------------------------
# Maj Beldring Henningsen, majbh@sund.ku.dk

# JAGS model testing
# inspiration from ABME-06; imperfect_cows and cow_raising

#-------------------------------------------------------
# Packages and settings:

library(tidyverse)
#library(reshape2)
library(data.table)
#library(plotly)
#library(GGally)
#library(ggExtra)
#library(ggalluvial)
Sys.setlocale("LC_ALL","English") # data formatting

#------------------------------------------------------
# 1: Loading data:

# Load the data and look at it:
####### missing. Chech imperfect cows data

# date description:
# SCC (logit scale): continuous value for each animal and should be used as a linear effect
# PCR Sensitivity (Se) is beta(?, ?) and Specificity (Sp) is beta(?, ?)
# PCR result, positive if smaller than 73
# Priors for PCR test? beta(?, ?) and Sp is beta(?, ?)
# Milk: continuous value for each animal and should be used as a linear effect 

# Covariates:
# Breed, which is a number for each animal (1=Holstein, 2=Jersey, 3=Angus) and should be used as a fixed effect
# Brugsart

logit <- plogis
ilogit <- qlogis
logit(2)
ilogit(0.88)
# And Monte Carlo integration to look at distributions of transformed priors:
S <- 10^4
priorsamples <- rnorm(S,0,sd=10)
hist(priorsamples)
hist(logit(priorsamples))

str(imperfect_cows)
summary(imperfect_cows)

# binary data / numbers:
table(brugsart)
table(breed)
# table(PCR) # if POS/NEG if added

# continuous data:
summary(SCC_pre)
summary(SCC_post)
summary(PCR_VALUE)

# We will also use N as usual, and NTests which is the number of tests per animal:
N <- nrow(TestResult)
NTests <- 2

# Here is the simplest model, ignoring covariates:

modeltext1 <- '
model{
	
	for(i in 1:N){		
		for(t in 1:NTests){
			# Both tests are a Bernoulli trial with probability specific to this animal and test combination:
			TestResult[i,t] ~ dbern(obs.probability[i,t])
			# The probability for this animal and test depends on this animal being infected or not, and the test sensitivity and specificity:
			obs.probability[i,t] <- (infected[i]*sensitivity[t]) 
							+ ((1-infected[i])*(1-specificity[t]))
		}
		
		# This animal being infected just depends on the prevalence:
		infected[i] ~ dbern(prevalence)
	}
	
	# Our Se/Sp priors as given above:
	sensitivity[1] ~ dbeta(75, 25)
	specificity[1] ~ dbeta(90, 10)
	sensitivity[2] ~ dbeta(4, 1)
	specificity[2] ~ dbeta(5, 1)	
	
	# A reference prior for prevalence (a probability):
	prevalence ~ dbeta(1, 1)
	
	#data# N, NTests, TestResult, 
	#monitor# sensitivity, specificity, prevalence, deviance
	#inits# sensitivity, specificity, prevalence
	
}
'

# The initial values we need:
sensitivity <- list(chain1=c(0.05,0.95), chain2=c(0.95,0.05))
specificity <- list(chain1=c(0.95,0.05), chain2=c(0.05,0.95))
prevalence <- list(0.05, 0.95)

library(runjags)
# The model is quite complicated and therefore slow, so let's parallelise:
results1 <- run.jags(modeltext1, n.chains=2, method='parallel')
# Notice that we get a warning that runjags has set random number generators for us - don't worry about that!  If they annoy you turn them off:
runjags.options(rng.warning=FALSE)
# We could also use:
# results1 <- run.jags(modeltext1, n.chains=2, method='rjparallel')
# Or:
# results1 <- run.jags(modeltext1, n.chains=2, method='snow')
# We can also use method= with the extend.jags function to change the method between model runs
# See the help file for the difference. You can also specify n.sims to explicitly control the number of processors used (usually the number of chains) 

# Check traceplots and summary statistics as always and extend as necessary:
plot(results1, 'trace')
results1

# This model compiles and runs, but you might find that it doesn't always converge!
# If it doesn't converge, what does the deviance look like in the two chains?
# It would probably be a good idea to use more than 2 chains for this...


# Note that you could replace the line:
# prevalence ~ dbeta(1, 1)
# with:
# logit(prevalence) <- intercept
# intercept ~ dnorm(0, 10^-6)
# But this second prior may cause problems - compare:
curve(dbeta(x,1,1))
# with (Tip:  plogis is an inverse logit function, qlogis is a logit function):
samples <- rnorm(10^4, 0, sd=1000)
hist(plogis(samples))
# Notice that this prior is informative towards values very near 0 and 1 on the probability scale
# Let's try this prior instead:
samples <- rnorm(10^4, 0, sd=2)
hist(plogis(samples))
# This is possibly more reasonable, and arguably less informative on this scale
# -> There is no such thing as an uninformative prior!!!!!
