# PCR_project

# Maj Beldring, majbh@sund.ku.dk
# UCPH 2020-2021

#-----------------------------------------
# overview of scripts

#  PCR_clean: Load and basic cleaning raw data
#  PCR_prepare: Steps between clean & merge; selecting PCR & teatment data
#  PCR_merge1: Merging scripts to full final data with dryoff data (dryoff data results in 1/5 data loss)
#  PCR_merge2: Merging scripts to full final data without dryoff data
#  PCR_descriptive: Descriptive analysis & population description on full data
#  PCR_visualizing: Visualizing full data and variables of interest
#  PCR_analysis: glm, lm and stan_glm on full data
#  PCR_model1: wilmink curve with nls
#  PCR_model2: Initial model testing with jags
#  PCR_test: Double randomized test data from full data; max 1000 observations
#  PCR_dryoff: Include all tests and treatments 150 days prior calving + descriptives
#  PCR_misc: Misc tests & analysis
