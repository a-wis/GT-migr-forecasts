############################################
### analysis of the GTI data and migration #
### analysis of forecasts
### plotting and tables with results
############################################
### created: 10/12/2020
### modified: 09/09/2021
############################################
### Arkadiusz Wisniowski
############################################


##### libraries
# library(tidyverse)
# library(stargazer)


#modelling relative standard errors ####
#model with logs - not used
# lm(formula = log(IPSSEpc/(1-IPSSEpc)) ~ (Raw_IPS), data = data_mig %>% filter(year>2000)) %>% summary()
#Models in Table A4
#Model(B)
lm.mod=lm(formula = log(IPSSEpc/(1-IPSSEpc)) ~ (Raw_IPS)-1, 
          data = data_mig %>% filter(year>2000)) 
#Model (A)
lm.mod0=lm(formula = log(IPSSEpc/(1-IPSSEpc)) ~ (Raw_IPS), 
           data = data_mig %>% filter(year>2000)) 



#### simulation ####

# README (important): ####
# The simulations were run in R 3.6.3 and rstan (Version 2.21.2, GitRev: 2e1f913d3ca3). Sometimes the simulation crashed RStudio with a cpp-related error (unreadable due to RStudio crash). To avoid that, the code generates compiled models in .rds files with rstan_options(auto_write = TRUE) option loaded. Alternatively, simulations can be run without pre-compiling  but crashes may occur. After generating rds files, the simulation can be run without issues. 
#If the simulations/rstan do not work, it might be because of conflicts with newer versions of package withr: rstan Version 2.21.2 does not work if withr>2.2.0 is loaded. This should not be a problem when using rstan 2.26:  
#https://discourse.mc-stan.org/t/new-error-cleanup-makevar-old-argument-rmu-is-missing-with-no-default/18137/75


library(rstan)
library(rstantools)

# compiling models to avoid crashing of the simulation
# warning: the below commands stan_model may not work in rstan 2.21.2 when withr >2.2.0:
rstan_options(auto_write = TRUE)

stan_model(file = "models/tsmodel011.stan")
stan_model(file = "models/tsmodel021.stan")
stan_model(file = "models/tsmodel011rw.stan")
stan_model(file = "models/tsmodel021rw.stan")
##################################################


#reading in functions
source("code/functions_modelling_plotting.R")

#data starting 2013: practically 2014 because of a lag
results022019_30=forecasts_cl1(data=data_mig2,
                               year.st=2012,
                               year.data=2019)
 save(results022019_30,file="output/results022019_30.RData")
results022018_30=forecasts_cl1(data=data_mig2,
                               year.st=2012,
                               year.data=2018)
 save(results022018_30,file="output/results022018_30.RData")
 save(results022018_30, results022019_30,
      file="output/results_30.RData")
 
#data starting 2012: added 0-timepoint-obs for y 
results022019_31=forecasts_cl1(data=data_mig2,
                               year.st=2011,
                               year.data=2019)
save(results022019_31,file="output/results022019_31.RData")
results022018_31=forecasts_cl1(data=data_mig2,
                               year.st=2011,
                               year.data=2018)
save(results022018_31,file="output/results022018_31.RData")
save(results022018_31, results022019_31,
     file="output/results_31.RData")
#data starting 2013 but using differences, not levels
results022019_32=forecasts_cl1(data=data_mig2 %>% 
                                 select(-value) %>% 
                                 rename(value=GTI_diff),
                               year.st=2012, 
                               year.data=2019)
save(results022019_32,file="output/results022019_32.RData")
results022018_32=forecasts_cl1(data=data_mig2 %>% 
                                 select(-value) %>% 
                                 rename(value=GTI_diff),
                               year.st=2012, 
                               year.data=2018)
save(results022018_32, results022019_32,
     file="output/results_32.RData")

# to save all results of the simulation run the below
# save(results022018_30, results022019_30, results022018_31, results022019_31, results022018_32, results022019_32,
     # file="output/results_all.RData")
