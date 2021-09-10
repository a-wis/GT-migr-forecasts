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

#hue_pal()[n] useful for colours

# tables for plotting clustered GTI values Fig 1####
pl.dati=GTIclust %>% 
  pivot_longer(cols = `2012`:`2019`,names_to="year") %>% 
  mutate(year=as.numeric(year)) %>% 
  right_join(data_mig) 
pl.datiav=GTIclust %>% 
  pivot_longer(cols = `2012`:`2019`,names_to="year") %>% 
  mutate(year=as.numeric(year)) %>%
  group_by(cluster,year) %>%
  summarise(cluster_mean=mean(value)) %>% ungroup()



#modelling relative standard errors ####
# lm(formula = log(IPSSEpc/(1-IPSSEpc)) ~ (Raw_IPS), data = data_mig %>% filter(year>2000)) %>% summary()
lm.mod=lm(formula = log(IPSSEpc/(1-IPSSEpc)) ~ (Raw_IPS)-1, 
          data = data_mig %>% filter(year>2000)) 
lm.mod0=lm(formula = log(IPSSEpc/(1-IPSSEpc)) ~ (Raw_IPS), 
           data = data_mig %>% filter(year>2000)) 





#### #simulation ####
#reading in functions
source("code/functions_modelling_plotting.R")
#simulation
# NOTE (important):
# The simulations were run in R 3.6.3 and rstan (Version 2.21.2, GitRev: 2e1f913d3ca3). Sometimes the simulation crashed RStudio with a cpp-related error (unreadable due to RStudio crash). To avoid that, I suggest running the simulation with rstan_options(auto_write = TRUE) option firstly only to generate .rds files (compiled models). After generating rds files, the simulation will likely crash. Then, the simulation can be re-run without any issues. 
library(rstan)
library(rstantools)
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

save(results022018_30, results022019_30, results022018_31, results022019_31, results022018_32, results022019_32,
     file="output/results_all.RData")
