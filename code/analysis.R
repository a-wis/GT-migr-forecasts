############################################
### AW coding the trend in GTI detrended ###
############################################
### created: 10/12/2020
### modified: 18/05/2021
############################################
### Arkadiusz Wisniowski
############################################


##### libraries
library(xlsx)
library(forecast)
library(tsoutliers)
library(tseries)
library(readxl)
library(tidyverse)
library(rstan)
library(ggthemes)
require(ggpubr)
library(scales) # useful parse_format() function
#hue_pal()[n] useful for colours


#plotting clustered GTI values
pl.dati=GTIclust %>% pivot_longer(cols = `2012`:`2019`,names_to="year") %>% mutate(year=as.numeric(year)) %>% right_join(data_mig) 
pl.datiav=GTIclust %>% pivot_longer(cols = `2012`:`2019`,names_to="year") %>% mutate(year=as.numeric(year)) %>%
  group_by(cluster,year) %>%
  summarise(cluster_mean=mean(value)) %>% ungroup()


# Figure 1: trends ####
pl.dat=  ggplot(pl.dati) + 
  geom_text(aes(x=2007,y=57,label="Romania joins the EU"), angle=90,nudge_x = -0.2, colour="grey25",size=4.5,family="Palatino") +
  geom_vline(xintercept = 2007,colour="darkgrey",linetype="dashed") +
  geom_text(aes(x=2012,y=50,label="Latest update to Google Trends"), angle=90,nudge_x = -0.25, colour="grey25",size=4.5,family="Palatino") +
  geom_vline(xintercept = 2011.98,colour="darkgrey",linetype="dashed") +
  
  geom_vline(xintercept = 2016.475,colour="darkgrey",linetype="dashed") +
  geom_line(data=. %>% filter(cluster!="all"),aes(x=year,y=value,group=name,colour=cluster,linetype=cluster),size=1,alpha=0.3) +
  # facet_wrap(year~.,nrow = 4) +
  # geom_smooth(data=. %>% filter(cluster!="all"),aes(x=year,y=value,group=cluster,colour=cluster, fill=cluster,linetype=cluster),method="loess",size=1.5,alpha=0.3) +
  # geom_smooth(aes(x=year,y=value,group=cluster,colour=cluster, linetype=cluster),method="loess",size=2.5,alpha=0.9,se=F) +
  scale_x_continuous(expand = c(0,0.2)) +
  scale_linetype_manual(values=c("solid","dashed", "twodash", "longdash" )) +
  geom_line(data=pl.datiav,aes(x=year,y=cluster_mean,colour=cluster,linetype=cluster), size=2) +
    geom_point(aes(x=year,y=exp(Raw_IPS), shape="immigration (IPS)"),colour="blue",size=4.5, alpha=0.9) +
  geom_text(aes(x=2016.475,y=15,label="\"Brexit\" referendum"), angle=90,nudge_x = -0.25, colour="grey25",size=4.5,family="Palatino") +
  theme_bw() +
  theme(axis.text.x = element_text(size=13),
        axis.title = element_text(size=12),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "right",
        legend.key.size = unit(c(1.1),units = "cm")
  ) +
  labs(y="value (Google Trend Index | immigration in 1,000 persons)") +
  scale_shape_discrete(name="") 
pl.dat
ggsave(filename = "Graphs/GTI_Mig_ts.pdf",device = "pdf",plot = pl.dat,width = 10,height=5.5)
  


#Figure A1: model for IPS SE vs observed IPS ####
# to create a prior for sigma of IPS data
pl.ipsse=ggplot(data=data_an) + 
  geom_point(aes(x=log(Raw_IPS),y=log(IPSSEpc/(1-IPSSEpc)))) +
  geom_smooth(aes(x=log(Raw_IPS),y=log(IPSSEpc/(1-IPSSEpc)),colour="Model (2)",fill="Model (2)"),method="lm",formula=y~x-1,alpha=0.2) +
  geom_smooth(aes(x=log(Raw_IPS),y=log(IPSSEpc/(1-IPSSEpc)),colour="Model (1)",fill="Model (1)"),method="lm",formula=y~x,alpha=0.1) +
  geom_text(aes(x=log(Raw_IPS),y=log(IPSSEpc/(1-IPSSEpc)),label=lubridate::year(Date)),size=3,nudge_x = 0.1,nudge_y = .05) +
  theme_bw() +
  theme(axis.text.x = element_text(size=13),
        axis.title = element_text(size=12),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14)) +
  labs(x="IPS point estimate (natural logarithm)",
       y="logit(IPS relative standard error", colour="Fit",fill="Fit")
pl.ipsse

ggsave(filename = "Graphs/IPS_rse.pdf",device = "pdf",plot = pl.ipsse,width = 8,height=5.5)

#modelling relative standard errors ####
# lm(formula = log(IPSSEpc/(1-IPSSEpc)) ~ (Raw_IPS), data = data_mig %>% filter(year>2000)) %>% summary()
lm.mod=lm(formula = log(IPSSEpc/(1-IPSSEpc)) ~ (Raw_IPS)-1, data = data_mig %>% filter(year>2000)) 
lm.mod0=lm(formula = log(IPSSEpc/(1-IPSSEpc)) ~ (Raw_IPS), data = data_mig %>% filter(year>2000)) 

# table A3 ####
stargazer(lm.mod0,lm.mod,type="latex")



#### #simulation proper ####
#reading in functions
source("code/functions_modelling_plotting.R")
#simulation

#data starting 2013: practically 2014 because of a lag
# results022019_2=forecasts_cl(data=data_mig2,year.data=2019)
# results022018_2=forecasts_cl(data=data_mig2,year.data=2018)

#data starting 2012: added 0-obs for y 
results022019_21=forecasts_cl1(data=data_mig2,year.st=2011,year.data=2019)
results022018_21=forecasts_cl1(data=data_mig2,year.st=2011,year.data=2018)

#data starting 2013 but using differences
results022019_22=forecasts_cl1(data=data_mig2 %>% select(-value) %>% rename(value=GTI_diff),
                               year.st=2012,year.data=2019)
results022018_22=forecasts_cl1(data=data_mig2 %>% select(-value) %>% rename(value=GTI_diff),
                               year.st=2012,year.data=2018)



# making a plot of errors ####
## plot for 2019 errors ####
pl2018_2=plot_error(res = results022018_2,pl_fct = 35,clusters.max = 3,year.f = 2018, label = ", sample starting 2013")
pl2019_2=plot_error(res = results022019_2,pl_fct = 6,clusters.max = 3,year.f = 2019, label = ", sample starting 2013",y.lab=F)
pl2018_21=plot_error(res = results022018_21,pl_fct = 35,clusters.max = 3,year.f = 2018, label = ", sample with 2012 data")
pl2019_21=plot_error(res = results022019_21,pl_fct = 6,clusters.max = 3,year.f = 2019, label = ", sample with 2012 data",y.lab=F)
pl2018_22=plot_error(res = results022018_22,pl_fct = 35,clusters.max = 3,year.f = 2018, label = ", differences of the GTI used as predictor")
pl2019_22=plot_error(res = results022019_22,pl_fct = 6,clusters.max = 3,year.f = 2019, label = ", differences of the GTI used as predictor",y.lab=F)
pl2018_2
pl2019_2

g=ggarrange(plotlist=list(pl2018_2,pl2019_2), ncol=1,nrow=2, labels = NULL, legend ="right", common.legend = TRUE)
g=ggarrange(plotlist=list(pl2018_21,pl2019_21,pl2018_22,pl2019_22), ncol=2,nrow=2, labels = NULL, legend ="bottom", common.legend = TRUE, widths=c(1.05,1))
g1=ggarrange(plotlist=list(pl2018_2,pl2019_2,pl2018_21,pl2019_21,pl2018_22,pl2019_22), ncol=2,nrow=3, labels = NULL, legend ="bottom", common.legend = TRUE, widths=c(1.05,1))

ggexport(g,filename= paste0("Graphs/results_err_rob.pdf"),
         width=13,height = 13)
ggexport(g1,filename= paste0("Graphs/results_err_rob1.pdf"),
         width=13,height = 16)
ggexport(g,filename= paste0("Graphs/results_err.pdf"),width=9,height = 10)


#plotting the actual forecasts

pl.2018=plot_forecast(res = results022018_2,lags = c("GT[4]","GT[12]"), year.f=2018, clusters=c("currency~I","job/study"),v1="GTI_lag",v2="model",v3="cluster",sc=150)
pl.2019_21=plot_forecast(res = results022019_21,year.f=2019)

pl.2018
pl.2019
ggsave(filename = "Graphs/GTI_forcast2018.pdf",device = "pdf",plot = pl.2018,width = 6,height=7)
ggsave(filename = "Graphs/GTI_forcast2019.pdf",device = "pdf",plot = pl.2019,width = 12,height=7)



##########
## assesssing uncertainty ####
pl.unc.2018= plot_uncertainty(res = results022018_2,year.f = 2018, label = ", sample starting 2013")
pl.unc.2019= plot_uncertainty(res = results022019_2,year.f = 2019, label = ", sample starting 2013")
g=ggarrange(plotlist=list(pl.unc.2018,pl.unc.2019), ncol=1,nrow=2, labels = NULL, legend ="bottom", common.legend = TRUE)
ggexport(g,filename= paste0("Graphs/unc2013.pdf"),
         width=10,height = 11)
