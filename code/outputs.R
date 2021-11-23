############################################
## analysis of the GTI data and migration #
## analysis of forecasts
## plotting and tables with results
############################################
## created: 09/09/2021
## modified: 13/09/2021
############################################
## Arkadiusz Wisniowski
############################################


##### libraries ####
library(tidyverse)
library(stargazer)
#hue_pal()[n] useful for colours

# README ####
# the commands here reproduce all plots and tables in the manuscript. Before running commands in here, all data need to be read in (data_prep.R), analysis carried out (analysis.R) and some wrapper functions for plotting read in (functions_modelling_plotting.R). 
#This function loads the data
source("code/data_prep.R")
#If only the results need to be analysed, the results can be read in using the following:
# load("output/results_all.RData")
#If the simulation is to be run, then use 
# source("code/analysis.R")




# Figure 1: trends ####
pl.dat=  ggplot(pl.dati) + 
  geom_text(aes(x=2007,y=57,label="Romania joins the EU"), angle=90,nudge_x = -0.2, colour="grey25",size=4.5,family="Palatino") +
  geom_vline(xintercept = 2007,colour="darkgrey",linetype="dashed") +
  geom_text(aes(x=2012,y=50,label="Latest update to Google Trends"), angle=90,nudge_x = -0.25, colour="grey25",size=4.5,family="Palatino") +
  geom_vline(xintercept = 2011.98,colour="darkgrey",linetype="dashed") +
  
  geom_vline(xintercept = 2016.475,colour="darkgrey",linetype="dashed") +
  geom_line(data=. %>% filter(cluster!="all"),aes(x=year,y=value,group=name,colour=cluster,linetype=cluster),size=1,alpha=0.3) +
  scale_x_continuous(expand = c(0,0.2)) +
  scale_linetype_manual(values=c("solid","dashed", "twodash", "longdash", "1333", "6246" )) +
  geom_line(data=pl.datiav,aes(x=year,y=cluster_mean,colour=cluster,linetype=cluster), size=2) +
  geom_point(aes(x=year,y=exp(Raw_IPS), shape="immigration (IPS)"),colour="blue",size=4.5, alpha=0.9) +
  geom_text(aes(x=2016.475,y=15,label="Brexit referendum"), angle=90,nudge_x = -0.25, colour="grey25",size=4.5,family="Palatino") +
  theme_bw() +
  theme(axis.text.x = element_text(size=13),
        axis.title = element_text(size=12),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "right",
        legend.key.size = unit(c(1.1),units = "cm")
  ) +
  labs(y="Value (Google Trends Index | immigration in 1,000 persons)", x="Year") +
  scale_shape_discrete(name="") 
pl.dat
ggsave(filename = "graphs/GTI_Mig_ts.pdf",device = "pdf",plot = pl.dat,width = 10,height=5.5)

#Figure 2: GTI lags ####
pl.lag=data_mig2 %>% 
  separate(col = GTI,into=c("GTI","lag","cluster"),sep="[\\_.]") %>%
  mutate(lag=as_factor(lag), cluster=cluster %>% 
           as_factor() %>%
           fct_relevel("all","currency","education","employment","housing","control")) %>%
  filter(year>2012) %>%
  ggplot() +
  geom_line(aes(x = year, y = exp(value), colour=lag,group=(lag)), size = 1) +
  scale_color_viridis_d(option="inferno",end = 0.9) +
  geom_point(aes(x = year, y = exp(Raw_IPS), shape="immigration"), size = 4, color = "blue") + 
  scale_linetype_manual(values=c("solid","dashed", "twodash", "longdash","1333", "6246" )) +
  facet_grid(.~cluster) +
  theme_bw() +
  theme(axis.text.x = element_text(size=13),
        axis.title = element_text(size=12),
        legend.text = element_text(size=11),
        legend.title = element_text(size=14),
        legend.position = "right",
        strip.text = element_text(size=12)
        # legend.key.size = unit(c(.9),units = "cm")
  ) +
  labs(y="Value (Google Trends Index | immigration in 1,000 persons)", shape="")
pl.lag  
ggsave(filename = "graphs/GTI_lags.pdf",device = "pdf",plot = pl.lag,width = 12, height=5)





#PAPER table A1 with IPS####
data_an %>% mutate(year=lubridate::year(Date),
                   SE=IPSCI/qnorm(0.975),
                   `SE/IPS %`=SE/Raw_IPS*100) %>%
  select(year, Raw_IPS,IPSCI,SE,`SE/IPS %`) %>%
  rename(IPS=Raw_IPS,`95% CI`=IPSCI) %>%
  # relocate(keyword,before=1) %>%
  knitr::kable(format="latex", digits=c(0,1,1,1,1))


# correlation #####

#correlation matrix for IPS and keyword values
corr=GTIdetail %>%
  left_join(data_mig) %>%
  filter(year>2012) %>%
  mutate(IPS=exp(Raw_IPS)) %>%
  select(-year,-IPSSE,-IPSSEpc,-IPSCI,-Date) %>% 
  cor()
#to see results in e.g. Excel
# write.table(corr[,c(60,74)],file="clipboard")
# correlation with Raw IPS and logIPS
foo=corr[,c(60,74)]
#t-test for correlation at 0.05 level
# to see t-test in Excel
# write.table(abs(foo/sqrt(1-foo^2)*sqrt(5))>qt(0.95,df = 5),file="clipboard")

#logIPS as used in the models
corr1=as.data.frame(corr[,74]) %>%
  rename(rho=`corr[, 74]`) %>%
  rownames_to_column("keyword") %>%
  mutate(keyword=str_replace_all(keyword,"[.]"," "))

#correlation matrix between iPS and GTIs 
corr2=data_mig2 %>%
  select(-IPSSE, -IPSSEpc,-Date,-lagIPS,-GTI_diff) %>%
  mutate(IPS=exp(Raw_IPS)) %>%
  filter(year>2012) %>%
  pivot_wider(names_from = GTI, values_from=value) %>% 
  select(-year) %>%
  cor()
#copy to clipboard
# write.table(corr2[,c(1,2)],file="clipboard")

#value of rho above which it is significant at 0.05
qt(0.95,df = 5)/sqrt(5+qt(0.95,df = 5)^2)

#PAPER table A2 with key words####
#needs minor adjustments in LaTeX
GTIclust %>% 
  mutate(keyword=str_replace_all(name,"[.]"," ")) %>%
  select(-name) %>%
  left_join(corr1) %>%
  relocate(keyword,before=1) %>%
  knitr::kable(format="latex", digits=c(NA,1,1,1,1,1,1,1,1,NA,2))


#correlation for clusters and lags
corr3=as.data.frame(corr2[-c(1,2),1]) %>%
  rename(rho=`corr2[-c(1, 2), 1]`) %>%
  rownames_to_column("GTI") %>%
  separate(GTI,into = c("lag","cluster"), sep = "\\.") %>%
  mutate(lag=str_remove(lag,"GT_")) %>% 
  pivot_wider(names_from = cluster, values_from=rho) %>%
  relocate(lag,employment, education, currency, housing, control)

#Table A3
corr3 %>% knitr::kable(format="latex", digits=2)




#Figure A1: model for IPS SE vs observed IPS ####
# to create a prior for sigma of IPS data
pl.ipsse=ggplot(data=data_an) + 
  geom_point(aes(x=log(Raw_IPS),y=log(IPSSEpc/(1-IPSSEpc)))) +
  geom_smooth(aes(x=log(Raw_IPS),y=log(IPSSEpc/(1-IPSSEpc)),
                  colour="Model (2)",fill="Model (2)"),
              method="lm",formula=y~x-1,alpha=0.2) +
  geom_smooth(aes(x=log(Raw_IPS),y=log(IPSSEpc/(1-IPSSEpc)),colour="Model (1)",fill="Model (1)"),method="lm",formula=y~x,alpha=0.1) +
  geom_text(aes(x=log(Raw_IPS),y=log(IPSSEpc/(1-IPSSEpc)),label=lubridate::year(Date)),size=3,nudge_x = 0.1,nudge_y = .05) +
  theme_bw() +
  theme(axis.text.x = element_text(size=13),
        axis.title = element_text(size=12),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14)) +
  labs(x="IPS point estimate (natural logarithm)",
       y="logit of the IPS relative standard error", colour="Fit",fill="Fit")
pl.ipsse

ggsave(filename = "Graphs/IPS_rse.pdf",device = "pdf",plot = pl.ipsse,width = 8,height=5.5)


# table A4 ####
# requires running Models A and B in output.R
stargazer(lm.mod0,lm.mod,
          type="latex", 
          style = "default",
          report = "vcsp")



# making a plot of errors ####
## plot of errors Fig 3 and A2####
#this and further plots require sourcing functions_modelling_plotting.R
# source("code/functions_modelling_plotting.R")
pl2018_3=plot_error(res = results022018_30,pl_fct = 35,year.f = 2018, label = "; sample starting 2013")
pl2019_3=plot_error(res = results022019_30,pl_fct = 6,year.f = 2019, label = "; sample starting 2013",y.labL=F,y.labR = T)
pl2018_31=plot_error(res = results022018_31,pl_fct = 35,year.f = 2018, label = "; sample with 2012 data",y.labL=T,y.labR = F)
pl2019_31=plot_error(res = results022019_31,pl_fct = 6,year.f = 2019, label = "; sample with 2012 data",y.labL=F,y.labR = T)
pl2018_32=plot_error(res = results022018_32,pl_fct = 35,year.f = 2018, label = "; differences of the GTI used as predictor",y.labL=T,y.labR = F)
pl2019_32=plot_error(res = results022019_32,pl_fct = 6,year.f = 2019, label = "; differences of the GTI used as predictor",y.labL=F,y.labR = T)
pl2018_3
pl2019_3

g=ggarrange(plotlist=list(pl2018_3,pl2019_3), ncol=1,nrow=2, labels = NULL, legend ="right", common.legend = TRUE)
g1=ggarrange(plotlist=list(pl2018_31,pl2019_31,pl2018_32,pl2019_32), ncol=2,nrow=2, labels = NULL, legend ="bottom", common.legend = TRUE, widths=c(1.05,1))
g2=ggarrange(plotlist=list(pl2018_3,pl2019_3,pl2018_31,pl2019_31,pl2018_32,pl2019_32), ncol=2,nrow=3, labels = NULL, legend ="bottom", common.legend = TRUE, widths=c(1.05,1))

#for the presentation
# ggsave(filename = "Graphs/err_2019a.pdf",device = "pdf",plot = pl2019_3,width = 14,height=6.5)

#Fig A2 ####
ggexport(g1,filename= paste0("graphs/results_err_rob.pdf"),
         width=13,height = 13)
ggexport(g2,filename= paste0("graphs/results_err_rob1.pdf"),
         width=13,height = 16)
#Fig 3 ####
ggexport(g,filename= paste0("graphs/results_err.pdf"),width=10.5,height = 12.6)

# show MAPE
pl2018_3$data %>% filter(cluster=="employment",error=="MAPE", model%in%c("AR","ARX")) %>% distinct() %>% View
pl2018_3$data %>% filter(cluster=="employment",error=="MAPE", model%in%c("RW","RWX")) %>% distinct() %>% View




#Figs A3 and A4: plotting the forecasts ####
pl.2018=plot_forecast(res = results022018_30,lags = c("GT[4]","GT[12]"), year.f=2018, clusters=c("education","employment"),v1="GTI_lag",v2="model",v3="cluster",sc=150)
pl.2019=plot_forecast(res = results022019_30,year.f=2019)

#for the presentations
# pl.2018a=plot_forecast(res = results022018_30, lags = c("GT[6]"),year.f=2018)
# ggsave(filename = "Graphs/GTI_forcast2018a.pdf",device = "pdf",plot = pl.2018a,width = 14,height=6.5)
# ggsave(filename = "Graphs/GTI_forcast2019a.pdf",device = "pdf",plot = pl.2019,width = 14,height=6.5)

pl.2018
pl.2019

ggsave(filename = "Graphs/GTI_forcast2018.pdf",device = "pdf",plot = pl.2018,width = 6.5,height=7.5)
ggsave(filename = "Graphs/GTI_forcast2019.pdf",device = "pdf",plot = pl.2019,width = 12,height=7)



##########
## assesssing uncertainty ####
pl.unc.2018= plot_uncertainty(res = results022018_30,year.f = 2018, label = "; sample starting 2013")
pl.unc.2019= plot_uncertainty(res = results022019_30,year.f = 2019, label = "; sample starting 2013")
# Fig A5 ####
g3=ggarrange(plotlist=list(pl.unc.2018,pl.unc.2019), ncol=1,nrow=2, labels = NULL, legend ="bottom", common.legend = TRUE)
ggexport(g3,filename= paste0("graphs/unc2013.pdf"),
         width=10,height = 11)


# plotting for Twitter
#getting the errors
err_18=pl2018_3$data %>% filter(error=="MAPE") %>% distinct() %>% group_by(cluster,model) %>% summarise(MAPE=mean(value)) %>%
  mutate(year=2018)

err_19=pl2019_3$data %>% filter(error=="MAPE") %>% distinct() %>% group_by(cluster,model) %>% summarise(MAPE=mean(value)) %>%
  mutate(year=2019)

err_m=bind_rows(err_18,err_19) %>%
  mutate(model_type=case_when(
    model == "AR" ~ "benchmark",
    model == "ARX" ~ "GT predictor",
    model == "RW" ~ "benchmark",
    model == "RWX" ~ "GT predictor"
  ),
  model_specp=case_when(
    model == "AR" ~ "AR/ARX",
    model == "ARX" ~ "AR/ARX",
    model == "RW" ~ "RW/RWX",
    model == "RWX" ~ "RW/RWX"
  ),
  model_spec=case_when(
    model == "AR" ~ "AR",
    model == "ARX" ~ "AR",
    model == "RW" ~ "RW",
    model == "RWX" ~ "RW"
  ),
  cluster=as_factor(cluster)) 
  err_m = err_m %>%
    mutate(cluster=cluster %>% 
             fct_relevel("all", after=Inf) %>%
             fct_recode(`all keywords`="all")) 
#plots for paper 
#Fig 6: average MAPE 
  gparrw=ggplot(err_m ) +
    geom_point(aes(x=model_type,y=MAPE,group=factor(year),colour=factor(year)),size=2) +
    geom_path(aes(x=model_type,y=MAPE,group=factor(year),colour=factor(year)),arrow = arrow(),size=1.1) +
    facet_grid(model_specp~cluster,space = "free_x",scales = "free") +
    theme_bw() +
    theme(axis.text.x = element_text(size=13,angle = 45, hjust = 1),
          axis.title = element_text(size=12),
          legend.text = element_text(size=14),
          legend.title = element_text(size=14),
          strip.text = element_text(size=12),
          legend.position = "right",
          legend.key.size = unit(c(1.1),units = "cm")
    ) +
    labs(subtitle = "Sample starting 2013",
         y="MAPE (%)",
         x="Model",
         colour="Year")
  ggsave(filename = "graphs/err_average2013.pdf",device = "pdf",plot = gparrw,width = 9,height=7)  
  # ggsave(filename = "graphs/err_average2013s.pdf",device = "pdf",plot = gparrw,width = 13,height=6)
  
  
#plots for Twitter
gtar=ggplot(err_m %>% filter(model%in%c("AR","ARX"))) +
  geom_point(aes(x=model_type,y=MAPE,group=factor(year),colour=factor(year)),size=2) +
  geom_path(aes(x=model_type,y=MAPE,group=factor(year),colour=factor(year)),arrow = arrow(),size=1.1) +
  facet_grid(.~cluster,space = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(size=13,angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(size=12, hjust = 0.5),
        axis.title = element_text(size=12),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        strip.text = element_text(size=12),
        legend.position = "bottom",
        legend.key.size = unit(c(1.1),units = "cm")
  ) +
  labs(title="Average reductions in forecast errors (MAPE) when predicting migration\nfrom Romania to the UK for 2018 and 2019\n",
       subtitle = "Autoregressive models with predictors constructed by using Google Trends (GT) keywords:",
       y="Mean Absolute Percentage Error (in %)",
       x="Model",
       colour="Year") 

ggsave(filename = "graphs/Social_Media_5282.png",device = "png",plot = gtar,width = 940,height=788,units = "px",dpi=120)

gtrw=ggplot(err_m %>% filter(model%in%c("RW","RWX"))) +
  geom_path(aes(x=model_type,y=MAPE,group=factor(year),colour=factor(year)),arrow = arrow(),size=1.2) +
  facet_grid(.~cluster,space = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(size=13,angle = 45, hjust = 1),
        axis.title = element_text(size=12),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        strip.text = element_text(size=12),
        legend.position = "right",
        legend.key.size = unit(c(1.1),units = "cm")
  ) +
  labs(title="Average reductions in forecast errors (MAPE) when predicting migration\nfrom Romania to the UK for 2018 and 2019",
       subtitle = "Random walk models with Google Trends (GT) predictors",
       y="Mean Absolute Percentage Error (in %)",
       x="Model",
       colour="Year") 

ggsave(filename = "graphs/RandomWalk.png",device = "png",plot = gtrw,width = 940,height=788,units = "px",dpi=120)

#RW and AR together
gtarrw=ggplot(err_m) +
  geom_point(aes(x=model_type,y=MAPE,group=factor(year),colour=factor(year)),size=2) +
  geom_path(aes(x=model_type,y=MAPE,group=factor(year),colour=factor(year)),arrow = arrow(angle=25),size=1.1) +
  facet_grid(model_spec~cluster,
             space = "free_x",scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(size=13,angle = 45, hjust = 1),
        axis.title = element_text(size=12),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        strip.text = element_text(size=12),
        legend.position = "bottom",
        legend.key.size = unit(c(1.1),units = "cm")
  ) +
  labs(title="Average reductions in forecast errors (MAPE) when predicting migration\nfrom Romania to the UK for 2018 and 2019",
       subtitle = "Autoregressive (AR) and random walk (RW) models with predictors constructed by using Google Trends (GT) keywords:",
       y="Mean Absolute Percentage Error (in %)",
       x="Model",
       colour="Year") 

ggsave(filename = "graphs/Social_Media_5282_A.png",device = "png",plot = gtarrw,width = 940,height=788,units = "px",dpi=110)


# ### folders structure
# library(data.tree)
# path=list.files(recursive = TRUE, include.dirs = TRUE)
# path=str_c("GT-migr-forecasts/",path)
# mytree <- data.tree::as.Node(data.frame(pathString = path))
# mytree=print(mytree,limit = 1000)
# sink("folders.txt")
# print(mytree,row.names = F)
# sink()

