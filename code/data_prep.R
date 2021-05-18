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
library(ggthemes)
require(ggpubr)
library(scales) # useful parse_format() function
#hue_pal()[n] useful for colours

# Import data on GTI ####
data <- read.xlsx("data/GT.xlsx", sheetName = "Sheet1")
#Separate the Month column into year and month
data$year <- format(data$Date, format = "%Y")
data$month <- format(data$Date, format = "%m")
#Format the data as numeric
data$year <- as.numeric(data$year)
data$month <- as.numeric(data$month)
#Outliers 
#Test if data are not stationary
adf.test(data$AVERAGE)

#Remove outliers in the AVERAGE column
tsoutliers(data$AVERAGE, iterate = 5, lambda = NULL)
data$AVERAGE[54] <- 45.92105

#all GT variables in detail ####
GTIdetail=data %>% 
  rename(GTI=AVERAGE) %>%
  group_by(year) %>%
  summarise_at(vars(GTI:leu.lira), mean) %>%
  ungroup() %>%
  select(-ambasada.angliei,-British.embassy,-transport.anglia,-visit.uk)

#clustering GTI data annually averaged ####
#clustering using raw GTI data
x1=GTIdetail %>% 
  select(-GTI) %>% 
  pivot_longer(cols = 2:35) %>% 
  pivot_wider(names_from = year, values_from=value) %>% 
  column_to_rownames(var = "name")  %>% 
  as.matrix()
#testing scree and silhouette for optimum number of clusters
require(factoextra)
fviz_nbclust(x1, kmeans, method='silhouette',iter.max=25,nstart=100)

GTIkm=kmeans(x1,centers = 3,iter.max=25,nstart=100)
# summary(GTIkm)
GTIkm=as.data.frame(GTIkm$cluster) %>% 
  rownames_to_column(var = "name") %>% 
  rename(cluster=`GTIkm$cluster`)
#GTIclust is a key between cluster.id and cluster (name)
GTIclust = GTIdetail %>% 
  # select(-GTI) %>% 
  pivot_longer(cols = 2:36) %>% 
  pivot_wider(names_from = year, values_from=value) %>% 
  left_join(GTIkm) %>%
  rename(cluster.id=cluster) %>%
  mutate(cluster=case_when(cluster.id==GTIclust %>% filter(name=="X1.lira") %>% pull(cluster.id) ~ "currency I",
                           cluster.id==GTIclust %>% filter(name=="ron.to.pound") %>% pull(cluster.id) ~ "currency II",
                           cluster.id==GTIclust %>% filter(name=="jobs.uk") %>% pull(cluster.id) ~ "job|study",
                           is.na(cluster.id) ~ "all")) 
# the above mutate() is the "original" coding (_2), in the new runs (_21, _22) clusters change, old 1 = new 3, and old 3 = new 1.


#plotting clustered GTI values
GTIclust %>% pivot_longer(cols = `2012`:`2019`,names_to="year") %>% 
  ggplot() + 
  geom_line(aes(x=year,y=value,group=name,colour=cluster),size=1,alpha=0.5) +
  # facet_wrap(year~.,nrow = 4) +
  geom_smooth(aes(x=year,y=value,group=cluster,colour=cluster, fill=cluster),method="loess",size=1.5) +
  theme_bw()

# GTI averages with monthly windows
GTIm_av=data %>% 
  select(Date, AVERAGE,year, month) %>%
  rename(GTI=AVERAGE) %>%
  mutate(GTI_0=(GTI), 
         GTI_1=lag(GTI), 
         GTI_2=lag(GTI,n = 2), 
         GTI_3=lag(GTI,n = 3),
         GTI_4=lag(GTI,n = 4),
         GTI_5=lag(GTI,n = 5),
         GTI_6=lag(GTI,n = 6),
         GTI_7=lag(GTI,n = 7),
         GTI_8=lag(GTI,n = 8),
         GTI_9=lag(GTI,n = 9),
         GTI_10=lag(GTI,n = 10),
         GTI_11=lag(GTI,n = 11),
         GTI_12=lag(GTI,n = 12)
  ) %>%
  group_by(year) %>%
  summarise_at(vars(GTI_0:GTI_12), mean) %>%
  ungroup()

GTIm=data %>% 
  rename(GTI=AVERAGE) %>%
  pivot_longer(cols = GTI:leu.lira,names_to = "GTname") %>%
  group_by(GTname) %>%
  mutate(GT_0=(value), 
         GT_1=lag(value), 
         GT_2=lag(value,n = 2), 
         GT_3=lag(value,n = 3),
         GT_4=lag(value,n = 4),
         GT_5=lag(value,n = 5),
         GT_6=lag(value,n = 6),
         GT_7=lag(value,n = 7),
         GT_8=lag(value,n = 8),
         GT_9=lag(value,n = 9),
         GT_10=lag(value,n = 10),
         GT_11=lag(value,n = 11),
         GT_12=lag(value,n = 12)
  ) %>%
  ungroup() %>%
  # filter(GTname!="GTI") %>%
  select(-value) %>%
  group_by(Date, year, GTname) %>%
  summarise_at(vars(GT_0:GT_12), mean) %>%
  ungroup() %>%
  left_join(GTIkm,by = c("GTname"="name")) %>%
  mutate(cluster = cluster %>% replace_na("all") %>%
           as_factor() %>% fct_relevel("1","2","3","all")) %>%
  select(-GTname) %>%
  group_by(cluster,year, Date) %>%
  summarise_at(vars(GT_0:GT_12), mean) %>%
  ungroup() %>%  
  pivot_wider(names_from = "cluster", 
              values_from = GT_0:GT_12, 
              names_sep=".") 


##### reading in data on migration
data_an <- read.xlsx('data/IPS_data.xlsx', sheetIndex = 1)
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")

# data for migr and GTI joined together
data_mig = data_an %>%
  mutate(year=lubridate::year(Date),
         IPSSE=IPSCI/qnorm(0.975),
         IPSSEpc=IPSSE/Raw_IPS) %>%
  select(Date,year, Raw_IPS,IPSSE,IPSSEpc, IPSCI) %>%
  left_join(GTIm_av) %>%
  mutate_at(vars(Raw_IPS,GTI_0:GTI_12),log)

# data for migr and GTI joined together ####
data_mig2 = data_an %>%
  mutate(year=lubridate::year(Date),
         IPSSE=IPSCI/qnorm(0.975),
         IPSSEpc=IPSSE/Raw_IPS) %>%
  select(Date,year, Raw_IPS,IPSSE,IPSSEpc) %>%
  left_join(GTIm) %>%
  mutate_at(vars(Raw_IPS,GT_0.1:GT_12.all),log) %>%
  pivot_longer(cols = GT_0.1:GT_12.all,names_to="GTI") %>%
  group_by(GTI) %>% 
  mutate(GTI_diff=value-lag(value)) %>% 
  ungroup()


#ex_rate=data.frame(year=2012:2019, EX=c(0.1821, 0.1922, 0.1814, 0.1634,  0.1825, 0.1918, 0.1901, 0.1848 ))


#Figure 2: GTI lags ####
pl.lag=data_mig2 %>% 
  separate(col = GTI,into=c("GTI","lag","cluster"),sep="[\\_.]") %>%
  mutate(lag=as_factor(lag), cluster=as_factor(cluster)) %>%
  rename(cluster.id=cluster) %>%
  mutate(cluster=case_when(cluster.id==GTIclust %>% filter(name=="X1.lira") %>% pull(cluster.id) ~ "currency I",
                           cluster.id==GTIclust %>% filter(name=="ron.to.pound") %>% pull(cluster.id) ~ "currency II",
                           cluster.id==GTIclust %>% filter(name=="jobs.uk") %>% pull(cluster.id) ~ "job|study",
                           cluster.id=="all" ~ "all")) %>%
  filter(year>2012) %>%
  ggplot() +
  geom_line(aes(x = year, y = exp(value), colour=lag,group=(lag)), size = 1) +
  scale_color_viridis_d(option="inferno",end = 0.9) +
  geom_point(aes(x = year, y = exp(Raw_IPS)), size = 4, color = "blue") + 
  scale_linetype_manual(values=c("solid","dashed", "twodash", "longdash" )) +
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
  labs(y="value (Google Trend Index | immigration in 1,000 persons)")
pl.lag  
ggsave(filename = "Graphs/GTI_lags.pdf",device = "pdf",plot = pl.lag,width = 12,height=5)



#PAPER table with key words####
GTIclust %>% 
  mutate(keyword=str_replace_all(name,"[.]"," ")) %>%
  select(-cluster.id,-name) %>%
  relocate(keyword,before=1) %>%
  knitr::kable(format="latex", digits=1)

#PAPER table with IPS####
data_an %>% mutate(year=lubridate::year(Date),
                   SE=IPSCI/qnorm(0.975),
                   `SE/IPS %`=SE/Raw_IPS*100) %>%
  select(year, Raw_IPS,IPSCI,SE,`SE/IPS %`) %>%
  rename(IPS=Raw_IPS,`95% CI`=IPSCI) %>%
  # relocate(keyword,before=1) %>%
  knitr::kable(format="latex", digits=c(0,1,1,1,1))
