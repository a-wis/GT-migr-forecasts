### Data reading and preps for analysis  ###
### plots and tables
############################################
### created: 10/12/2020
### modified: 09/09/2021
############################################
### Arkadiusz Wisniowski
############################################
rm(list=ls())

##### libraries
library(tidyverse)
library(xlsx)
library(readr)
library(zoo)
library(ggthemes)
require(ggpubr)
library(scales) # useful parse_format() function
#hue_pal()[n] useful for colours

#set working directory ####
#setwd("~/GT-migr-forecasts/")

# Import data on GTI ####
data <- read_csv("data/GT_data.csv")
data = data %>%
  mutate(Date=as.yearmon(Date),
         year=format(Date, format = "%Y") %>% as.numeric(), 
         month=format(Date, format="%m") %>% as.numeric()) %>%
  rename_all(list(~
    str_replace_all(.,
                    pattern = " ",
                    replacement = "."))) %>%
  mutate(GTI=rowMeans(select(.,evolutie.curs.lira.sterlina:education.uk))) %>%
  filter(year<2020) %>%
  select(Date,year, month, everything(.))
  
cluster_id = read_csv("data/GT_cluster_dictionary.csv") %>%
  mutate(keyword=str_replace_all(keyword,
                                 pattern = " ",
                                 replacement = ".")) %>%
  rename(name=keyword)

#all GT variables in detail ####
GTIdetail=data %>% 
  group_by(year) %>%
  summarise_at(vars(evolutie.curs.lira.sterlina:GTI), mean) %>%
  ungroup() 

#clustering GTI data annually averaged ####
#clustering using raw GTI data
x1=GTIdetail %>% 
  select(-GTI,-c(50:60)) %>% 
  pivot_longer(cols = 2:49) %>% 
  pivot_wider(names_from = year, values_from=value) %>% 
  column_to_rownames(var = "name")  %>% 
  as.matrix()

# clustering using k-means - from earlier version - not published
#testing scree and silhouette for optimum number of clusters
# require(factoextra)
# fviz_nbclust(x1, kmeans, method='silhouette',iter.max=50,nstart=100)

# GTIkm=kmeans(x1,centers = 4,iter.max=50,nstart=100)
# summary(GTIkm)
# GTIkm=as.data.frame(GTIkm$cluster) %>% 
  # rownames_to_column(var = "name") %>% 
  # rename(cluster=`GTIkm$cluster`)


#GTIclust is a key between cluster.id and cluster (name)
GTIclust = GTIdetail %>% 
  # select(-GTI) %>% 
  pivot_longer(cols = 2:60) %>% 
  pivot_wider(names_from = year, values_from=value) %>% 
  left_join(cluster_id) %>%
  mutate(cluster=case_when(
    is.na(cluster) ~ "all",
    TRUE ~ cluster)
    ) #cluster=as.factor(cluster)


#plotting clustered GTI values
#for an example cluster
# GTIclust %>% 
#   pivot_longer(cols = `2012`:`2019`,names_to="year") %>% 
#   filter(cluster=="employment") %>%
#   ggplot() + 
#   geom_line(aes(x=year,y=value,group=name,colour=cluster),size=1,alpha=0.5) +
#   # facet_wrap(year~.,nrow = 4) +
#   geom_smooth(aes(x=year,y=value,group=cluster,colour=cluster, fill=cluster),method="loess",size=1.5) +
#   theme_bw()

# GTIclust %>% pivot_longer(cols = `2012`:`2019`,names_to="year") %>% mutate(year=as.numeric(year)) %>%
#   select(-cluster) %>%
#   group_by(cluster_s,year) %>%
#   summarise(cluster_mean=mean(value)) %>% ungroup() %>%
#   ggplot() + 
#   geom_line(aes(x=year,y=cluster_mean,colour=cluster_s),size=1,alpha=0.5) + 
#   theme_bw()



# GTI averages with monthly windows ####
GTIm_av=data %>% 
  select(Date, GTI,year, month) %>%
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
  pivot_longer(cols = evolutie.curs.lira.sterlina:GTI,names_to = "GTname") %>%
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
  group_by(year, GTname) %>%
  summarise_at(vars(GT_0:GT_12), mean) %>%
  ungroup() %>%
  left_join(cluster_id,by = c("GTname"="name")) %>%
  mutate(cluster = cluster %>% replace_na("all") %>%
           as_factor() ) %>%
  select(-GTname) %>%
  group_by(cluster,year) %>%
  summarise_at(vars(GT_0:GT_12), mean) %>%
  ungroup() %>%  
  pivot_wider(names_from = "cluster", 
              values_from = GT_0:GT_12, 
              names_sep=".") 


##### reading in data on migration ####
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
         IPSSEpc=IPSSE/Raw_IPS,
         lagIPS=dplyr::lag(log(Raw_IPS))) %>%
  select(Date,year, Raw_IPS,lagIPS,IPSSE,IPSSEpc) %>%
  left_join(GTIm) %>%
  mutate_at(vars(Raw_IPS,GT_0.currency:GT_12.employment),log) %>%
  pivot_longer(cols = GT_0.currency:GT_12.employment,names_to="GTI") %>%
  group_by(GTI) %>% 
  mutate(GTI_diff=value-lag(value)) %>% 
  ungroup()





  
  