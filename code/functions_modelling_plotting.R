### IPS migration forecasting using GTI
### using 12 lagged versions of GTI
############################################
############################################
## created: 09/12/2020
## modified: 09/09/2021
############################################
## Arkadiusz Wisniowski
############################################

##### function for running a model ####
run.model.forecast <- function(model.file="Models/tsmodel02.stan", 
                               data.inp=data.inp_im00,
                               inits=inits02,
                               trees=10,
                               model.name,GT.nam,
                               year.st=2013){
  # require(tidyverse)
  # require(rstan)
  
#fitting
  start_time <- Sys.time()
  fit.m <- stan(file = paste(model.file), data = data.inp, iter = 1500, verbose = FALSE, chains=2,thin=1,warmup = 500,control = list(adapt_delta=0.95,max_treedepth=trees),init = list(inits,inits),cores = 2,seed=26)
  end_time <- Sys.time()
  end_time - start_time  
  
  dfying.f = function(x, nam, GTl) {
    x %>% 
      as.data.frame() %>%
      rownames_to_column(var="parameter") %>%
      mutate(model=model.name,GTI_lag=GT.nam)
  }

  #transformation  
  m.sum = fit.m %>% as.data.frame() %>% select(matches("yf")) %>% 
    gather(Parameter, Value) %>% group_by(Parameter) %>% 
    mutate(Iteration=row_number(Parameter)) %>% ungroup() %>% 
    separate(Parameter, c("series","year"), sep = "\\[") %>% 
    mutate(Value=exp(Value), year = year.st+1+as.numeric(str_match(year,"\\d+"))) %>% 
    group_by(year) %>%
    summarise(yhat=median(Value),yhat_lower=quantile(Value,0.025),yhat_upper=quantile(Value,0.975)) %>% ungroup() %>%
    mutate(model=model.name,GTI_lag=GT.nam)
  
  f.sum = summary(fit.m)$summary %>% dfying.f(nam=model.name,GTl=GT.nam) 
  
  return(list(forecasts=m.sum,fit=f.sum))
}


##### functions for creating forecasts ####



# GTI decomposed in three clusters ####
# DO NOT USE this function
# forecasts_cl <- function(data=data_mig2,
#                          year.st=2012,
#                          year.data=2019){
#   
#   inits02 <- list(phi0=0, phi1=0.1, phi2=0.5, sig_a=-0.5)
#   for.sum = list(NULL)
#   fit.sum = list(NULL)
#   ll=data$GTI %>% unique()
#   
#   data.inp_im<-list(y=data %>% filter(year>year.st & year<year.data,GTI==ll[1]) %>% pull(Raw_IPS), 
#                     x=data %>% filter(year>2012,GTI==ll[1]) %>% pull(value), 
#                     NG=7, NI=year.data-2013)
#   m01.sum = run.model.forecast(model.file="models/tsmodel01.stan", data.inp=data.inp_im, inits=inits02, trees=10, model.name="AR",GT.nam=NA) 
#   m02.sum = run.model.forecast(model.file="models/tsmodel01rw.stan", data.inp=data.inp_im, inits=inits02, trees=10, model.name="RW",GT.nam=NA)
#   for.sum[[1]] = m01.sum$forecasts
#   fit.sum[[1]] = m01.sum$fit
#   
#   for.sum[[2]] = m02.sum$forecasts
#   fit.sum[[2]] = m02.sum$fit
#   
#   for (i in 1:length(ll)){    
#     data.inp_im<-list(y=data %>% filter(year>2012 & year<year.data, GTI==ll[1]) %>% pull(Raw_IPS), 
#                       x=data %>% filter(year>2012, GTI==ll[i]) %>% pull(value), 
#                       NG=7, NI=year.data-2013)
# #ADL models with GTI    
#     temp1.sum = run.model.forecast(model.file="models/tsmodel02.stan", data.inp=data.inp_im, inits=inits02, trees=10, model.name="ADL",GT.nam=ll[i]) 
# #RWDL models with GTI
#     temp2.sum = run.model.forecast(model.file="models/tsmodel02rw.stan", data.inp=data.inp_im, inits=inits02, trees=10, model.name="RWDL",GT.nam=ll[i]) 
#     for.sum[[2+i]] = temp1.sum$forecasts
#     fit.sum[[2+i]] = temp1.sum$fit
#     for.sum[[2+length(ll)+i]] = temp2.sum$forecasts
#     fit.sum[[2+length(ll)+i]] = temp2.sum$fit
#   }
#  
#   forecasts.all=bind_rows(for.sum,.id=NULL)
#   fit.all=bind_rows(fit.sum,.id=NULL)
#   return(list(forecasts=forecasts.all,fit=fit.all))
# }

# simulation function corrected ####
forecasts_cl1 <- function(data=data_mig2,
                         year.st=2012,
                         year.data=2019){
  
  inits02 <- list(phi0=0, phi1=0.1, phi2=0.5, sig_a=-0.5)
  for.sum = list(NULL)
  fit.sum = list(NULL)
  ll=data$GTI %>% unique()
  
  y.dat=data %>% filter(year>year.st & year<year.data,GTI==ll[1]) %>% pull(Raw_IPS)
  x.dat=data %>% filter(year>year.st+1,GTI==ll[1]) %>% pull(value)
  
  
  data.inp_im<-list(y=y.dat, 
                    x=x.dat, 
                    NG=length(x.dat), 
                    NI=length(y.dat)-1)
  m01.sum = run.model.forecast(model.file="Models/tsmodel011.stan", data.inp=data.inp_im, inits=inits02, trees=10, model.name="AR",GT.nam=NA,year.st = year.st) 
  m02.sum = run.model.forecast(model.file="Models/tsmodel011rw.stan", data.inp=data.inp_im, inits=inits02, trees=10, model.name="RW",GT.nam=NA,year.st = year.st)
  for.sum[[1]] = m01.sum$forecasts
  fit.sum[[1]] = m01.sum$fit
  
  for.sum[[2]] = m02.sum$forecasts
  fit.sum[[2]] = m02.sum$fit
  
  for (i in 1:length(ll)){    
    y.dat=data %>% filter(year>year.st & year<year.data,GTI==ll[1]) %>% pull(Raw_IPS)
    x.dat=data %>% filter(year>year.st+1,GTI==ll[i]) %>% pull(value)
    
        data.inp_im<-list(y=y.dat, 
                      x=x.dat, 
                      NG=length(x.dat), 
                      NI=length(y.dat)-1)
    #ADL models with GTI    
    temp1.sum = run.model.forecast(model.file="Models/tsmodel021.stan", data.inp=data.inp_im, inits=inits02, trees=10, model.name="ADL",GT.nam=ll[i],year.st = year.st) 
    #RWDL models with GTI
    temp2.sum = run.model.forecast(model.file="Models/tsmodel021rw.stan", data.inp=data.inp_im, inits=inits02, trees=10, model.name="RWDL",GT.nam=ll[i],year.st = year.st) 
    for.sum[[2+i]] = temp1.sum$forecasts
    fit.sum[[2+i]] = temp1.sum$fit
    for.sum[[2+length(ll)+i]] = temp2.sum$forecasts
    fit.sum[[2+length(ll)+i]] = temp2.sum$fit
  }
  
  forecasts.all=bind_rows(for.sum,.id=NULL)
  fit.all=bind_rows(fit.sum,.id=NULL)
  return(list(forecasts=forecasts.all,fit=fit.all))
}

#ploting errors####
plot_error = function(res=results022019,
                      input=data_mig2,
                      pl_fct=7,
                      year.f=2019,
                      label="",
                      y.lab=T){
  clusters_name=res[[2]] %>% filter(parameter=="phi2") %>%
    separate(GTI_lag,into = c("GTI_lag","cluster"),sep = "\\.") %>% 
    select(cluster) %>% unique()
  temp=expand_grid(model=c("AR","RW"),clusters_name)
  
  
  res.sum = res[[1]] %>% 
    left_join(input %>% pivot_wider(names_from="GTI",values_from="value")) %>%
    mutate(ME=exp(Raw_IPS)-yhat,  MAPE=abs(exp(Raw_IPS)-yhat)/exp(Raw_IPS),
           MASE=abs(exp(Raw_IPS)-yhat)/abs(exp(Raw_IPS)-exp(lagIPS))) %>% #RMSE=sqrt((exp(Raw_IPS)-yhat)^2), = ME
    filter(year==year.f) %>%
    dplyr::select(Raw_IPS,year,model, GTI_lag, ME, MAPE) 
  
  res.phi = res[[2]] %>% filter(parameter=="phi2") %>%
    separate(GTI_lag,into = c("GTI_lag","cluster"),sep = "\\.") %>%
    left_join(temp, by="model") %>%
    unite(cluster,c("cluster.x","cluster.y"),sep="",na.rm=T) %>%
    mutate(model=case_when(model=="ADL" ~ "ARX", model=="RWDL"~"RWX",TRUE ~ model) %>% as_factor(), 
           GTI_lag=as_factor(GTI_lag))
  
  p= res.sum %>% 
    mutate(MAPE=MAPE*100) %>%
    pivot_longer(cols = ME:MAPE, names_to=c("error")) %>%
    separate(GTI_lag,into = c("GTI_lag","cluster"),sep = "\\.") %>%
    left_join(temp, by="model") %>%
    unite(cluster,c("cluster.x","cluster.y"),sep="",na.rm=T) %>% 
    # unite(model,c("model","GTI_lag"),sep=".",na.rm=T) %>%
    mutate(model=case_when(model=="ADL" ~ "ARX", model=="RWDL"~"RWX",TRUE ~ model) %>% as_factor(), 
           GTI_lag=replace_na(GTI_lag," "),
           GTI_lag=GTI_lag %>% str_replace("\\_","\\[") %>% str_c(ifelse(GTI_lag!=" ","]"," ")),
           GTI_lag=as_factor(GTI_lag)) %>%#%>% fct_relevel("RW",after = 40)
    ggplot() +
    geom_point(aes(x=GTI_lag,y=value, colour=error, shape=error),size=3) +
    geom_abline(intercept=0,slope=0,colour="darkgrey") +
    geom_errorbar(data=res.phi,mapping = aes(x=as.numeric(GTI_lag)+0.2,y=mean*pl_fct, ymin=`2.5%`*pl_fct,ymax=`97.5%`*pl_fct,linetype=parameter, group=parameter), width=.2,size=0.9, alpha=0.8) +
    geom_point(data=res.phi,mapping = aes(x=as.numeric(GTI_lag)+0.2,y=mean*pl_fct,fill=parameter, group=parameter), size=1, alpha=0.4) +
    scale_y_continuous(sec.axis = sec_axis(~./pl_fct, name=expression("value (parameter "~phi[2]~")"))) +
    scale_linetype_discrete(labels=list(bquote(phi[2]))) +
    scale_fill_discrete(labels=list(bquote(phi[2]))) +
    scale_x_discrete(labels=parse_format()) +
    facet_grid(cluster~model,scales = "free_x",space = "free_x") +
    theme_bw() +
    theme(axis.text.x = element_text(size=13, angle=90, vjust=0.5),
          axis.title = element_text(size=12),
          legend.text = element_text(size=14),
          legend.title = element_text(size=14),
          legend.position = "right",
          axis.title.y = if (y.lab==F) element_blank() else element_text(size=12)
    ) +
    labs(linetype="95% CI", fill="95% CI", x="GTI monthly lag",
         y="1,000s persons (ME) or % (MAPE)", #(MASE x 100)
         title = paste0("Forecast for ",year.f,label))
  return(p)
}

plot_forecast=function(res=results022019_2,
                       data=data_mig2,
                       year.f=2019,
                       lags="GT[12]",
                       clusters=c("employment","education","currency","housing","control","all"),
                       v1="GTI_lag",
                       v2="model",
                       v3="cluster",
                       sc=NULL){
  # this code is a bit clumsy in joining data with results 
  
  #auxiliary data frames for data that don't have models with GTI  
  clusters_name=res[[2]] %>% filter(parameter=="phi2") %>%
    separate(GTI_lag,into = c("GTI_lag","cluster"),sep = "\\.") %>% 
    select(cluster) %>% unique()
  temp=expand_grid(model=c("AR","RW"),clusters_name)
  temp_d=expand_grid(model=c("AR","RW","ADL","RWDL"), GTI_lag=data %>% select(GTI) %>% distinct() %>% pull(GTI))
    
  data_aux= data %>% 
    select(year,GTI,value) %>%
    rename(GTI_lag=GTI,GTI=value)  
  data_ips= data %>% 
    select(year,Raw_IPS)
  
#joining data and results  
  dat=res[[1]] %>% left_join(data_aux) %>%
    left_join(data_ips) %>%
    separate(GTI_lag,into = c("GTI_lag","cluster"),sep = "\\.") %>%
    left_join(temp, by="model") %>%
    unite(cluster,c("cluster.x","cluster.y"),sep="",na.rm=T) %>% 
    mutate(model=case_when(model=="ADL" ~ "ARX", 
                           model=="RWDL"~"RWX",TRUE ~ model) %>% as_factor(), 
           GTI_lag=replace_na(GTI_lag," "),
           GTI_lag=GTI_lag %>% str_replace("\\_","\\[") %>% str_c(ifelse(GTI_lag!=" ","]"," ")),
           GTI_lag=as_factor(GTI_lag)) %>%
    filter(GTI_lag%in%lags|model%in%c("AR","RW")) %>%
    filter(cluster%in%clusters,year<=year.f) 
  
  p=ggplot(data=dat) +
    geom_line(aes(x = year, y = exp(GTI)), size = 1, color = "red",linetype="dashed") +
    geom_line(aes(x = year, y = yhat),color = "#670878") + 
    geom_ribbon(aes(x = year, ymin = yhat_lower, ymax = yhat_upper), fill = "#855d8c", alpha = 0.3) + 
    geom_point(aes(x = year, y = ifelse(year==year.f,NA,exp(Raw_IPS))), size = 3, color = "#0062ff") +
    geom_point(aes(x = year, y = ifelse(year==year.f,exp(Raw_IPS),NA)), size = 3, color = "#0062ff", shape=17) +
    # geom_vline(aes(xintercept = year.f-.5), linetype="dashed",colour="grey") +
    facet_grid(dat[[v1]]+dat[[v2]]~dat[[v3]],labeller = label_parsed) +
    scale_y_continuous(limits = if (is.null(sc)) NULL else c(NA,sc), oob=scales::squish) +
    theme_bw() + #GTI_lag+model~cluster
    theme(axis.text.x = element_text(angle=90,vjust = 0.5),
          strip.text = element_text(size=13)) +
    labs(title=paste("Forecasts for ",year.f),y="1,000 persons")
  
  return(p)
}


## plot uncertainty ####
plot_uncertainty= function(res=results022019_2,
                           data=data_mig,
                           year.f=2019,
                           label=""){
  clusters_name=res[[2]] %>% filter(parameter=="phi2") %>%
    separate(GTI_lag,into = c("GTI_lag","cluster"),sep = "\\.") %>% 
    select(cluster) %>% unique()
  temp=expand_grid(model=c("AR","RW"),clusters_name)
  
  dat=res[[1]] %>% left_join(data) %>%
  separate(GTI_lag,into = c("GTI_lag","cluster"),sep = "\\.") %>%
  left_join(temp, by="model") %>%
  unite(cluster,c("cluster.x","cluster.y"),sep="",na.rm=T) %>%   mutate(model=case_when(model=="ADL" ~ "ARX", model=="RWDL"~"RWX",TRUE ~ model) %>% as_factor(), 
         GTI_lag=replace_na(GTI_lag," "),
         GTI_lag=GTI_lag %>% str_replace("\\_","\\[") %>% str_c(ifelse(GTI_lag!=" ","]"," ")),
         GTI_lag=as_factor(GTI_lag)) %>%
  mutate(IPS_lower=exp(Raw_IPS)-IPSCI,
         IPS_upper=exp(Raw_IPS)+IPSCI,
         yhat_point=yhat,
         IPS_point=exp(Raw_IPS),
         yhatCIu=yhat_upper-yhat,yhatCIl=-yhat_lower+yhat,
         yhatCI=.5*yhatCIu+.5*yhatCIl) %>%
  filter(year==year.f) %>%
  pivot_longer(cols=c(yhat_lower,IPS_lower,yhat_upper,IPS_upper,IPS_point,yhat_point), names_to=c("data","type"),names_sep="\\_") %>%
    mutate(data=data %>% as_factor %>% recode("yhat"="forecast")) %>%
  pivot_wider(names_from=type) 
  

p=ggplot(data=dat) +
  geom_point(aes(x=GTI_lag,y=point,colour=data),size=1.2,position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(x=GTI_lag,ymin=lower,ymax=upper,colour=data),width=0.3,size=1.0,position = position_dodge(width = 0.5)) +
  scale_x_discrete(labels=parse_format()) +
  facet_grid(cluster~model,scales = "free_x",space = "free_x", labeller = label_parsed) +
  theme_bw() +
  theme(axis.text.x = element_text(size=13, angle=90, vjust=0.5),
        axis.title = element_text(size=12),
        legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        legend.position = "right",
        axis.title.y = element_text(size=12) #if (y.lab==F) element_blank() else
  ) +
  labs(x="GTI monthly lag",
       y="1,000s persons",
       title = paste0("Forecast for ",year.f,label))

return(p)
}

#old code for comparison only
# GTI lagged over monthly data ####
# forecasts_2019 <- function(data=data_mig){
# 
#   inits02 <- list(phi0=0, phi1=0.1, phi2=0.5, sig_a=-0.5)
#   m02.sum = list(NULL)
#   m02rw.sum = list(NULL)
#   
#   for (i in 1:13){    
#     data.inp_im<-list(y=data %>% filter(year>2012 & year<2019) %>% pull(Raw_IPS), 
#                            x=data %>% filter(year>2012) %>% pull(5+i), 
#                            NG=7, NI=6)
# 
#     m02.sum[[i]] = run.model.forecast(model.name="Models/tsmodel02.stan", data.inp=data.inp_im, inits=inits02, trees=10) %>%
#       mutate(GTI_lag=i-1, model = "ADL")
#     m02rw.sum[[i]] = run.model.forecast(model.name="Models/tsmodel02rw.stan", data.inp=data.inp_im, inits=inits02, trees=10) %>%
#       mutate(GTI_lag=i-1, model = "RWDL")
#   }
#   
#   data.inp_im<-list(y=data %>% filter(year>2012 & year<2019) %>% pull(Raw_IPS), 
#                     x=data %>% filter(year>2012) %>% pull(6), 
#                     NG=7, NI=6)
#   m01.sum=list(NULL)
#   m01.sum[[1]] = run.model.forecast(model.name="Models/tsmodel01.stan", data.inp=data.inp_im, inits=inits02, trees=10) %>%
#     mutate(GTI_lag=NA, model = "AR")
#   m01.sum[[2]] = run.model.forecast(model.name="Models/tsmodel01rw.stan", data.inp=data.inp_im, inits=inits02, trees=10) %>%
#     mutate(GTI_lag=NA, model = "RW")
#   
#   m.sum=bind_rows(m01.sum,m02.sum,m02rw.sum,.id=NULL)
#   return(m.sum)
# }