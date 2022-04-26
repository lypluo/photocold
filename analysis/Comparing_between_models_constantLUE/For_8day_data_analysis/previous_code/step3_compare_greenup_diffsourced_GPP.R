#---------------------------------
#Aim:compare the gpp data from different sources
#---------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(phenopix)
library(zoo)
#------------
#(1)load the data
#-------------
load.path<-"D:/data/photocold_project/Merge_Data/Comparing_between_models_calibrated_nonGreeenUp/"
load(paste0(load.path,"GPP_from_diffSource_8day_conLUE.RDA"))

#----------------
#(2)compare the GPP_obs with conLUE modelled GPP(through different data sources)
#------------------
#I. first have a look at the performance for all the available sites 
#for each site or all sites
#1) for each site:
df_eachsite<-gpp_8day_conLUE_final %>%
  select(sitename,date,doy,sos,peak,gpp_obs_8day,starts_with("conLUE_"))%>%
  group_by(sitename,doy)%>%
  summarise(obs = mean(gpp_obs_8day, na.rm = TRUE),
            mod_conLUE_obs = mean(conLUE_gpp_obs_8day,na.rm=T),
            mod_conLUE_pmodel = mean(conLUE_gpp_pmodel_8day,na.rm=T),
            mod_conLUE_bess = mean(conLUE_gpp_bess_8day,na.rm=T),
            mod_conLUE_rf = mean(conLUE_gpp_rf_8day,na.rm=T),
            mod_conLUE_vpm = mean(conLUE_gpp_vpm_8day,na.rm=T),
            sos = mean(sos,na.rm=T),
            peak = mean(peak,na.rm=T)) %>%
  pivot_longer(c(obs,mod_conLUE_obs,mod_conLUE_pmodel,mod_conLUE_bess,mod_conLUE_rf,mod_conLUE_vpm),
               names_to = "Source", values_to = "gpp")
df_eachsite %>%
  ggplot(aes(doy, gpp, color = Source))+
  geom_line()+
  # scale_color_manual(values = c("mod_conLUE" = "orange",
  #                               "mod_Pmodel"="steelblue2","obs" = "black"),
  #                    labels = c("constant LUE model","P model","Obs.")) +
  geom_vline(aes(xintercept=mean(sos)),col="blue",lty=2,size=1.1)+
  # geom_vline(aes(xintercept=mean(peak)),col="blue",lty=2,size=1.1)+
  labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "Day of year")+
  facet_wrap(~sitename)
#2)for all the sites:
#only take out the 8-day mean and smooth the data
df_sel<-gpp_8day_conLUE_final[,c("sitename","date","doy","sos","peak","gpp_obs_8day","conLUE_gpp_obs_8day","conLUE_gpp_pmodel_8day",
                                "conLUE_gpp_bess_8day","conLUE_gpp_rf_8day","conLUE_gpp_vpm_8day")]
#fill the data and smooth
sites<-unique(df_sel$sitename)
df_sel.proc<-c()
for (i in 1:length(sites)) {
  df_sel_temp<-df_sel %>%
    filter(sitename==sites[i])
  df.pre<-df_sel_temp[,c(1:5)]
  df.post<-df_sel_temp%>%
    mutate(sos=NULL,peak=NULL)
  
  #fill the data 
  t_pre<-df.post[,-c(1:3)]
  t<-apply(df.post[,-c(1:3)],2,function(x){na.fill(x,"extend")})
  #and smooth
  smooth_fun<-function(x){
    if(length(x[!is.na(x)])>5){
      x_sm<-SplineFit(x)
      x_fit<-as.numeric(x_sm$fit$predicted)
      
    }
    if(length(x[!is.na(x)])<=5){
      x_fit<-rep(NA,length(x))
    }
    return(x_fit)
  }
  t_sm<-apply(t, 2, smooth_fun)
  t_sm<-as.data.frame(t_sm)
  names(t_sm)<-paste0(names(df.post[,-c(1:3)]),"_sm")
  df_sel.proc_temp<-cbind(df.pre,t_pre,t_sm)
  df_sel.proc<-rbind(df_sel.proc,df_sel.proc_temp)
}
#
df_allsites_1<-df_sel.proc %>%
  select(sitename,date,doy,sos,peak,gpp_obs_8day,starts_with("conLUE_"))%>%
  group_by(doy)%>%
  summarise(obs = mean(gpp_obs_8day, na.rm = TRUE),
            mod_conLUE_obs = mean(conLUE_gpp_obs_8day,na.rm=T),
            mod_conLUE_pmodel = mean(conLUE_gpp_pmodel_8day,na.rm=T),
            mod_conLUE_bess = mean(conLUE_gpp_bess_8day,na.rm=T),
            mod_conLUE_rf = mean(conLUE_gpp_rf_8day,na.rm=T),
            mod_conLUE_vpm = mean(conLUE_gpp_vpm_8day,na.rm=T),
            sos = mean(sos,na.rm=T),
            peak = mean(peak,na.rm=T)
  ) %>%
  pivot_longer(c(obs,mod_conLUE_obs,mod_conLUE_pmodel,mod_conLUE_bess,mod_conLUE_rf,mod_conLUE_vpm),
               names_to = "Source", values_to = "gpp")
df_allsites_2<-df_sel.proc %>%
  select(sitename,date,doy,ends_with("_sm"))%>%
  group_by(doy)%>%
  summarise(obs = mean(gpp_obs_8day_sm, na.rm = TRUE),
            mod_conLUE_obs = mean(conLUE_gpp_obs_8day_sm,na.rm=T),
            mod_conLUE_pmodel = mean(conLUE_gpp_pmodel_8day_sm,na.rm=T),
            mod_conLUE_bess = mean(conLUE_gpp_bess_8day_sm,na.rm=T),
            mod_conLUE_rf = mean(conLUE_gpp_rf_8day_sm,na.rm=T),
            mod_conLUE_vpm = mean(conLUE_gpp_vpm_8day_sm,na.rm=T),
  ) %>%
  pivot_longer(c(obs,mod_conLUE_obs,mod_conLUE_pmodel,mod_conLUE_bess,mod_conLUE_rf,mod_conLUE_vpm),
               names_to = "Source", values_to = "gpp_sm")
##
df_final<-left_join(df_allsites_1,df_allsites_2,by=c("doy","Source"))

##
df_final %>%
  ggplot()+
  geom_point(aes(doy, gpp, color = Source))+
  geom_line(aes(doy, gpp_sm, color = Source),size=1.2)+
  # scale_color_manual(values = c("mod_conLUE" = "orange",
  #                               "mod_Pmodel"="steelblue2","obs" = "black"),
  #                    labels = c("constant LUE model","P model","Obs.")) +
  geom_vline(aes(xintercept=mean(sos,na.rm=T)),col="blue",lty=2,size=1.1)+
  # geom_vline(aes(xintercept=mean(peak)),col="blue",lty=2,size=1.1)+
  labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "Day of year")


#############
#II. have a look at the performance for all the sites identified as "overestimated sites"
#for (each site) or all sites
gpp_8day_conLUE_final_over<-gpp_8day_conLUE_final[gpp_8day_conLUE_final$Over_days_length>20 &
                            !is.na(gpp_8day_conLUE_final$Over_days_length),]
#for all sites:
#only take out the 8-day mean and smooth the data
df_sel<-gpp_8day_conLUE_final_over[,c("sitename","date","doy","sos","peak","classid","gpp_obs_8day","conLUE_gpp_obs_8day","conLUE_gpp_pmodel_8day",
                                 "conLUE_gpp_bess_8day","conLUE_gpp_rf_8day","conLUE_gpp_vpm_8day")]
#fill the data and smooth
general_plots<-function(df_sel,PFT_name){
  # df_sel<-df_sel
  # PFT_name<-"ENF"
  
  if(PFT_name=="all"){
    df_t<-df_sel
  }
  if(PFT_name!="all"){
    df_t<-df_sel%>%
    filter(classid==PFT_name)
  }
  
  sites<-unique(df_t$sitename)
  df_sel.proc<-c()
  for (i in 1:length(sites)) {
    df_sel_temp<-df_t %>%
      filter(sitename==sites[i])
    df.pre<-df_sel_temp[,c(1:5)]
    df.post<-df_sel_temp%>%
      mutate(sos=NULL,peak=NULL)
    
    #fill the data 
    t_pre<-df.post[,-c(1:4)]
    t<-apply(df.post[,-c(1:4)],2,function(x){na.fill(x,"extend")})
    #and smooth
    smooth_fun<-function(x){
      if(length(x[!is.na(x)])>5){
        x_sm<-SplineFit(x)
        x_fit<-as.numeric(x_sm$fit$predicted)
        
      }
      if(length(x[!is.na(x)])<=5){
        x_fit<-rep(NA,length(x))
      }
      return(x_fit)
    }
    t_sm<-apply(t, 2, smooth_fun)
    t_sm<-as.data.frame(t_sm)
    names(t_sm)<-paste0(names(df.post[,-c(1:4)]),"_sm")
    df_sel.proc_temp<-cbind(df.pre,t_pre,t_sm)
    df_sel.proc<-rbind(df_sel.proc,df_sel.proc_temp)
  }
  
  df_allsites_1<-df_sel.proc %>%
    select(sitename,date,doy,sos,peak,gpp_obs_8day,starts_with("conLUE_"))%>%
    group_by(doy)%>%
    summarise(obs = mean(gpp_obs_8day, na.rm = TRUE),
              mod_conLUE_obs = mean(conLUE_gpp_obs_8day,na.rm=T),
              mod_conLUE_pmodel = mean(conLUE_gpp_pmodel_8day,na.rm=T),
              mod_conLUE_bess = mean(conLUE_gpp_bess_8day,na.rm=T),
              mod_conLUE_rf = mean(conLUE_gpp_rf_8day,na.rm=T),
              mod_conLUE_vpm = mean(conLUE_gpp_vpm_8day,na.rm=T),
              sos = mean(sos,na.rm=T),
              peak = mean(peak,na.rm=T)
    ) %>%
    pivot_longer(c(obs,mod_conLUE_obs,mod_conLUE_pmodel,mod_conLUE_bess,mod_conLUE_rf,mod_conLUE_vpm),
                 names_to = "Source", values_to = "gpp")
  df_allsites_2<-df_sel.proc %>%
    select(sitename,date,doy,ends_with("_sm"))%>%
    group_by(doy)%>%
    summarise(obs = mean(gpp_obs_8day_sm, na.rm = TRUE),
              mod_conLUE_obs = mean(conLUE_gpp_obs_8day_sm,na.rm=T),
              mod_conLUE_pmodel = mean(conLUE_gpp_pmodel_8day_sm,na.rm=T),
              mod_conLUE_bess = mean(conLUE_gpp_bess_8day_sm,na.rm=T),
              mod_conLUE_rf = mean(conLUE_gpp_rf_8day_sm,na.rm=T),
              mod_conLUE_vpm = mean(conLUE_gpp_vpm_8day_sm,na.rm=T),
    ) %>%
    pivot_longer(c(obs,mod_conLUE_obs,mod_conLUE_pmodel,mod_conLUE_bess,mod_conLUE_rf,mod_conLUE_vpm),
                 names_to = "Source", values_to = "gpp_sm")
  ##
  df_final<-left_join(df_allsites_1,df_allsites_2,by=c("doy","Source"))
  #
  p.plot<-df_final %>%
    ggplot()+
    geom_point(aes(doy, gpp, color = Source))+
    geom_line(aes(doy, gpp_sm, color = Source),size=1.2)+
    # scale_color_manual(values = c("mod_conLUE" = "orange",
    #                               "mod_Pmodel"="steelblue2","obs" = "black"),
    #                    labels = c("constant LUE model","P model","Obs.")) +
    geom_vline(aes(xintercept=mean(sos,na.rm=T)),col="blue",lty=2,size=1.1)+
    annotate(geom = "text",x=90,y=10,label="sos",size=6,col="blue")+
    annotate(geom = "text",x=180,y=1,label=PFT_name,size=6)+
    # geom_vline(aes(xintercept=mean(peak)),col="blue",lty=2,size=1.1)+
    labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
         x = "Day of year")
  return(p.plot)
} 
## for all the sites:
general_plots(df_sel,"all")


#############
#III. for different PFTs
#For DBF
general_plots(df_sel,"DBF")
general_plots(df_sel,"ENF")  
general_plots(df_sel,"MF")  
  

