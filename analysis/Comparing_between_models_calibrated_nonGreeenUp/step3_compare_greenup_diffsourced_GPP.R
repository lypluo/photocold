#---------------------------------
#Aim:compare the gpp data from different sources
#---------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
#------------
#(1)load the data
#-------------
load.path<-"D:/data/photocold_project/Merge_Data/Comparing_between_models_calibrated_nonGreeenUp/"
load(paste0(load.path,"GPP_from_diffSource_daily_conLUE.RDA"))

#----------------
#(2)compare the GPP_obs with conLUE modelled GPP(through different data sources)
#------------------
#I. first have a look at the performance for all the available sites 
#for each site or all sites
#1) for each site:
df_eachsite<-gpp_daily_conLUE_final %>%
  select(sitename,date,doy,sos,peak,gpp_obs,starts_with("conLUE_"))%>%
  group_by(sitename,doy)%>%
  summarise(obs = mean(gpp_obs, na.rm = TRUE),
            mod_conLUE_obs = mean(conLUE_gpp_obs,na.rm=T),
            mod_conLUE_pmodel = mean(conLUE_gpp_pmodel,na.rm=T),
            mod_conLUE_bess = mean(conLUE_gpp_bess,na.rm=T),
            mod_conLUE_rf = mean(conLUE_gpp_rf,na.rm=T),
            sos = mean(sos,na.rm=T),
            peak = mean(peak,na.rm=T)
  ) %>%
  pivot_longer(c(obs,mod_conLUE_obs,mod_conLUE_pmodel,mod_conLUE_bess,mod_conLUE_rf),
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
df_allsites<-gpp_daily_conLUE_final %>%
  select(sitename,date,doy,sos,peak,gpp_obs,starts_with("conLUE_"))%>%
  group_by(doy)%>%
  summarise(obs = mean(gpp_obs, na.rm = TRUE),
            mod_conLUE_obs = mean(conLUE_gpp_obs,na.rm=T),
            mod_conLUE_pmodel = mean(conLUE_gpp_pmodel,na.rm=T),
            mod_conLUE_bess = mean(conLUE_gpp_bess,na.rm=T),
            mod_conLUE_rf = mean(conLUE_gpp_rf,na.rm=T),
            sos = mean(sos,na.rm=T),
            peak = mean(peak,na.rm=T)
  ) %>%
  pivot_longer(c(obs,mod_conLUE_obs,mod_conLUE_pmodel,mod_conLUE_bess,mod_conLUE_rf),
               names_to = "Source", values_to = "gpp")
df_allsites %>%
  ggplot(aes(doy, gpp, color = Source))+
  geom_line()+
  # scale_color_manual(values = c("mod_conLUE" = "orange",
  #                               "mod_Pmodel"="steelblue2","obs" = "black"),
  #                    labels = c("constant LUE model","P model","Obs.")) +
  geom_vline(aes(xintercept=mean(sos,na.rm=T)),col="blue",lty=2,size=1.1)+
  # geom_vline(aes(xintercept=mean(peak)),col="blue",lty=2,size=1.1)+
  labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "Day of year")
#############
#II. have a look at the performance for all the sites identified as "overestimated sites"
#for each site or all sites
gpp_daily_conLUE_final_over<-gpp_daily_conLUE_final[gpp_daily_conLUE_final$Over_days_length>20,]
#
df_allsites<-gpp_daily_conLUE_final_over %>%
  select(sitename,date,doy,sos,peak,gpp_obs,starts_with("conLUE_"))%>%
  group_by(doy)%>%
  summarise(obs = mean(gpp_obs, na.rm = TRUE),
            mod_conLUE_obs = mean(conLUE_gpp_obs,na.rm=T),
            mod_conLUE_pmodel = mean(conLUE_gpp_pmodel,na.rm=T),
            mod_conLUE_bess = mean(conLUE_gpp_bess,na.rm=T),
            mod_conLUE_rf = mean(conLUE_gpp_rf,na.rm=T),
            sos = mean(sos,na.rm=T),
            peak = mean(peak,na.rm=T)
  ) %>%
  pivot_longer(c(obs,mod_conLUE_obs,mod_conLUE_pmodel,mod_conLUE_bess,mod_conLUE_rf),
               names_to = "Source", values_to = "gpp")
#
df_allsites %>%
  ggplot(aes(doy, gpp, color = Source))+
  geom_line(size=1.2)+
  # scale_color_manual(values = c("mod_conLUE" = "orange",
  #                               "mod_Pmodel"="steelblue2","obs" = "black"),
  #                    labels = c("constant LUE model","P model","Obs.")) +
  geom_vline(aes(xintercept=mean(sos,na.rm=T)),col="blue",lty=2,size=1.1)+
  annotate(geom = "text",x=90,y=10,label="sos",col="blue",size=6)+
  # geom_vline(aes(xintercept=mean(peak)),col="blue",lty=2,size=1.1)+
  labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "Day of year")
