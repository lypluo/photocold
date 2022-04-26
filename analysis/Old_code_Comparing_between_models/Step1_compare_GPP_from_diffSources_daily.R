##############################################################
#Aim: compare the GPP from different sources(from P-model,BESS,VPM,MTE,consLUE...)
############################################################
library(tidyr)
library(dplyr)
library(ggsci)  #for plot colors
#-----------------
#(1)load the data
#----------------
load.path<-"D:/data/photocold_project/GPP_from_diffSources/site_scale/"
#BESS
load(paste0(load.path,"GPP_BESS_FLUX2015_daily.RDA"))
# #VPM
# load(paste0(load.path,"GPP_VPM_FLUX2015.RDA")) 
#MTE
load(paste0(load.path,"GPP_MTE_RF_FLUX2015_daily.RDA"))
#conLUE
load(paste0(load.path,"GPP_conLUE_FLUX2015_daily.RDA"))

#-----------------
#(2)Merge different data sources
#----------------
df_merge<-left_join(GPP_conLUE,GPP_BESS_new,by=c("sitename","date","doy"))
#
#do not use VPM temporaily since it has 8-day avearge
# df_merge<-left_join(df_merge,GPP_VPM_new,by=c("sitename","date","doy"))
#
df_merge<-left_join(df_merge,GPP_MTE_new,by=c("sitename","date","doy"))
#
#-------------------------------------------------
#add additional code for PFTs: add PFTs information for each sites-->2021-12-13
#------------------------------------------------
#load the modis data for classifying the vegetation types:
library(tidyverse) #-->function read_rds
PFT.path<-"D:/Github/photocold/data/"
df_sites_modis_era <- read_rds(paste0(PFT.path,"df_sites_modis_era.csv"))
#
df_merge<-df_merge%>%
  left_join(
    df_sites_modis_era,
    by = "sitename"
  )

#-------------------------------------------
#(3)compare GPP between different modelled GPP
#-------------------------------------------
#-------------
#a.1st part:for all the available sites
#-------------
#a1. for each site:
df_merge %>%
  group_by(sitename,doy)%>%
  summarise(obs = mean(gpp_obs, na.rm = TRUE),
            obs_sd = sd(gpp_obs,na.rm = T),
            mod_conLUE = mean(gpp_conLUE,na.rm=T),
            mod_conLUE_sd = sd(gpp_conLUE,na.rm = T),
            mod_Pmodel = mean(gpp_mod_FULL,na.rm=T),
            mod_Pmodel_sd = sd(gpp_mod_FULL,na.rm = T),
            mod_BESS = mean(gpp_bess,na.rm=T),
            mod_BESS_sd = sd(gpp_bess,na.rm = T),
            # mod_VPM = mean(gpp_vpm,na.rm=T),
            # mod_VPM_sd = sd(gpp_vpm,na.rm = T),
            mod_RF = mean(gpp_rf,na.rm=T),  ##using RF as the data are daily
            mod_RF_sd = sd(gpp_rf,na.rm = T)
  ) %>%
  pivot_longer(c(obs,mod_conLUE,mod_Pmodel,mod_BESS,mod_RF),
               names_to = "Source", values_to = "gpp") %>%
  ggplot(aes(doy, gpp, color = Source))+
  geom_line()+
  # scale_color_manual(values = c("mod_conLUE" = "orange",
  #                               "mod_Pmodel"="steelblue2","obs" = "black"),
  #                    labels = c("constant LUE model","P model","Obs.")) +
  labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "Day of year")+
  facet_wrap(~sitename)+
  scale_color_npg()

#a2. for all the sites:
df_gpp_all<-df_merge %>%
  group_by(doy)%>%
  summarise(obs = mean(gpp_obs, na.rm = TRUE),
            obs_sd = sd(gpp_obs,na.rm = T),
            mod_conLUE = mean(gpp_conLUE,na.rm=T),
            mod_conLUE_sd = sd(gpp_conLUE,na.rm = T),
            mod_Pmodel = mean(gpp_mod_FULL,na.rm=T),
            mod_Pmodel_sd = sd(gpp_mod_FULL,na.rm = T),
            mod_BESS = mean(gpp_bess,na.rm=T),
            mod_BESS_sd = sd(gpp_bess,na.rm = T),
            # mod_VPM = mean(gpp_vpm,na.rm=T),
            # mod_VPM_sd = sd(gpp_vpm,na.rm = T),
            mod_RF = mean(gpp_rf,na.rm=T),  ##using RF as the data are daily
            mod_RF_sd = sd(gpp_rf,na.rm = T))%>%
  mutate(obs_mean=obs,obs_m_sd=obs - obs_sd,obs_a_sd=obs + obs_sd,
         mod_conLUE_mean=mod_conLUE,
         mod_conLUE_m_sd=mod_conLUE - mod_conLUE_sd,
         mod_conLUE_a_sd=mod_conLUE + mod_conLUE_sd,
         mod_Pmodel_mean=mod_Pmodel,
         mod_Pmodel_m_sd=mod_Pmodel - mod_Pmodel_sd,
         mod_Pmodel_a_sd=mod_Pmodel + mod_Pmodel_sd,
         mod_BESS_mean=mod_BESS,
         mod_BESS_m_sd=mod_BESS - mod_BESS_sd,
         mod_BESS_a_sd=mod_BESS + mod_BESS_sd,
         # mod_VPM_mean=mod_VPM,
         # mod_VPM_m_sd=mod_VPM - mod_VPM_sd,
         # mod_VPM_a_sd=mod_VPM + mod_VPM_sd,
         mod_RF_mean=mod_RF,
         mod_RF_m_sd=mod_RF - mod_RF_sd,
         mod_RF_a_sd=mod_RF + mod_RF_sd,
         ) %>%
  mutate(obs=NULL,mod_conLUE=NULL,mod_Pmodel=NULL,
         mod_BESS=NULL,mod_RF=NULL) %>%   ##remove the variables do not use
  pivot_longer(c(obs_mean,mod_conLUE_mean,mod_Pmodel_mean,
                 mod_BESS_mean,mod_RF_mean),
               names_to = "Source", values_to = "gpp") 
df_gpp_all %>%
  ggplot(aes(doy, gpp, color = Source))+
  # geom_ribbon(aes(ymin = mod_conLUE_m_sd, ymax = mod_conLUE_a_sd), fill="orange",alpha = 0.5,color=NA)+
  # geom_ribbon(aes(ymin = mod_Pmodel_m_sd, ymax = mod_Pmodel_a_sd), fill="skyblue",alpha = 0.5,color=NA)+
  # geom_ribbon(aes(ymin = obs_m_sd, ymax = obs_a_sd),fill="green3", alpha = 0.3,color=NA)+
  geom_line()+
  # scale_color_manual(values = c("mod_conLUE_mean" = "orange",
  #                               "mod_Pmodel_mean"="steelblue2","obs_mean" = "black"),
  #                    labels = c("constant LUE model","P model","Obs.")) +
  labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "Day of year")+
  theme_classic()+
  scale_color_npg()

#-------------
#b.2nd part:for the sites that identify as the "GPP overestimated"
#-------------
df_merge_over<-df_merge[!is.na(df_merge$Over_days_length)&df_merge$Over_days_length>20,]
#first have a look at the performance for each site or all sites
#b1. for each site:
df_merge_over %>%
  group_by(sitename,doy)%>%
  summarise(PFT = classid,
            obs = mean(gpp_obs, na.rm = TRUE),
            obs_sd = sd(gpp_obs,na.rm = T),
            mod_conLUE = mean(gpp_conLUE,na.rm=T),
            mod_conLUE_sd = sd(gpp_conLUE,na.rm = T),
            mod_Pmodel = mean(gpp_mod_FULL,na.rm=T),
            mod_Pmodel_sd = sd(gpp_mod_FULL,na.rm = T),
            mod_BESS = mean(gpp_bess,na.rm=T),
            mod_BESS_sd = sd(gpp_bess,na.rm = T),
            # mod_VPM = mean(gpp_vpm,na.rm=T),
            # mod_VPM_sd = sd(gpp_vpm,na.rm = T),
            mod_RF = mean(gpp_rf,na.rm=T),  ##using RF as the data are daily
            mod_RF_sd = sd(gpp_rf,na.rm = T)
  ) %>%
  pivot_longer(c(obs,mod_conLUE,mod_Pmodel,mod_BESS,mod_RF),
               names_to = "Source", values_to = "gpp") %>%
  ggplot(aes(doy, gpp, color = Source))+
  geom_line()+
  # scale_color_manual(values = c("mod_conLUE" = "orange",
  #                               "mod_Pmodel"="steelblue2","obs" = "black"),
  #                    labels = c("constant LUE model","P model","Obs.")) +
  labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "Day of year")+
  geom_text(aes(x=200,y=1,label=PFT))+
  # annotate(geom = "text",x=180,y=1,label=sitename)
  facet_wrap(~sitename)+
  # theme_classic()+
  scale_color_npg()

#b2. for all the sites:
# remove one site(IT-lsp) for BESS model since it is failed to simulate:
df_merge_over[df_merge_over$sitename=="IT-Isp",]$gpp_bess<-NA
df_gpp_over<-df_merge_over %>%
  # filter(classid=="ENF")%>%
  group_by(doy)%>%
  summarise(obs = mean(gpp_obs, na.rm = TRUE),
            obs_sd = sd(gpp_obs,na.rm = T),
            mod_conLUE = mean(gpp_conLUE,na.rm=T),
            mod_conLUE_sd = sd(gpp_conLUE,na.rm = T),
            mod_Pmodel = mean(gpp_mod_FULL,na.rm=T),
            mod_Pmodel_sd = sd(gpp_mod_FULL,na.rm = T),
            mod_BESS = mean(gpp_bess,na.rm=T),
            mod_BESS_sd = sd(gpp_bess,na.rm = T),
            # mod_VPM = mean(gpp_vpm,na.rm=T),
            # mod_VPM_sd = sd(gpp_vpm,na.rm = T),
            mod_RF = mean(gpp_rf,na.rm=T),  ##using RF as the data are daily
            mod_RF_sd = sd(gpp_rf,na.rm = T))%>%
  mutate(obs_mean=obs,obs_m_sd=obs - obs_sd,obs_a_sd=obs + obs_sd,
         mod_conLUE_mean=mod_conLUE,
         mod_conLUE_m_sd=mod_conLUE - mod_conLUE_sd,
         mod_conLUE_a_sd=mod_conLUE + mod_conLUE_sd,
         mod_Pmodel_mean=mod_Pmodel,
         mod_Pmodel_m_sd=mod_Pmodel - mod_Pmodel_sd,
         mod_Pmodel_a_sd=mod_Pmodel + mod_Pmodel_sd,
         mod_BESS_mean=mod_BESS,
         mod_BESS_m_sd=mod_BESS - mod_BESS_sd,
         mod_BESS_a_sd=mod_BESS + mod_BESS_sd,
         # mod_VPM_mean=mod_VPM,
         # mod_VPM_m_sd=mod_VPM - mod_VPM_sd,
         # mod_VPM_a_sd=mod_VPM + mod_VPM_sd,
         mod_RF_mean=mod_RF,
         mod_RF_m_sd=mod_RF - mod_RF_sd,
         mod_RF_a_sd=mod_RF + mod_RF_sd,
  ) %>%
  mutate(obs=NULL,mod_conLUE=NULL,mod_Pmodel=NULL,
         mod_BESS=NULL,mod_RF=NULL) %>%   ##remove the variables do not use
  pivot_longer(c(obs_mean,mod_conLUE_mean,mod_Pmodel_mean,
                 mod_BESS_mean,mod_RF_mean),
               names_to = "Source", values_to = "gpp") 
df_gpp_over %>%
  ggplot(aes(doy, gpp, color = Source))+
  # geom_ribbon(aes(ymin = mod_conLUE_m_sd, ymax = mod_conLUE_a_sd), fill="orange",alpha = 0.5,color=NA)+
  # geom_ribbon(aes(ymin = mod_Pmodel_m_sd, ymax = mod_Pmodel_a_sd), fill="skyblue",alpha = 0.5,color=NA)+
  # geom_ribbon(aes(ymin = obs_m_sd, ymax = obs_a_sd),fill="green3", alpha = 0.3,color=NA)+
  geom_line(size=1.2)+
  # scale_color_manual(values = c("mod_conLUE_mean" = "orange",
  #                               "mod_Pmodel_mean"="steelblue2","obs_mean" = "black"),
  #                    labels = c("constant LUE model","P model","Obs.")) +
  labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
       x = "Day of year")+
  theme_classic()+
  scale_color_npg()

#-------------
#c.3nd part:for the sites that identify as the "GPP overestimated"-->for different PFTs:DBF,MF,ENF
#-------------
general_plot<-function(df_merge_over,PFT_name){
  # df_merge_over<-df_merge_over
  # PFT_name<-"DBF"
  
  df_gpp_over<-df_merge_over %>%
    filter(classid==PFT_name)%>%
    group_by(doy)%>%
    summarise(obs = mean(gpp_obs, na.rm = TRUE),
              obs_sd = sd(gpp_obs,na.rm = T),
              mod_conLUE = mean(gpp_conLUE,na.rm=T),
              mod_conLUE_sd = sd(gpp_conLUE,na.rm = T),
              mod_Pmodel = mean(gpp_mod_FULL,na.rm=T),
              mod_Pmodel_sd = sd(gpp_mod_FULL,na.rm = T),
              mod_BESS = mean(gpp_bess,na.rm=T),
              mod_BESS_sd = sd(gpp_bess,na.rm = T),
              # mod_VPM = mean(gpp_vpm,na.rm=T),
              # mod_VPM_sd = sd(gpp_vpm,na.rm = T),
              mod_RF = mean(gpp_rf,na.rm=T),  ##using RF as the data are daily
              mod_RF_sd = sd(gpp_rf,na.rm = T))%>%
    mutate(obs_mean=obs,obs_m_sd=obs - obs_sd,obs_a_sd=obs + obs_sd,
           mod_conLUE_mean=mod_conLUE,
           mod_conLUE_m_sd=mod_conLUE - mod_conLUE_sd,
           mod_conLUE_a_sd=mod_conLUE + mod_conLUE_sd,
           mod_Pmodel_mean=mod_Pmodel,
           mod_Pmodel_m_sd=mod_Pmodel - mod_Pmodel_sd,
           mod_Pmodel_a_sd=mod_Pmodel + mod_Pmodel_sd,
           mod_BESS_mean=mod_BESS,
           mod_BESS_m_sd=mod_BESS - mod_BESS_sd,
           mod_BESS_a_sd=mod_BESS + mod_BESS_sd,
           # mod_VPM_mean=mod_VPM,
           # mod_VPM_m_sd=mod_VPM - mod_VPM_sd,
           # mod_VPM_a_sd=mod_VPM + mod_VPM_sd,
           mod_RF_mean=mod_RF,
           mod_RF_m_sd=mod_RF - mod_RF_sd,
           mod_RF_a_sd=mod_RF + mod_RF_sd,
    ) %>%
    mutate(obs=NULL,mod_conLUE=NULL,mod_Pmodel=NULL,
           mod_BESS=NULL,mod_RF=NULL) %>%   ##remove the variables do not use
    pivot_longer(c(obs_mean,mod_conLUE_mean,mod_Pmodel_mean,
                   mod_BESS_mean,mod_RF_mean),
                 names_to = "Source", values_to = "gpp") 
  df_plot<-df_gpp_over %>%
    ggplot(aes(doy, gpp, color = Source))+
    # geom_ribbon(aes(ymin = mod_conLUE_m_sd, ymax = mod_conLUE_a_sd), fill="orange",alpha = 0.5,color=NA)+
    # geom_ribbon(aes(ymin = mod_Pmodel_m_sd, ymax = mod_Pmodel_a_sd), fill="skyblue",alpha = 0.5,color=NA)+
    # geom_ribbon(aes(ymin = obs_m_sd, ymax = obs_a_sd),fill="green3", alpha = 0.3,color=NA)+
    geom_line(size=1.2)+
    # scale_color_manual(values = c("mod_conLUE_mean" = "orange",
    #                               "mod_Pmodel_mean"="steelblue2","obs_mean" = "black"),
    #                    labels = c("constant LUE model","P model","Obs.")) +
    labs(y = expression( paste("GPP (g C m"^-2, " d"^-1, ")" ) ),
         x = "Day of year")+
    annotate(geom = "text",x=180,y=1,label=PFT_name,size=6)+
    theme_classic()+
    scale_color_npg()
  return(df_plot)
}

#For DBF
GPP_comp_DBF<-general_plot(df_merge_over,"DBF")
#For MF
GPP_comp_MF<-general_plot(df_merge_over,"MF")
#For ENF
GPP_comp_ENF<-general_plot(df_merge_over,"ENF")

  
