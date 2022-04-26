##############################################################
#Aim: tidy the GPP from different sources(from P-model,BESS,VPM,MTE,consLUE...)
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
#only keep the first several variables:
df_sites_modis_era <- df_sites_modis_era %>% select(sitename:koeppen_code)
#
df_merge<-df_merge%>%
  left_join(
    df_sites_modis_era,
    by = "sitename"
  )
#re-name the variables:gpp_mod_FULL-->gpp_pmodel
df_merge<-df_merge %>%
  mutate(gpp_pmodel=gpp_mod_FULL,
         gpp_mod_FULL=NULL)
# check gpp values for each site:
df_merge %>%
  select(sitename,doy,Over_days_length,starts_with("gpp_")) %>%
  pivot_longer(c(gpp_obs,gpp_conLUE,gpp_bess,gpp_rf,gpp_pmodel),
               names_to="source",values_to="gpp") %>%
  filter(Over_days_length>20)%>%
  group_by(sitename) %>%
  ggplot(aes(x=doy,y=gpp,col=source))+
  geom_point()+
  facet_wrap(~sitename)
# check gpp values for all:
df_merge %>%
  select(sitename,doy,Over_days_length,starts_with("gpp_")) %>%
  pivot_longer(c(gpp_obs,gpp_conLUE,gpp_bess,gpp_rf,gpp_pmodel),
               names_to="source",values_to="gpp") %>%
  filter(Over_days_length>20)%>%
  group_by(doy,source) %>%
  summarise(gpp=mean(gpp,na.rm=T))%>%
  ggplot(aes(x=doy,y=gpp,col=source))+
  geom_point()

#--------------------------------
#(3)save the data
#-------------------------------
save.path<-"D:/data/photocold_project/Merge_Data/Comparing_between_models_calibrated_nonGreeenUp/"
save(df_merge,file = paste0(save.path,"GPP_from_diffSource_daily.RDA"))
