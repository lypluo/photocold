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
load(paste0(load.path,"GPP_BESS_FLUX2015_8day.RDA"))
# #VPM
load(paste0(load.path,"GPP_VPM_FLUX2015_8day.RDA")) 
#MTE
load(paste0(load.path,"GPP_MTE_RF_FLUX2015_8day.RDA"))
#Pmodel and Obs
load(paste0(load.path,"GPP_Obs_Pmodel_FLUX2015_8day.RDA"))

#-----------------
#(2)Merge different data sources
#----------------
df_merge<-left_join(GPP_Obs_Pmodel_new,GPP_BESS_new,by=c("sitename","date","doy"))
#
df_merge<-left_join(df_merge,GPP_VPM_new,by=c("sitename","date","doy"))
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

#--------------------------------
#(3)save the data
#-------------------------------
save.path<-"D:/data/photocold_project/Merge_Data/Comparing_between_models_calibrated_nonGreeenUp/"
save(df_merge,file = paste0(save.path,"GPP_from_diffSource_8day.RDA"))
