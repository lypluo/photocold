##------------------------
#Aim: tidy daily and 8-ady data for Beni:
##------------------------

#---------(1) daily data-------------
load.path<-"D:/data/photocold_project/Merge_Data/Comparing_between_models_calibrated_nonGreeenUp/"
###daily:
#---load the tidied gppdata----
load(paste0(load.path,"GPP_from_diffSource_daily.RDA"))
df_gpp<-df_merge;rm(df_merge)
#re-adjust the variables sequences:
df_gpp_sel<-df_gpp %>%
  select(sitename,date,doy,sos,peak,Over_days_length,
         gpp_obs,gpp_pmodel,gpp_bess,gpp_rf,classid,koeppen_code)

#--load the merged fluxes and climate data
#1) load the data from Koen(the meteos are complete in this datasets):
base.path<-"D:/Github/gpp_bias_correction/"
df_recent <- readRDS(paste0(base.path,"data/model_data.rds"))
df_recent_sel<-df_recent%>%
  select(sitename,date,temp,tmin,tmax,vpd)

#2) load the data from other part:
load.path<-"D:/data/photocold_project/Merge_Data/Using_sites_in_Fluxnet2015_constant_LUEmodel/"
load(paste0(load.path,"Merged_Flux_and_PhenoCam.RDA"))
#selected the desired variables and rename them:
df_meteos<-df_merge %>%
  select(sitename,date,doy,fapar_itpl,
         ppfd_fluxnet2015,prec_fluxnet2015,SW_IN_fluxnet2015)
names(df_meteos)<-c("sitename","date","doy",
      "fapar","ppfd","prec","SW_IN")
#calculate the PAR =fapar_itpl *ppdf_fluxnet2015
df_meteos$APAR<-df_meteos$fapar*df_meteos$ppfd
df_meteos$doy<-lubridate::yday(df_meteos$date)

#---merge the data:
df_final<-left_join(df_gpp_sel,df_recent_sel,by=c("sitename","date"))
#
df_final<-left_join(df_final,df_meteos,by=c("sitename","date","doy"))
#save the data:
save.path<-"D:/data/photocold_project/Merge_Data/Comparing_between_models_calibrated_nonGreeenUp/data_for_Beni/"
save(df_final,file=paste0(save.path,"df_daily.RDA"))

#---------(2) 8-day data-------------
load.path<-"D:/data/photocold_project/Merge_Data/Comparing_between_models_calibrated_nonGreeenUp/"
###8-day:
#---load the tidied gppdata----
load(paste0(load.path,"GPP_from_diffSource_8day.RDA"))
df_gpp<-df_merge;rm(df_merge)
#re-adjust the variables sequences:
df_gpp_sel<-df_gpp %>%
  select(sitename,date,doy,sos,peak,Over_days_length,
         gpp_obs_8day,gpp_pmodel_8day,gpp_bess_8day,
         gpp_rf_8day,gpp_vpm_8day,
         classid,koeppen_code)
df_final<-df_gpp_sel
#save the gpp data:
save.path<-"D:/data/photocold_project/Merge_Data/Comparing_between_models_calibrated_nonGreeenUp/data_for_Beni/"
save(df_final,file=paste0(save.path,"gpp_8day.RDA"))

#--load the merged fluxes and climate data
# #1) load the data from Koen(the meteos are complete in this datasets):
# base.path<-"D:/Github/gpp_bias_correction/"
# df_recent <- readRDS(paste0(base.path,"data/model_data.rds"))
# df_recent_sel<-df_recent%>%
#   select(sitename,date,temp,tmin,tmax,vpd)
#
# #2) load the data from other part:
# load.path<-"D:/data/photocold_project/Merge_Data/Using_sites_in_Fluxnet2015_constant_LUEmodel/"
# load(paste0(load.path,"Merged_Flux_and_PhenoCam.RDA"))
# #selected the desired variables and rename them:
# df_meteos<-df_merge %>%
#   select(sitename,date,doy,fapar_itpl,
#          ppfd_fluxnet2015,prec_fluxnet2015,SW_IN_fluxnet2015)
# names(df_meteos)<-c("sitename","date","doy",
#                     "fapar","ppfd","prec","SW_IN")
# #calculate the PAR =fapar_itpl *ppdf_fluxnet2015
# df_meteos$APAR<-df_meteos$fapar*df_meteos$ppfd
# df_meteos$doy<-lubridate::yday(df_meteos$date)
# #---merge df_recent and df_meteos----
# df_final<-left_join(df_recent_sel,df_meteos)
#
# #-------convert daily to 8-day
# #width=8-->window size=8; by=8:nonoverlapping groups of 8;
# #align: align the average value to the pos 1 of [1:8] values; fill="NA"-->fill NA between values
# library(zoo)
# library(tidyr)
# df.proc<-df_final %>%
#   group_by(sitename)%>%
#   mutate(doy=doy,
#          temp_8day=rollapply(temp,width=8,FUN=function(x){mean(x,na.rm=T)},by=8,align="left",fill="NA"),
#          tmin_8day=rollapply(tmin,width=8,FUN=function(x){mean(x,na.rm=T)},by=8,align="left",fill="NA"),
#          tmax_8day=rollapply(tmax,width=8,FUN=function(x){mean(x,na.rm=T)},by=8,align="left",fill="NA"),
#          vpd_8day=rollapply(vpd,width=8,FUN=function(x){mean(x,na.rm=T)},by=8,align="left",fill="NA"),
#          fapar_8day=rollapply(fapar,width=8,FUN=function(x){mean(x,na.rm=T)},by=8,align="left",fill="NA"),
#          ppfd_8day=rollapply(ppfd,width=8,FUN=function(x){mean(x,na.rm=T)},by=8,align="left",fill="NA"),
#          prec_8day=rollapply(prec,width=8,FUN=function(x){mean(x,na.rm=T)},by=8,align="left",fill="NA"),
#          SW_IN_8day=rollapply(SW_IN,width=8,FUN=function(x){mean(x,na.rm=T)},by=8,align="left",fill="NA"),
#          APAR_8day=rollapply(APAR,width=8,FUN=function(x){mean(x,na.rm=T)},by=8,align="left",fill="NA")
# )
# df.proc<-df.proc %>%
#   select(sitename,date,ends_with("_8day"))
#
# #---merge the data:
# df_final<-left_join(df_gpp_sel,df.proc,by=c("sitename","date"))



