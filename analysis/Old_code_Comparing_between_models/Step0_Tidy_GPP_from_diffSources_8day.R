###################################
##Tidy 8-day GPP from different source(models): -->site scale for fluxnet 2015
#BESS, VPM, MODIS, and FLUXCOM
###################################
library(lubridate)
# library to read matlab data formats into R
library(R.matlab)
library(zoo)

GPP.path<-"D:/data/GPP_from_diffSources/"
daily.GPP.path<-"D:/data/photocold_project/GPP_from_diffSources/site_scale/"
save.path<-"D:/data/photocold_project/GPP_from_diffSources/site_scale/"
#--------------------------
#daily GPP-->8-day GPP:average the GPP data and assign the first of the 8 values 
#---------------
#(1) For BESS model
#---------------
# read in daily data
load(paste0(daily.GPP.path,"GPP_BESS_FLUX2015_daily.RDA"))
#width=8-->window size=8; by=8:nonoverlapping groups of 8; 
#align: align the average value to the 1 of 8 values; fill="NA"-->fill value between with NA
#however, needs to rollmean for each site
convert_8day_fun<-function(df,source_name){
  # df<-GPP_BESS_new
  # source_name<-"bess"
  
  Nvar<-length(df)
  new_names<-c(names(df)[-Nvar],"gpp_ori")
  names(df)<-new_names
  #
  df.proc<-df %>%
  group_by(sitename)%>%
  mutate(gpp_ori_8day=rollapply(gpp_ori,width=8,FUN=function(x){mean(x,na.rm=T)},by=8,align="left",fill="NA"))
  #add new names:
  gpp_names<-c(paste0("gpp_",source_name),paste0("gpp_",source_name,"_8day"))
  Nvars<-length(names(df.proc))
  new.names<-c(names(df.proc)[-c(c(Nvars-1):Nvars)],
               gpp_names)
  names(df.proc)<-new.names
  return(df.proc)
}

GPP_BESS_new<-convert_8day_fun(GPP_BESS_new,"bess")
#save the data
save(GPP_BESS_new,file = paste0(save.path,"GPP_BESS_FLUX2015_8day.RDA"))

#---------------
#(2) For VPM model
#---------------
#read the data: GPP unit: g C m-2 d-1
GPP_VPM_ori<-read.csv(paste0(GPP.path,"VPM/sitescale_fluxnet/gpp_vpm_combined_data_for_figure_allsites_v20.csv"))
#
GPP_VPM<-GPP_VPM_ori[,c("sid","date","VPM")]
names(GPP_VPM)<-c("sitename","date","gpp_vpm")
#
GPP_VPM$date<-as.Date(strptime(GPP_VPM$date,format = "%Y%j"))
GPP_VPM$doy<-yday(GPP_VPM$date)
GPP_VPM_new<-GPP_VPM[,c("sitename","date","doy","gpp_vpm")]
names(GPP_VPM_new)<-c("sitename","date","doy","gpp_vpm_8day")
#save the data
save(GPP_VPM_new,file = paste0(save.path,"GPP_VPM_FLUX2015_8day.RDA"))

#----------------------
#(3) For MTE model(methods for FLUXCOM)-->adopt Tramontana et al., 2016:
#paper: https://bg.copernicus.org/articles/13/4291/2016/bg-13-4291-2016.pdf
#and supplement: https://bg.copernicus.org/articles/13/4291/2016/bg-13-4291-2016-supplement.pdf
#----------------------
#here I adopt the random forest results since it provides the daily data:

# #read the data:
# GPP_MTE_ori<-read.csv(paste0(GPP.path,"GPP_MTE_Tramontana2016/sitescale_fluxnet/GPP_8Days_4Beni.csv"))
# #three different MTE methods and RF-->see details in Table S6.1 in Tramontana et al., 2016:
# GPP_MTE<-GPP_MTE_ori[,c("Site.code","StartYear","StartDoY","RF","MTE","MTE_Viterbo","MTE_M")]
# names(GPP_MTE)<-c("sitename","Year","doy","gpp_rf","gpp_MTE","gpp_MTEv","gpp_MTEm")
# date_ts<-paste0(GPP_MTE$Year,GPP_MTE$doy)
# date_ts<-as.Date(strptime(date_ts,format = "%Y%j"))
# #
# GPP_MTE_new<-cbind("sitename"=GPP_MTE$sitename,"date"=date_ts,GPP_MTE[,c("doy","gpp_rf","gpp_MTE","gpp_MTEv","gpp_MTEm")])
# #if value=-9999, set values =NA
# GPP_MTE_new[GPP_MTE_new==c(-9999)]<-NA
# #save the data
# save(GPP_MTE_new,file = paste0(save.path,"GPP_MTE_RF_FLUX2015_8day.RDA"))
##---------------
load(paste0(daily.GPP.path,"GPP_MTE_RF_FLUX2015_daily.RDA"))
#width=8-->window size=8; by=8:nonoverlapping groups of 8; 
#align: align the average value to the 1 of 8 values; fill="NA"-->fill value between with NA
GPP_MTE_new<-convert_8day_fun(GPP_MTE_new,"rf")
#save the data
save(GPP_MTE_new,file = paste0(save.path,"GPP_MTE_RF_FLUX2015_8day.RDA"))

#---------------
#(4) For obs and pmodel gpp
#---------------
# read in daily data
load(paste0(daily.GPP.path,"GPP_conLUE_FLUX2015_daily.RDA"))
GPP_Obs_new<-GPP_conLUE %>%
  select(sitename:gpp_obs)
GPP_Pmodel_new<-GPP_conLUE %>%
  select(c(sitename:APAR,gpp_mod_FULL))
##----------convert to 8-day data
#width=8-->window size=8; by=8:nonoverlapping groups of 8; 
#align: align the average value to the pos 1 of [1:8] values; fill="NA"-->fill NA between values
GPP_Obs_new<-convert_8day_fun(GPP_Obs_new,"obs")
GPP_Pmodel_new<-convert_8day_fun(GPP_Pmodel_new,"pmodel")
#
GPP_Obs_Pmodel_new<-cbind(GPP_Obs_new,GPP_Pmodel_new[,c("gpp_pmodel","gpp_pmodel_8day")])
#save the data
save(GPP_Obs_Pmodel_new,file = paste0(save.path,"GPP_Obs_Pmodel_FLUX2015_8day.RDA"))
