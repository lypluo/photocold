###################################
##Tidy Daily GPP from different source(models): -->site scale for fluxnet 2015
#BESS, VPM, MODIS, and FLUXCOM
###################################
library(lubridate)
# library to read matlab data formats into R
library(R.matlab)

GPP.path<-"D:/data/GPP_from_diffSources/"
save.path<-"D:/data/photocold_project/GPP_from_diffSources/site_scale/"
#---------------
#(1) For BESS model
#---------------
# read in data
GPP_BESS_ori <- readMat(paste0(GPP.path,"BESS/sitescale_fluxnet/BESSv2.GPP.Daily.mat"))
site.infos<-read.csv(paste0(GPP.path,"BESS/sitescale_fluxnet/FLUXNET2015v1-3.csv"))
# check out data structure
str(GPP_BESS_ori)
# print out the first few rows of the dataset
head(GPP_BESS_ori$data)
dim(GPP_BESS_ori$data)  #5110 166-->row: 2001-2014 and col: 166 sites

#-----add the date and site names-----
GPP_BESS<-as.data.frame(GPP_BESS_ori$data)
names(GPP_BESS)<-site.infos$Name
Date.ori<-seq.Date(as.Date("2001-01-01"),as.Date("2014-12-31"),by="day")
Date.ts<-data.frame("date"=Date.ori,"doy"=yday(Date.ori))
#remove the doy=366 since the BESS do not consider the extra one day GPP in the leap year.
Date.ts<-Date.ts[Date.ts$doy<366,]
#re-tidy the format of the data:
sites<-names(GPP_BESS)
GPP_BESS_new<-c()
for(i in 1:length(sites)){
  GPP_BESS_temp<-data.frame("sitename"=rep(sites[i],nrow(GPP_BESS)),Date.ts,"gpp_bess"=GPP_BESS[,i])
  GPP_BESS_new<-rbind(GPP_BESS_new,GPP_BESS_temp)
}
# GPP unit:g C m-2 d-1
save(GPP_BESS_new,file = paste0(save.path,"GPP_BESS_FLUX2015_daily.RDA"))

#---------------
#(2) For VPM model-->currently no daily GPP product
#---------------
# #read the data: GPP unit: g C m-2 d-1
# GPP_VPM_ori<-read.csv(paste0(GPP.path,"VPM/sitescale_fluxnet/gpp_vpm_combined_data_for_figure_allsites_v20.csv"))
# #
# GPP_VPM<-GPP_VPM_ori[,c("sid","date","VPM")]
# names(GPP_VPM)<-c("sitename","date","gpp_vpm")
# #
# GPP_VPM$date<-as.Date(strptime(GPP_VPM$date,format = "%Y%j"))
# GPP_VPM$doy<-yday(GPP_VPM$date)
# GPP_VPM_new<-GPP_VPM[,c("sitename","date","doy","gpp_vpm")]
# 
# #save the data
# save(GPP_VPM_new,file = paste0(save.path,"GPP_VPM_FLUX2015.RDA"))

#----------------------
#(3) For MTE model(methods for FLUXCOM)-->adopt Tramontana et al., 2016:
#https://bg.copernicus.org/articles/13/4291/2016/bg-13-4291-2016.pdf
#----------------------
#here I adopt the random forest results since it provides the daily data:
#read the data:
GPP_MTE_ori<-read.csv(paste0(GPP.path,"GPP_MTE_Tramontana2016/sitescale_fluxnet/GPP_Daily_4Beni.csv"))
#
GPP_MTE<-GPP_MTE_ori[,c("Site.code","StartYear","StartDoY","RFdaily")]
names(GPP_MTE)<-c("sitename","Year","doy","gpp_rf")
date_ts<-paste0(GPP_MTE$Year,GPP_MTE$doy)
date_ts<-as.Date(strptime(date_ts,format = "%Y%j"))
#
GPP_MTE_new<-cbind("sitename"=GPP_MTE$sitename,"date"=date_ts,GPP_MTE[,c("doy","gpp_rf")])
#if value=-9999, set values =NA
GPP_MTE_new$gpp_rf[GPP_MTE_new$gpp_rf==c(-9999)]<-NA
#save the data
save(GPP_MTE_new,file = paste0(save.path,"GPP_MTE_RF_FLUX2015_daily.RDA"))
